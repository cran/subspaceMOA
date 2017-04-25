/**
 * [SubspaceARFFStream.java] for Subspace MOA
 * 
 * Subspace version of FileStream class
 * 
 * @author Yunsu Kim
 * Data Management and Data Exploration Group, RWTH Aachen University
 */

package moa.streams.clustering;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import moa.core.InputStreamProgressMonitor;
import moa.core.ObjectRepository;
import moa.core.SubspaceInstance;
import moa.options.FileOption;
import moa.options.FlagOption;
import moa.options.IntOption;
import moa.options.ListOption;
import moa.options.Option;
import moa.tasks.TaskMonitor;
import weka.core.Instance;
import weka.core.Instances;

public class SubspaceARFFStream extends SubspaceClusteringStream {

	private static final long serialVersionUID = 1L;

	private boolean debug = false;
    String defaultfile = "";

    
    /* Options */
    
	public FileOption arffFileOption = new FileOption("arffFile", 'f',
			"ARFF file to load.", defaultfile, "arff", false);

	public IntOption classIndexOption = new IntOption(
			"classIndex",
			'c',
			"Class index of data. 0 for none or -1 for last attribute in file.",
			-1, -1, Integer.MAX_VALUE);

    public FlagOption normalizeOption = 
    		new FlagOption("normalize", 'n', 
    				"Numerical data will be normalized to 0-1 " +
    				"for the visualization to work. The complete arff file needs to be read upfront.");

    public ListOption removeAttributesOption = new ListOption("removeAttributes", 'r',
            "Attributes to remove. Enter comma seperated list, " +
            "starting with 1 for first attribute.", 
            new IntOption("removeAttribute", ' ', "Attribute to remove.",-1),
            new Option[0], ',');	
	
    public FlagOption keepNonNumericalAttrOption = 
    		new FlagOption("keepNonNumericalAttr", 'K',
    		"Non-numerical attributes are being filtered by default " +
    		"(except the class attribute). " +
    		"Check to keep all attributes. This option is being " +
    		"overwritten by the manual attribute removal filter.");
   
    
    /* File variable */
	protected Reader fileReader;    
	protected InputStreamProgressMonitor fileProgressMonitor;
	
	/* Preprocessing */
	private Integer[] removeAttributes = null;				// Filtering: duplicates, invalid attributes
	private Instances filteredDataset = null;				// After filtering
	private ArrayList<Double[]> valuesMinMaxDiff = null;	// For normalization
	
	/* Instances */
	protected Instances instances;
	protected boolean hitEndOfFile;
	protected Instance lastInstanceRead;
	protected int numInstancesRead;

	
	public SubspaceARFFStream() {
		numAttsOption = null;
	}
	
	@Override
	public void prepareForUseImpl(TaskMonitor monitor, ObjectRepository repository) {
		restart();
	}

	public long estimatedRemainingInstances() {
		double progressFraction = this.fileProgressMonitor.getProgressFraction();
		if ((progressFraction > 0.0) && (this.numInstancesRead > 0)) {
			return (long) ((this.numInstancesRead / progressFraction) - this.numInstancesRead);
		}
		return -1;
	}

	public boolean hasMoreInstances() {
		return !this.hitEndOfFile;
	}
	

	/**
	 * Getting the next instance.
	 */
	@Override
	public SubspaceInstance nextInstance() {
		Instance instance = this.lastInstanceRead;
		this.hitEndOfFile = !readNextInstanceFromFile();
		
		/* Make the instance subspace-labeled */
		if (instance != null) {
			return toSubspaceInstance(instance);
		} else {
			return null;
		}
	}

	public boolean isRestartable() {
		return true;
	}

	
	/**
	 * Starts from here.
	 */
	public void restart() {
		try {
			if (fileReader != null) {	// If there was a file already open, close it.
				fileReader.close();
			}
			
			// File variables
			InputStream fileStream = new FileInputStream(arffFileOption.getFile());
			fileProgressMonitor = new InputStreamProgressMonitor(fileStream);
			fileReader = new BufferedReader(new InputStreamReader(fileProgressMonitor));
			
			// Extracting instances from the file
			instances = new Instances(fileReader, 1);
			if (classIndexOption.getValue() < 0) {
				instances.setClassIndex(instances.numAttributes() - 1);
			} else if (classIndexOption.getValue() > 0) {
				instances.setClassIndex(classIndexOption.getValue() - 1);
			}

			// Delete duplicates and invalid attributes numbers
			HashSet<Integer> attributesToBeRemoved =  new HashSet<Integer>(); 
			Option[] rawAttributeList = removeAttributesOption.getList();
			for (int i = 0; i < rawAttributeList.length; i++) {
				int attribute = ((IntOption) rawAttributeList[i]).getValue();
				if (1 <= attribute && attribute <= instances.numAttributes()) {
					attributesToBeRemoved.add(attribute - 1);
				} else {
					System.out.println("Found invalid attribute removal description: " +
							"Attribute option " + attribute
							+ " will be ignored. Filestream only has "
							+ instances.numAttributes() + " attributes.");
				}
			}
			
			// Remove all non-numeric attributes except the class attribute
			if (!keepNonNumericalAttrOption.isSet()) {
				for (int i = 0; i < instances.numAttributes(); i++) {
					if (!instances.attribute(i).isNumeric() && i != instances.classIndex()) {
						attributesToBeRemoved.add(i);
					}
				}
			}
			
			// Normalize if needed
			if (normalizeOption.isSet()) {
				valuesMinMaxDiff = readMinMaxDiffValues(attributesToBeRemoved);
			}
			
			// Attributes to be removed (in an array, sequentially accessible)
			removeAttributes = attributesToBeRemoved.toArray(new Integer[0]);
			Arrays.sort(removeAttributes);
			
			// New number of attributes (class attribute included)
			numAttsOption = new IntOption("numAtts", 'a',"", instances.numAttributes() - removeAttributes.length - 1);	// Without class label
			if (removeAttributes.length > 0) {		// We deleted this!
				System.out.println("Removing the following attributes:");
				for (int i = 0; i < removeAttributes.length; i++) {
					System.out.println((removeAttributes[i] + 1) + " "
							+ instances.attribute(removeAttributes[i]).name());
				}
			}
            
			// Create filtered dataset
			filteredDataset = new Instances(instances);
			for (int i = removeAttributes.length - 1; i >= 0 ; i--) {
				filteredDataset.deleteAttributeAt(removeAttributes[i]);
			}

			// Initialize: instance-reading variables
			this.numInstancesRead = 0;
			this.lastInstanceRead = null;
			this.hitEndOfFile = !readNextInstanceFromFile();
			
		} catch (IOException ioe) {
			throw new RuntimeException("ArffFileStream restart failed.", ioe);
		}
	}

	protected boolean readNextInstanceFromFile() {
		try {			
			if (this.instances.readInstance(this.fileReader)) {
				Instance rawInstance = this.instances.instance(0);
				
				// Remove dataset from instance so we can delete attributes
				rawInstance.setDataset(null);
				for (int i = removeAttributes.length - 1; i >= 0 ; i--) {
					rawInstance.deleteAttributeAt(removeAttributes[i]);	
				}
				rawInstance.setDataset(filteredDataset);

				// Normalize if needed
				if (normalizeOption.isSet() && valuesMinMaxDiff != null) {
					for (int i = 0; i < rawInstance.numAttributes() ; i++) {
						if (valuesMinMaxDiff.get(i)[2] != 1 &&		// Already normalized
							valuesMinMaxDiff.get(i)[2] != 0 &&		// Max. value is 0 (unable to be normalized)
							i != rawInstance.classIndex()) {		// Class label is not subject to be normalized
							double v = rawInstance.value(i);
							v = (v - valuesMinMaxDiff.get(i)[0]) / valuesMinMaxDiff.get(i)[2];
							rawInstance.setValue(i, v);
						}
					}
				}
				
				// Set next instance
				this.lastInstanceRead = rawInstance;
				this.instances.delete();	// Keep instances clean
				this.numInstancesRead++;
				return true;
			}
			
			// End of file
			if (this.fileReader != null) {
				this.fileReader.close();
				this.fileReader = null;
			}
			
			return false;
			
		} catch (IOException ioe) {
			throw new RuntimeException(
					"ArffFileStream failed to read instance from stream.", ioe);
		}
	}
	
	/**
	 * @param ignoredAttributes Attributes that will be ignored
	 * @return A list with min/max and diff=max-min values per attribute of the arff file 
	 */
	protected ArrayList<Double[]> readMinMaxDiffValues(HashSet<Integer> ignoredAttributes) {
		ArrayList<Double[]> valuesMinMaxDiff = null;
		
		if(ignoredAttributes == null)
			ignoredAttributes = new HashSet<Integer>();
		
		try {
			InputStream fileStream = new FileInputStream(arffFileOption.getFile());
			InputStreamProgressMonitor fileProgressMonitor = new InputStreamProgressMonitor(fileStream);
			Reader fileReader = new BufferedReader(new InputStreamReader(fileProgressMonitor));
			Instances instances = new Instances(fileReader, 1);

			valuesMinMaxDiff = new ArrayList<Double[]>();
			for (int i = 0; i < instances.numAttributes()-ignoredAttributes.size(); i++) {
				Double[] values =  {Double.POSITIVE_INFINITY,Double.NEGATIVE_INFINITY,0.0};
				valuesMinMaxDiff.add(values);
			}
			
			System.out.print("Reading arff file for normalization...");
			int counter = 0;
			while (instances.readInstance(fileReader)) {
				Instance instance = instances.instance(0);
				int a = 0;
				for (int i = 0; i < instances.numAttributes(); i++) {
					if(!ignoredAttributes.contains(i)){
						double value = instance.value(i);
						if(value < valuesMinMaxDiff.get(a)[0])
							valuesMinMaxDiff.get(a)[0] = value;
						if(value > valuesMinMaxDiff.get(a)[1])
							valuesMinMaxDiff.get(a)[1] = value;
						a++;
					}
				}
				instances.delete();

				//show some progress
				counter++;
				if(counter >= 10000){
					counter = 0;
					System.out.print(".");
				}
			}
			if (fileReader != null) {
				fileReader.close();
				fileReader = null;
			}
			System.out.println("done!");

			for (int i = 0; i < valuesMinMaxDiff.size(); i++) {
				valuesMinMaxDiff.get(i)[2]=valuesMinMaxDiff.get(i)[1]-valuesMinMaxDiff.get(i)[0];
			}

			return valuesMinMaxDiff;
		} catch (IOException ioe) {
			throw new RuntimeException(
					"ArffFileStream failed to read instance from stream.", ioe);
		}
	}	
	

	public void getDescription(StringBuilder sb, int indent) {

	}

	@Override
	public String getPurposeString() {
		return "Emit a data stream from a given file: \n" +
			   "*.arff: WEKA full-space data file\n" +
			   "HINT: Visualization only works correctly with numerical 0-1 normalized attributes!";
	}
	
	
	
	/** Additional **/
	
	protected SubspaceInstance toSubspaceInstance(Instance instance) {
		SubspaceInstance subspaceInstance = new SubspaceInstance(instance);
		
		double classValue = instance.classValue();		
		double[] classLabels = new double[instance.numAttributes() - 1];	// w/o class index
		for (int j = 0; j < classLabels.length; j++) {
			classLabels[j] = classValue;
		}
		subspaceInstance.setClassLabels(classLabels);
		subspaceInstance.setDataset(instance.dataset());
		
		return subspaceInstance;
	}
}
