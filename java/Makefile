JAVAC=javac
CLASSPATH=".:../inst/java/*"

all: SubspaceClustererBuilder.class DataStreamBuilder.class DataStreamAccessor.class ClusteringAccessor.class MacroClustererBuilder.class MicroClustererBuilder.class
	jar -cf ../inst/java/added.jar *.class
%.class: %.java
	$(JAVAC) -cp $(CLASSPATH) $<
run:
	java -cp $(CLASSPATH) DataStreamBuilder
clean:
	rm *.class
