#'@title Evaluate Subspace Clusterings
#'
#'@param dsc The clusterer whose current clustering should be evaluated.
#'@param dsd The stream from which the data points for evaluation should be drawn.
#'@param n How many points to evaluate over
#'@param measures A vector of evaluation measures to use. By default, all supported measures are used.
#'@param alsoTrainOn This will train the clusterer on the data points before running the evaluation.
#'@description
#'
#'This function evaluates Subspace Clusterings based on data points from a stream.
#'
#'
#'@import stream
#'@import rJava
#'@export
evaluate_subspace <- function(dsc,
                             dsd,
                             n=1000,
                             measures=c("clustering error","cmm subspace","entropy subspace","f1 subspace", "purity","rand statistic"),
                             alsoTrainOn=F) {
  evaluator <- rJava::.jnew("moa/r_interface/Evaluator")
  if(rJava::is.jnull(evaluator)) {print("evaluator not found")}
  result <- rJava::.jcall(evaluator,
                          returnSig="Lmoa/r_interface/RCompatibleEvaluationResult;",
                          "evaluate",
                          dsc$javaObj,dsd$javaObj,as.integer(n),measures,alsoTrainOn)
  result_names <- rJava::.jcall(result,
                                returnSig="[Ljava/lang/String;",
                                method="getNames",
                                evalArray=T,
                                evalString=T,
                                simplify=T)
  result_values <- rJava::.jcall(result,
                                 returnSig="[D",
                                 method="getValues",
                                 evalArray = T)
  result_points <- rJava::.jcall(result,returnSig="[[D",method="getPoints",evalArray=T,simplify=T)
  result_points_df <- data.frame(result_points)
  names(result_points_df)[ncol(result_points_df)] <- "class"
  return(list(names=result_names,values=result_values,points=result_points_df))
}

all_eval_measures <- function() {
  c("clustering error","cmm subspace","entropy subspace","f1 subspace", "purity","rand statistic")
}