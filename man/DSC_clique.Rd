% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macro.R
\name{DSC_clique}
\alias{DSC_clique}
\title{CLIQUE algorithm for use with DSC_ThreeStage}
\usage{
DSC_clique(xi = 10, tau = 0.2)
}
\arguments{
\item{xi}{the grid size used. E.g. a value of 10 means that the dataspace is
divided into 10 regions along each dimension.}

\item{tau}{the density threshold used to determine whether a hypercube is
dense}
}
\description{
An implementation of the CLIQUE algorithm that can be used with
\link{DSC_ThreeStage}. For more details on this algorithm, consult
\link[subspace]{CLIQUE}
}
\examples{
dsc <- DSC_ThreeStage(macro=DSC_clique(),micro=DSC_subspaceCluStream())
dsd <- DSD_RandomRBFSubspaceGeneratorEvents()
update(dsc,dsd,1000)
}
