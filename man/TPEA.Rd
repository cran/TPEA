\name{TPEA}
\alias{TPEA}
\title{
Statistical test and calculate the significance
}
\description{
Comparing with the R_AUCSC which the interested gene set extract from the background gene set randomly and the corresponding AUCSC based on interested gene set you input. The last step is to calculate the significance.
}
\usage{
TPEA(DEGs, scores, n)
}
\arguments{
  \item{DEGs}{
Interested gene set such as differentailly expressed gene set.
}
  \item{scores}{
The "AUCSC" based on the interested gene set of 87 pathways.
}
  \item{n}{
Randomly number,e.g. 1000, 5000.
}
}
\details{
To calculate the significance of the result, you can set "n" as "1000" or any other number you want. 
}
\value{
The ultimately result of this topology-based enrichment analysis method.
}
\author{
Wei Jiang

}
\examples{
##Randomly generated interested gene set
DEGs<-sample(100:10000,15);
DEGs<-as.matrix(DEGs);
##Set the times of perturbation
number<-50;
##Calculate the observed statistic
scores<-AUCSC(DEGs);
##Significant computational
results<-TPEA(DEGs,scores,number);
}
