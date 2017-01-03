\name{TPEA}
\alias{TPEA}
\title{
Statistical test and calculate the significance
}
\description{
Comparing with the R_AUEC which the interested gene set extract from the background gene set randomly and the corresponding AUEC based on interested gene set you input. The last step is to calculate the significance.
}
\usage{
TPEA(DEGs, scores, n)
}
\arguments{
  \item{DEGs}{
Interested gene set such as differentailly expressed gene set.
}
  \item{scores}{
The "AUEC" based on the interested gene set of 87 pathways.
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
ViewLatestTime()
##If you want to use the latest information,please run "UPDATE()".
DEGs<-sample(100:10000,15);
DEG<-as.matrix(DEGs);
##Set the times of perturbation
number<-50;
##Calculate the observed statistic
scores<-AUEC(DEG);
##Significant computational
results<-TPEA(DEG,scores,number);
}
