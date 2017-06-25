\name{TPEA}
\alias{TPEA}
\title{
Statistical test and calculate the significance
}
\description{
Comparing with the AUEC_R which the interested gene set extract from the background gene set randomly and the corresponding AUEC based on interested gene set you input. The last step is to calculate the significance.
}
\usage{
TPEA(DEGs, scores, n, FDR_method)
}
\arguments{
  \item{DEGs}{
Interested gene set such as differentailly expressed gene set.
}
  \item{scores}{
The "AUEC" based on the interested gene set of 109 pathways.
}
  \item{n}{
Randomly number,e.g. 1000, 5000.
}
\item{FDR_method}{
The methods of calculating FDR value,e.g. "fdr","BH","BY" ,"bonferroni" and etc..
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
DEGs<-sample(100:10000,10);
DEG<-as.matrix(DEGs);
##Set the times of perturbation
number<-50;
##Calculate the observed statistic
scores<-AUEC(DEG);
##Significant computational
FDR_method<-"fdr";
results<-TPEA(DEG,scores,number,FDR_method);
}
