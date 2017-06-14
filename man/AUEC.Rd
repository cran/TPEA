\name{AUEC}
\alias{AUEC}
\title{Calculate the area under the cumulative enrichment curve (AUEC) based on the interested gene set.}
\description{
The interested gene set may be the differentially expressed genes or any other gene set. The function calculate the AUEC based on the interested genes. AUEC is the area under the cumulative enrichment curve in a coordinate system. X-axis displays the nodes by the scores from maximum to minimum. Y-axis displays the cumulative enrichment curve.
}
\usage{
AUEC(DEGs)
}
\arguments{
  \item{DEGs}{
The interested genes you input and the format must be "Entrez ID". If not,translate the interested genes into Entrez ID.
}
}
\details{
The function only identifies Entrez ID of genes. The nodes are sorted by their AUEC in the pathway. If genes locates on the upstream or the nodes with high degree in a certain pathway, the AUEC of this pathway is high.
}
\value{The AUEC of 87 pathways based on the interested gene set.}
\author{
Wei Jiang
}
\examples{
##Randomly generated interested genes
DEGs<-sample(100:100000,15)
DEG<-as.matrix(DEGs);
## The function is used to calculate the observed statistic
area<-AUEC(DEG);
}