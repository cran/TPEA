\name{AUCSC}
\alias{AUCSC}
\title{Calculate the area under the cumulative score of node curve (AUCSC) based on the interested gene set.}
\description{
The interested gene set may be the differentially expressed genes or any other gene set. The function calculate the AUCSC based on the interested genes. AUCSC is the area under the cumulative score of node curve in a coordinate system. X-axis displays the nodes by the scores from maximum to minimum. Y-axis displays the cumulative score of each node.
}
\usage{
AUCSC(DEGs)
}
\arguments{
  \item{DEGs}{
The interested genes you input and the format must be "Entrez ID". If not,translate the interested genes into Entrez ID.
}
}
\details{
The function only identifies Entrez ID of genes. The nodes are sorted by their AUCSC in the pathway. If genes locates on the upstream or the nodes with high degree in a certain pathway, the AUCSC of this pathway is high.
}
\value{The AUCSC of 87 pathways based on the interested gene set.}
\author{
Wei Jiang
}
\examples{
##Randomly generated interested genes
DEGs<-sample(100:100000,15)
genes<-as.matrix(DEGs);
## The function is used to calculate the observed statistic
area<-AUCSC(genes);
}