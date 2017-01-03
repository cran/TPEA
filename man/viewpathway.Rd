\name{viewpathway}
\alias{viewpathway}
\title{
The visualization of interested pathway based on the genes you input, such as differentially expressed genes.
}
\description{
Input the number of the interested pathway in KEGG Database and genes you interested in, such as differentially expressed genes.
}
\usage{
viewpathway(pathwayID, DEGs)
}
\arguments{
  \item{pathwayID}{
The number of interested pathway ID in KEGG Database, such as "hsa05210".
}
  \item{DEGs}{
The genes you interested in, such as differentially expressed genes.
}
}
\details{
The "DEGs" must be Entrez ID. If not, please translate them into Entrez ID.
}
\value{
The interface link to KEGG Database to visualize the pathway you input.
}
\author{
Wei Jiang

}
\examples{
DEGs<-c(836,842,5594,595);
DEG<-as.data.frame(DEGs);
pathwayID<-"hsa05210";
viewpathway(pathwayID,DEG);
}
