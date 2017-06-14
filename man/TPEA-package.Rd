\name{TPEA-package}
\alias{TPEA-package}
\docType{package}
\title{
TPEA: A Novel Pathway enrichment analysis approach based on topological structure and updated annotation of pathway
}
\description{
This package descirbed A Novel Pathway enrichment analysis approach based on topological structure and updated annotation of pathway which integrated the topological property of the pathway and the global position of nodes in pathways.Additionally,it also provided the update functions which could obtain the latest pathway information from KEGG database and users can use the latest information to do the pathway enrichment analysis.
}
\details{
The function AUEC is to calculate the area under the cumulative enrichment curve.
The function TPEA is to measure the significance of pathways.
The function UPDATE is to online download the latest KEGG pathway information.
The viewpathway function is to visualize the pathway in the result based on the genes you input, 
such as differentially expressed genes. Several other functions are the update related functions,
including ViewUpdateTime,UpdateKGML,PathNetwork,NodeGeneData,NodeGene,importUpdateData. 
The functions involved in relationship between nodes and genes were provided by Chunquan Li.
If you want to use the latest information of KEGG database,please run "UPDATE()" functions first,
and then run the pathway enrichment analysis functions AUEC and TPEA.
}
\author{
Wei Jiang
}
