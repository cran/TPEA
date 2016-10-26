\name{TPEA-package}
\alias{TPEA-package}
\docType{package}
\title{
TPEA: A novel topology-based pathway enrichment analysis approach
}
\description{
This package descirbed a novel topology-based pathway enrichment analysis approach which integrated the topological property of the pathway and the global position of nodes in pathways.Additionally,it also provided the update functions which could obtain the latest pathway information from KEGG database and users can use the latest information to do the pathway enrichment analysis.
}
\details{
The first function is to calculate AUCSC (the area under the cumulative score of node curve) 
based on interested gene set and the second function is to measure the significance.
The Third function is to visualize the pathway in the result based on the genes you input, 
such as differentially expressed genes. Several functions are the update functions,
including ViewUpdateTime,UpdateKGML,PathNetwork,NodeGeneData,NodeGene,importUpdateData. 
The functions involved in relationship between nodes and genes were provided by Chunquan Li.
If you want to use the latest information of KEGG database,please run "UPDATE()" functions first,
and then run the pathway enrichment analysis functions (AUCSC,TPEA).
}
\author{
Wei Jiang
}
