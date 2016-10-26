getGeneFromKGene <-
function(keggGeneList){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("keggGene2gene", pkgEnv)) {
  data("keggGene2gene", package="TPEA", envir=pkgEnv)
  da1<-pkgEnv[["keggGene2gene"]]
}
	  keggGeneList<-as.character(keggGeneList)
      #if(!exists("k2ri")) initializeK2ri()
      keggGene2gene<-da1
      geneList<-unique(as.character(sapply(strsplit(as.character(keggGene2gene[as.character(keggGene2gene[,1]) %in% keggGeneList,2]),":"),function(x) return (x[2]))))
      return(geneList)
}
