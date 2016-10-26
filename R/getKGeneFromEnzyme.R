getKGeneFromEnzyme <-
function(enzymeList,ignoreAmbiguousEnzyme=TRUE){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("gene2ec", pkgEnv)) {
  data("gene2ec", package="TPEA", envir=pkgEnv)
  da1<-pkgEnv[["gene2ec"]]
}
	  enzymeList<-as.character(enzymeList)
	  if(ignoreAmbiguousEnzyme==TRUE){
	     enzymeList<-grep("-",enzymeList,value=TRUE,invert=TRUE)
	  }
      #if(!exists("k2ri")) initializeK2ri()
	  gene2ec<-da1
      keggGeneList<-unique(as.character(gene2ec[as.character(gene2ec[,2]) %in% enzymeList,1]))
      return(keggGeneList)
}
