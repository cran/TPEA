getKGeneFromKO <-
function(KOList){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("gene2ko", pkgEnv)) {
  data("gene2ko", package="TPEA", envir=pkgEnv)
  da1<-pkgEnv[["gene2ko"]]
}
	  KOList<-as.character(KOList)
      #if(!exists("k2ri")) initializeK2ri()
	  gene2ko<-da1
      keggGeneList<-unique(as.character(gene2ko[as.character(gene2ko[,2]) %in% KOList,1]))
      return(keggGeneList)
}
