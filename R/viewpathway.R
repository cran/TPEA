viewpathway <-
function(pathwayID,DEGs){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("node_gene", pkgEnv)) {
  data("node_gene", package="TPEA", envir=pkgEnv)
  da1<-pkgEnv[["node_gene"]]
}
if(!exists("pathway_names", pkgEnv)) {
  data("pathway_names", package="TPEA", envir=pkgEnv)
  da2<-pkgEnv[["pathway_names"]]
}
node_gene<-da1;
pathway_names<-da2;
ID<-node_gene[which(pathway_names[,1]%in%pathwayID)];
ID<-as.data.frame(ID);
DE_pathway_genes<-intersect(ID[,2],DEGs[,1]);
DE_pathway_genes<-as.data.frame(DE_pathway_genes);
web1<-"http://www.kegg.jp/kegg-bin/show_pathway?";
bgcolor<-"/default%3dyellow";
fgcolor<-paste(apply(DE_pathway_genes,1,function(x)paste("/",x,"%09,black",sep="")),collapse="");
browseURL(paste(web1,pathwayID,bgcolor,fgcolor,"/",sep=""));
}
