NodeGeneData <-
function(){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("num_node_gene_score", pkgEnv)) {
  data("num_node_gene_score", package="TPEA", envir=pkgEnv)
  da2<-pkgEnv[["num_node_gene_score"]]
}
path1<-paste(system.file(package="TPEA"),"/data/DownloadKGMLfiles/",sep="")
a<-list.files(path1)
n<-length(a)
path2<-paste(system.file(package="TPEA"),"/data/",sep="")
dir.create(paste(path2,"updateData/",sep=""))
path3<-paste(system.file(package="TPEA"),"/data/updateData/",sep="")
setwd(path3)
file1<-da2
for(i in 1:n){
p<-getPathway(path1,c(a[i]))
g<-getNonMetabolicGraph(p,ambiguousEdgeDirection="single")
gf<-filterNode(g,nodeType=c("map","ortholog","compound"))
  g1g<-getUGraph(gf,simpleGraph=TRUE)
  g1<-mapNode(g1g)
  gene_id<-V(g1[[1]])$names###(Entrez gene id)
  pathway_id<-V(g1[[1]])$id###pathway_id
  gene_id<-as.matrix(gene_id)
  pathway_id<-as.matrix(pathway_id)
  node_genes<-cbind(pathway_id,gene_id)
file2<-as.data.frame(file1[i])
file4<-data.frame()
for(j in 1:nrow(node_genes)){
loc<-which(file2[,2]==node_genes[j,1])
sp<-strsplit(node_genes[j,2]," ")
file3<-matrix(0,length(sp[[1]]),4)
file3[,1]<-file2[loc[1],1]
file3[,2]<-node_genes[j,1]
file3[,3]<-as.matrix(as.data.frame(sp[[1]]))
file3[,4]<-file2[loc[1],4]
file4<-rbind(file4,file3)
file4<-file4[apply(file4,1,function(x)!any(is.na(x))),,drop=F]
}
write.table(file4,a[i],col.names=F,row.names=F,sep="\t",quote=F)
}
}
