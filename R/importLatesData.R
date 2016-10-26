importLatesData <-
function(){
path1<-paste(system.file(package="TPEA"),"/data/updateData/",sep="")
a<-list.files(path=path1,full.names=TRUE,pattern="")
num_node_gene_score<-lapply(a,function(x)read.table(x,header=F,sep="\t"))

path2<-paste(system.file(package="TPEA"),"/data/NodeGeneRelationship/",sep="")
b<-list.files(path=path2,full.names=TRUE,pattern="")
node_gene<-lapply(b,function(x)read.table(x,header=F,sep="\t"))
}
