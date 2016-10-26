NodeGene <-
function(){
path1<-paste(system.file(package="TPEA"),"/data/updateData/",sep="")
a<-list.files(path1)
n<-length(a)
path2<-paste(system.file(package="TPEA"),"/data/",sep="")
dir.create(paste(path2,"NodeGeneRelationship/",sep=""))
path3<-paste(system.file(package="TPEA"),"/data/NodeGeneRelationship/",sep="")
for(i in 1:n){
setwd(path1)
file1<-read.table(a[i],header=F,sep="\t")
file2<-file1[,c(2,3)]
setwd(path3)
write.table(file2,a[i],col.names=F,row.names=F,sep="\t",quote=F)
}
}
