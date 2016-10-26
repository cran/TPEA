DownloadKGML <-
function(){
pkgEnv <- new.env(parent=emptyenv())
 if(!exists("pathway_names", pkgEnv)) {
   data("pathway_names", package="TPEA", envir=pkgEnv)
   da2<-pkgEnv[["pathway_names"]]
 }
path<-paste(system.file(package="TPEA"),"/data/",sep="")
setwd(path)
if(file.exists("DownloadKGMLfiles")==TRUE){
unlink("DownloadKGMLfiles", recursive=TRUE)
}
if(file.exists("network")==TRUE){
unlink("network", recursive=TRUE)
}
if(file.exists("updateData")==TRUE){
unlink("updateData", recursive=TRUE)
}
if(file.exists("NodeGeneRelationship")==TRUE){
unlink("NodeGeneRelationship", recursive=TRUE)
}
dir.create(paste(path,"DownloadKGMLfiles/",sep=""))
path1<-paste(system.file(package="TPEA"),"/data/DownloadKGMLfiles/",sep="")
setwd(path1)
pathway<-as.matrix(da2)
for(i in 1:nrow(pathway)){
website1<-c("http://www.kegg.jp/kegg-bin/download?entry=")
website2<-pathway[i,1]
website3<-c("&format=kgml")
website<-paste(website1,website2,website3,sep="")
download.file(website,destfile=pathway[i,1])
print(i)
}
}
