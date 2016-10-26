PathNetwork <-
function(){
##library(iSubpathwayMiner)
path1<-paste(system.file(package="TPEA"),"/data/DownloadKGMLfiles/",sep="")
a<-list.files(path1)
n<-length(a)
path2<-paste(system.file(package="TPEA"),"/data/",sep="")
dir.create(paste(path2,"network/",sep=""))
path3<-paste(system.file(package="TPEA"),"/data/network/",sep="")
setwd(path3)
for(i in 1:n){
p<-getPathway(path1,c(a[i]))
g<-getNonMetabolicGraph(p,ambiguousEdgeDirection="single")
gf<-filterNode(g,nodeType=c("map","ortholog","compound"))
gs<-simplifyGraph(gf,nodeType="geneProduct")
write.graph(gs[[1]],a[i],"ncol")
}
}
