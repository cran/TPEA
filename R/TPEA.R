TPEA <-
function(DEGs,scores,n,FDR_method){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("all_genes", pkgEnv)) {
  data("all_genes", package="TPEA", envir=pkgEnv)
  da1<-pkgEnv[["all_genes"]]
}
if(!exists("pathway_names", pkgEnv)) {
  data("pathway_names", package="TPEA", envir=pkgEnv)
  da2<-pkgEnv[["pathway_names"]]
}
all_genes<-da1;
pathway_names<-da2;
number<-n;
all_rand_area<-matrix(0,87,1);
for(i in 1:number){
DEG1<-intersect(DEGs[,1],all_genes[,1]);
DEG1<-as.data.frame(DEG1);
num<-sample(1:nrow(all_genes),size=nrow(DEG1));
rand_genes<-all_genes[num,1];
rand_genes<-as.data.frame(rand_genes);
rand_area<-AUEC(rand_genes);
rand_area[,2]<-as.matrix(rand_area[,2]);
all_rand_area<-cbind(all_rand_area,rand_area[,2]);
print(i);
}
all_rand_area[,1]<-scores[,2];
p_value<-data.frame();
N_AUEC<-data.frame();
for(j in 1:87){
p<-length(which(all_rand_area[j,-1]>=all_rand_area[j,1]))/number;
p_value<-rbind(p_value,p);
nor_area<-(all_rand_area[j,1]-mean(all_rand_area[j,-1]))/sd(all_rand_area[j,-1]);
N_AUEC<-rbind(N_AUEC,nor_area);
}

result1<-cbind(pathway_names,scores[,1],p_value,N_AUEC);

p_v<-as.matrix(p_value);
FDR<-p.adjust(p_v,method=FDR_method,n=87);
FDR<-as.matrix(FDR);
colnames(FDR)<-c("FDR");
result1<-as.matrix(result1);
result2<-cbind(result1,FDR);
result2<-result2[order(result2[,4]),];
return(result2);
}
