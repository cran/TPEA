AUEC <-
function(DEGs){
pkgEnv <- new.env(parent=emptyenv())
if(!exists("all_genes", pkgEnv)) {
  data("all_genes", package="TPEA", envir=pkgEnv)
  da1<-pkgEnv[["all_genes"]]
}
if(!exists("num_node_gene_score", pkgEnv)) {
  data("num_node_gene_score", package="TPEA", envir=pkgEnv)
  da2<-pkgEnv[["num_node_gene_score"]]
}
if(!exists("node_gene", pkgEnv)) {
  data("node_gene", package="TPEA", envir=pkgEnv)
  da3<-pkgEnv[["node_gene"]]
}
all_genes<-da1;
num_node_gene_score<-da2;
node_gene<-da3;
true_area<-data.frame();
count<-data.frame();
DEG<-intersect(DEGs[,1],all_genes[,1]);
DEG<-as.data.frame(DEG);
for(i in 1:87){
    node_gene_score <- num_node_gene_score[i];
    node_gene_score <- as.data.frame(node_gene_score);
    step1 <- node_gene_score[which(node_gene_score[, 3]%in%DEG[, 1]), c(1,3,4)];
    step_count<-node_gene[i];
	step_count<-as.data.frame(step_count);
	gene_num<-length(unique(intersect(step_count[,2],DEG[,1])));
    count<-rbind(count,gene_num);
    if (nrow(step1) == 0) {
        ob_area <- 0;
    }
    else {
        final <- unique(node_gene_score[, c(1, 4)]);
        final <- final[order(final[, 1]), ];
        index <- as.matrix(table(step1[, 1]));
        position <- as.numeric(rownames(index));
        loc<-which(final[,1]%in%position);
        final[-loc, 2] <- 0;
        final[loc, 2] <- final[loc, 2] * index;
        ob_range <- cumsum(final[, 2]);
        ob_x <- 1:nrow(final);
        ob_area <- auc(ob_x, ob_range, type = c("linear"));
    }
  true_area<-rbind(true_area,ob_area);
}
 area<-true_area/nrow(DEG);
 result<-cbind(count,area);
 colnames(result)<-c("Count","area");
 return(result);
}
