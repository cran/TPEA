getUnknowReaction <-
function(){
     reactionlist<-list("unknow","unknow","unknow","unknow","unknow")
     names(reactionlist)<-c("id","name","type","substrate","product")
     return(reactionlist)
}
