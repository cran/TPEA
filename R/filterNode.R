filterNode <-
function(graphList,nodeType=c("map")){
       graphListLength<-length(graphList)
	   if(graphListLength>0){
       for(i in 1:graphListLength){
           vCount<-vcount(graphList[[i]])
           deleteId<-c()
		   if(vCount>0){
           for(j in 1:vCount){
               #if(get.vertex.attribute(graphList[[i]],"type",j-1) %in% nodeType){
			   if(get.vertex.attribute(graphList[[i]],"type",j) %in% nodeType){
                   deleteId<-c(deleteId,j)
               }
           }
           graphList[[i]]<-delete.vertices(graphList[[i]],deleteId)		   
		   }
       }
	   }
       return(graphList)
}
