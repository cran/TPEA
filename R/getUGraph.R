getUGraph <-
function(graphList,simpleGraph=TRUE){
    graphListLength<-length(graphList)
	if(graphListLength>0){
         for(i in 1:graphListLength){
              graphList[[i]]<-as.undirected(graphList[[i]],mode = "each")
        }	
	}
	if(simpleGraph==TRUE)
	  graphList<-getSimpleGraph(graphList)
    return(graphList)
}
