getSimpleGraph <-
function(graphList){
     pathwayListLength<-length(graphList)
     pathwayList<-graphList
	 if(pathwayListLength>0){
     for(i in 1:pathwayListLength){
         deleteEdges<-c()
		 EMultiple<-c()
         eCount<-ecount(pathwayList[[i]]) 
         if(eCount>0){
		 if(any(is.loop(pathwayList[[i]]))||any(is.multiple(pathwayList[[i]]))){
		   #index<-seq(0,eCount-1)
		   #new!
            index<-seq(1,eCount)
		   deleteEdges<-c(deleteEdges,index[is.loop(pathwayList[[i]])])

		   EMultiple<-index[count.multiple(pathwayList[[i]])>1]
           EMultipleLength<-length(EMultiple)

           if(EMultipleLength>0){
               mulEdges<-get.edges(pathwayList[[i]],EMultiple)
               a<-unique(mulEdges)
               mid<-list()
               xLength<-length(a[,1])
			   a_string<-apply(a,1,function(x) paste(x,collapse=";"))
			   mulEdges_string<-apply(mulEdges,1,function(x) paste(x,collapse=";"))
               for(j in 1:xLength){
				  matched_mulEdges_string_index<-match(mulEdges_string,a_string[j])
                  mid[[j]]<-EMultiple[!is.na(matched_mulEdges_string_index)]
                       for(n in 2:length(mid[[j]])){
                          deleteEdges<-c(deleteEdges,mid[[j]][n])
                       }					  
               }####for(j in 1:xLength)
			   #print(Sys.time())
               edgeAttr<-list.edge.attributes(pathwayList[[i]])

			 edgeAttrLength<-length(edgeAttr)
             if(edgeAttrLength>0){
             for(k in 1:edgeAttrLength){   
                 edgeAtt.k<-get.edge.attribute(pathwayList[[i]],edgeAttr[k])	
                 str_k<-c()		
                 str_k_index<-c()				 
                 for(j in 1:xLength){
                       str<-edgeAtt.k[mid[[j]][1]+1]
                       for(m in 2:length(mid[[j]])){
                          str<-paste(str,edgeAtt.k[mid[[j]][m]+1],sep=";")
                       }
					   str_k<-c(str_k,str)
					   str_k_index<-c(str_k_index,mid[[j]][1])
                       #pathwayList[[i]]<-set.edge.attribute(pathwayList[[i]],edgeAttr[k],mid[[j]][1],str)                    
                }####for(j in 1:xLength)
				
				pathwayList[[i]]<-set.edge.attribute(pathwayList[[i]],edgeAttr[k],str_k_index,str_k)
            }###for(k in 1:edgeAttrLength)
            }###if(edgeAttrLength>0) 
           }###if(EMultipleLength>0)
		}####if(any(is.loop
        }####if(eCount+1>0)
        pathwayList[[i]]<-delete.edges(pathwayList[[i]],deleteEdges)
     }####for(i in 1:pathwayListLength)
	 }
     return(pathwayList)
}
