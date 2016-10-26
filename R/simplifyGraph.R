simplifyGraph <-
function(graphList,nodeType="geneProduct",directEdge=TRUE,verbose=FALSE){
     if(nodeType!="geneProduct"&&nodeType!="compound") stop("nodeType must be compound or geneProduct.")
     #if(!is.igraph(graphList)) stop("graphList should is a igraph class.")
     pathwayList<-graphList
     pathwayListLength<-length(pathwayList)
     graphList<-list()
     if(pathwayListLength>0){
    for(i in 1:pathwayListLength){
         if(class(pathwayList[[i]])!="igraph") stop(paste("graph ",i," must belong to a igraph class in graphList",sep=""))
         #if(is.directed(pathwayList[[i]])==FALSE) stop(paste("graph ",i," must be directed in graphList",sep=""))
   	     if(verbose==TRUE)
           print(paste("deal with the graph ",i," in ",pathwayListLength, " graphs",sep="")) 
		 directed<-is.directed(pathwayList[[i]])
	     #Vcount<-vcount(pathwayList[[i]]) #new!
	     #if(Vcount>0){
		 enzymeType<-""   
	     if(get.graph.attribute(pathwayList[[i]],"org")=="ec"){
	         enzymeType<-"enzyme"
	     }else if(get.graph.attribute(pathwayList[[i]],"org")=="ko"){
	         enzymeType<-"ortholog"
	     }else{
	         enzymeType<-"gene"
	     }
		 temp_nodeType<-""
		 if(nodeType=="geneProduct"){
		     temp_nodeType<-enzymeType
		 }else{
		     temp_nodeType<-"compound"
		 }
         vCount<-vcount(pathwayList[[i]])-1
		 #vCount<-vcount(pathwayList[[i]])
         enzyme_index<-c()#

         ID<-c();id<-c();names<-c();type<-c();reaction<-c();link<-c()
         graphics_name<-c();graphics_fgcolor<-c();graphics_bgcolor<-c()              
         graphics_type<-c();graphics_x<-c();graphics_y<-c()
         graphics_width<-c();graphics_height<-c();graphics_coords<-c()
        if(vCount>0){
        for(j in 0:vCount){
        #for(j in 1:vCount){		
            if(get.vertex.attribute(pathwayList[[i]],"type",j+1)==temp_nodeType){
                 enzyme_index<-c(enzyme_index,j+1)
                 id<-c(id,get.vertex.attribute(pathwayList[[i]],"id",j+1))
                 names<-c(names,get.vertex.attribute(pathwayList[[i]],"names",j+1))
                 type<-c(type,get.vertex.attribute(pathwayList[[i]],"type",j+1+1))
                 reaction<-c(reaction,get.vertex.attribute(pathwayList[[i]],"reaction",j+1))
                 link<-c(link,get.vertex.attribute(pathwayList[[i]],"link",j+1))
                 graphics_name<-c(graphics_name,get.vertex.attribute(pathwayList[[i]],"graphics_name",j+1))
                 graphics_fgcolor<-c(graphics_fgcolor,get.vertex.attribute(pathwayList[[i]],"graphics_fgcolor",j+1)) 
                 graphics_bgcolor<-c(graphics_bgcolor,get.vertex.attribute(pathwayList[[i]],"graphics_bgcolor",j+1))
                 graphics_type<-c(graphics_type,get.vertex.attribute(pathwayList[[i]],"graphics_type",j+1))
                 graphics_x<-c(graphics_x,get.vertex.attribute(pathwayList[[i]],"graphics_x",j+1))
                 graphics_y<-c(graphics_y,get.vertex.attribute(pathwayList[[i]],"graphics_y",j+1)) 
                 graphics_width<-c(graphics_width,get.vertex.attribute(pathwayList[[i]],"graphics_width",j+1)) 
                 graphics_height<-c(graphics_height,get.vertex.attribute(pathwayList[[i]],"graphics_height",j+1)) 
                 graphics_coords<-c(graphics_coords,get.vertex.attribute(pathwayList[[i]],"graphics_coords",j+1)) 
            }
          ID<-id
        }##for(j in 1:vCount)
        }
        ###
        start<-c();terminate<-c();middle<-list()
        eCount<-ecount(pathwayList[[i]])
        if(eCount>0){ 
		     flag<-enzymeType
             if(nodeType=="geneProduct"){
                 flag<-"compound"
             }
             VindexLength<-length(enzyme_index)
            if(VindexLength>0){
            for(j in 1:VindexLength){		
                 outIndex<-c()#
                 outIndex<-as.integer(neighbors(pathwayList[[i]],enzyme_index[j],"out"))
                 outIndexLength<-length(outIndex)
			     Out<-c();In<-c();Mid<-list()
			     #
                 if(outIndexLength>0){
                     for(k in 1:outIndexLength){
				        outType<-get.vertex.attribute(pathwayList[[i]],"type",outIndex[k])
                        if(outType==flag){
                             Index<-as.integer(neighbors(pathwayList[[i]],outIndex[k],"out"))
                             IndexLength<-length(Index)
                             if(IndexLength>0){
                                 for(m in 1:IndexLength){
						        
						             if((directed==TRUE&&Index[m]!=enzyme_index[j])||
								       (directed==FALSE&&Index[m]>enzyme_index[j])){
                                        if(unlist(strsplit(get.vertex.attribute(pathwayList[[i]],"id",Index[m]),"_"))[1]!=unlist(strsplit(get.vertex.attribute(pathwayList[[i]],"id",enzyme_index[j]),"_"))[1]&&get.vertex.attribute(pathwayList[[i]],"type",Index[m])==temp_nodeType){
										matched_enzyme_index<-match(Index[m],In)
										     if(is.na(matched_enzyme_index)){
	                                             Out<-c(Out,enzyme_index[j])
                                                 In<-c(In,Index[m])
										         Mid[[length(In)]]<-outIndex[k]
										     }else{
											     Mid[[matched_enzyme_index]]<-c(Mid[[matched_enzyme_index]],outIndex[k])
										     }								  
                                        }
							        }
                                }#end for(m in 1:IndexLength)
                            }###end if(IndexLength>0)       
                        }else if(directEdge==TRUE&&outType==temp_nodeType){

						        if((directed==TRUE&&outIndex[k]!=enzyme_index[j])||
								 (directed==FALSE&&outIndex[k]>enzyme_index[j])){
                                    if(unlist(strsplit(get.vertex.attribute(pathwayList[[i]],"id",outIndex[k]),"_"))[1]!=unlist(strsplit(get.vertex.attribute(pathwayList[[i]],"id",enzyme_index[j]),"_"))[1]){
										   matched_enzyme_index<-match(outIndex[k],In)
										   if(is.na(matched_enzyme_index)){
	                                             Out<-c(Out,enzyme_index[j])
                                                 In<-c(In,outIndex[k])
												 Mid[[length(In)]]<--1
										   }
										   else{
											Mid[[matched_enzyme_index]]<-c(Mid[[matched_enzyme_index]],-1)
										   }								  
                                    }
							    }                        
                        }#end if(outType==flag)					
                    }###for(k in 1:outIndexLength)
                }###if(outIndexLength>0)
			    start<-c(start,Out);terminate<-c(terminate,In);middle<-c(middle,Mid)
            }##for(j in 1:VindexLength)
            }###if(VindexLength>0)  
        }##if(eCount>0)
 

        startLength<-length(start)
        if(startLength>0){
            entry1<-c();entry2<-c();Eid<-c();Enames<-c();Etype<-c();Ereaction<-c();Egraphics_name<-c()
            for(j in 1:startLength){
                 entry1[j]<-get.vertex.attribute(pathwayList[[i]],"id",start[j])
                 entry2[j]<-get.vertex.attribute(pathwayList[[i]],"id",terminate[j])

			    if(middle[[j]][1]!=-1){
			         Eid[j]<-get.vertex.attribute(pathwayList[[i]],"id",middle[[j]][1])
                     Enames[j]<-get.vertex.attribute(pathwayList[[i]],"names",middle[[j]][1])
                     Etype[j]<-get.vertex.attribute(pathwayList[[i]],"type",middle[[j]][1])
                     Ereaction[j]<-get.vertex.attribute(pathwayList[[i]],"reaction",middle[[j]][1])
			         Egraphics_name[j]<-get.vertex.attribute(pathwayList[[i]],"graphics_name",middle[[j]][1])
			    }else{
			         Eid[j]<-"unknow"
                     Enames[j]<-"unknow"
                     Etype[j]<-"unknow"
                     Ereaction[j]<-"unknow"
			         Ereaction[j]<-"unknow"
			         Egraphics_name[j]<-"unknow"
			    }
                middleJLength<-length(middle[[j]])
                if(middleJLength>1){
                     for(k in 2:middleJLength){
			             if(middle[[j]][k]!=-1){#20120709revised,
               #print(paste(middle[[j]][k],"ddd"))						 
               Eid[j]<-paste(Eid[j],";",get.vertex.attribute(pathwayList[[i]],"id",middle[[j]][k]),sep="")
               Enames[j]<-paste(Enames[j],";",get.vertex.attribute(pathwayList[[i]],"names",middle[[j]][k]),sep="")
               Etype[j]<-paste(Etype[j],";",get.vertex.attribute(pathwayList[[i]],"type",middle[[j]][k]),sep="")
               Ereaction[j]<-paste(Ereaction[j],";",get.vertex.attribute(pathwayList[[i]],"reaction",middle[[j]][k]),sep="")
			   Egraphics_name[j]<-paste(Egraphics_name[j],";",get.vertex.attribute(pathwayList[[i]],"graphics_name",middle[[j]][k]),sep="")
			            }else{
               Eid[j]<-paste(Eid[j],";","unknow",sep="")
               Enames[j]<-paste(Enames[j],";","unknow",sep="")
               Etype[j]<-paste(Etype[j],";","unknow",sep="")
               Ereaction[j]<-paste(Ereaction[j],";","unknow",sep="")	
			   Egraphics_name[j]<-paste(Egraphics_name[j],";","unknow",sep="")
			            }
			   
                    }
                }
            }##for(j in 1:startLength)
        }##end if(startLength>0)
		
#######################################		
		if(startLength>0){
             vertex<-data.frame(ID=id,id=id,names=names,type=type,reaction=reaction,link=link,
		     graphics_name=graphics_name,graphics_fgcolor=graphics_fgcolor,graphics_bgcolor=graphics_bgcolor,
		     graphics_type=graphics_type,graphics_x=graphics_x,graphics_y=graphics_y,graphics_width=graphics_width,
		     graphics_height=graphics_height,graphics_coords=graphics_coords)
             edges<-data.frame(entry1=entry1,entry2=entry2,id=Eid,names=Enames,type=Etype,reaction=Ereaction,graphics_name=Egraphics_name)
		#edges<-unique(edges)	
             graphList[[i]]<-graph.data.frame(edges,directed=directed,vertex)
        }else{
             graphList[[i]]<-graph.empty(n=0,directed=directed)
             graphList[[i]]<-add.vertices(graphList[[i]],length(id),name=id,id=id,names=names,type=type,
		         reaction=reaction,link=link,graphics_name=graphics_name,graphics_fgcolor=graphics_fgcolor,
		         graphics_bgcolor=graphics_bgcolor,graphics_type=graphics_type,graphics_x=graphics_x,
		         graphics_y=graphics_y,graphics_width=graphics_width,graphics_height=graphics_height,
                 graphics_coords=graphics_coords)
        }##else
	    graphList[[i]]<-set.graph.attribute(graphList[[i]],"name",get.graph.attribute(pathwayList[[i]],"name"))
        graphList[[i]]<-set.graph.attribute(graphList[[i]],"number",get.graph.attribute(pathwayList[[i]],"number"))
        graphList[[i]]<-set.graph.attribute(graphList[[i]],"org",get.graph.attribute(pathwayList[[i]],"org"))
        graphList[[i]]<-set.graph.attribute(graphList[[i]],"title",get.graph.attribute(pathwayList[[i]],"title"))
        graphList[[i]]<-set.graph.attribute(graphList[[i]],"image",get.graph.attribute(pathwayList[[i]],"image"))
        graphList[[i]]<-set.graph.attribute(graphList[[i]],"link",get.graph.attribute(pathwayList[[i]],"link"))

    }##for(i in 1:pathwayListLength)
    }
    names(graphList)<-names(pathwayList)
    return(graphList) 
}
