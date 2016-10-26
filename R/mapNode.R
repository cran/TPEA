mapNode <-
function(graphList){
    newgraphList<-list()
    graphListLength<-length(graphList)
	if(graphListLength>0){
    for(i in 1:graphListLength){     
	     org<-graphList[[i]]$org
		 nodeType<-""
		 if(org=="ec"){
		     nodeType<-"enzyme"
		 }else if(org=="ko"){
		     nodeType<-"ortholog"
		 }else{
		     nodeType<-"gene"
		 }
		 name<-c();id<-c();names<-c();type<-c();reaction<-c();link<-c()
         graphics_name<-c();graphics_fgcolor<-c();graphics_bgcolor<-c()              
         graphics_type<-c();
         graphics_width<-c();graphics_height<-c();graphics_coords<-c() 
		 Vcount<-vcount(graphList[[i]])
		 if(Vcount>0){
		 for(j in 1:Vcount){
		     #if(get.vertex.attribute(graphList[[i]],"type",j-1)==nodeType){
		     if(get.vertex.attribute(graphList[[i]],"type",j)==nodeType){			 
		         #node_name<-get.vertex.attribute(graphList[[i]],"names",j-1)
		         node_name<-get.vertex.attribute(graphList[[i]],"names",j)				 
		         expand_node_names<-unlist(strsplit(node_name,"[ ;]"))
				 if(org=="ko"){
	                 genes<-getGeneFromKO(expand_node_names)
				 }else if(org=="ec"){
	                 genes<-getGeneFromEnzyme(expand_node_names)				 
				 }else{
	                 genes<-getGeneFromKGene(expand_node_names)				     
				 }
				 if(length(genes)>0){
		             new_node_names<-paste(genes,collapse=" ")
				     #print(new_node_names)
                     names<-c(names,new_node_names)
                     type<-c(type,"gene")
                     link<-c(link,"unknow")
					 if(length(genes)>1){
                         graphics_name<-c(graphics_name,paste(genes[1],"...",sep=""))
					 }else{
					     graphics_name<-c(graphics_name,genes[1])
					 }
                     graphics_fgcolor<-c(graphics_fgcolor,"#000000")
                     graphics_bgcolor<-c(graphics_bgcolor,"#BFFFBF")
                }else{
                     #names<-c(names,get.vertex.attribute(graphList[[i]],"names",j-1))
                     #type<-c(type,get.vertex.attribute(graphList[[i]],"type",j-1))
                     #link<-c(link,get.vertex.attribute(graphList[[i]],"link",j-1))
                     #graphics_name<-c(graphics_name,get.vertex.attribute(graphList[[i]],"graphics_name",j-1))
                     names<-c(names,get.vertex.attribute(graphList[[i]],"names",j))
                     type<-c(type,get.vertex.attribute(graphList[[i]],"type",j))
                     link<-c(link,get.vertex.attribute(graphList[[i]],"link",j))
                     graphics_name<-c(graphics_name,get.vertex.attribute(graphList[[i]],"graphics_name",j))					 
                     graphics_fgcolor<-c(graphics_fgcolor,"#000000")
                     graphics_bgcolor<-c(graphics_bgcolor,"#FFFFFF")
                }				
				 
			}else{
                 #names<-c(names,get.vertex.attribute(graphList[[i]],"names",j-1))
                 #type<-c(type,get.vertex.attribute(graphList[[i]],"type",j-1))
                 #link<-c(link,get.vertex.attribute(graphList[[i]],"link",j-1))
                 #graphics_name<-c(graphics_name,get.vertex.attribute(graphList[[i]],"graphics_name",j-1))
                names<-c(names,get.vertex.attribute(graphList[[i]],"names",j))
                 type<-c(type,get.vertex.attribute(graphList[[i]],"type",j))
                 link<-c(link,get.vertex.attribute(graphList[[i]],"link",j))
                 graphics_name<-c(graphics_name,get.vertex.attribute(graphList[[i]],"graphics_name",j))				 
                 graphics_fgcolor<-c(graphics_fgcolor,"#000000")
                 graphics_bgcolor<-c(graphics_bgcolor,"#FFFFFF")
			}			
		 }#end for(j in 1:Vcount)
		 }#end if(Vcount>0)
		 newgraphList[[i]]<-set.vertex.attribute(graphList[[i]],"names",value=names)
		 newgraphList[[i]]<-set.vertex.attribute(newgraphList[[i]],"type",value=type)
		 newgraphList[[i]]<-set.vertex.attribute(newgraphList[[i]],"link",value=link)	
		 newgraphList[[i]]<-set.vertex.attribute(newgraphList[[i]],"graphics_name",value=graphics_name)		 
 		 newgraphList[[i]]<-set.vertex.attribute(newgraphList[[i]],"graphics_fgcolor",value=graphics_fgcolor)		
		 newgraphList[[i]]<-set.vertex.attribute(newgraphList[[i]],"graphics_bgcolor",value=graphics_bgcolor)
         
		number<-get.graph.attribute(graphList[[i]],"number") 
		new_org<-c("hsa")
		new_idType<-c("ncbi-geneid")
	    newgraphList[[i]]<-set.graph.attribute(newgraphList[[i]],"name",paste("path:",new_org,number,sep=""))
        newgraphList[[i]]<-set.graph.attribute(newgraphList[[i]],"org",paste(new_org,new_idType,sep=";"))
        newgraphList[[i]]<-set.graph.attribute(newgraphList[[i]],"image",paste("http://www.genome.jp/kegg/pathway/",
		  new_org,"/",new_org,number,".png",sep=""))
        newgraphList[[i]]<-set.graph.attribute(newgraphList[[i]],"link",paste("http://www.genome.jp/kegg-bin/show_pathway?",
		  new_org,number,sep=""))
        names(newgraphList)[i]<-names(graphList)[i]
	}
	}
	return (newgraphList)
}
