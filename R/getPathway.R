getPathway <-
function(path,filelist=list.files(path),verbose=FALSE){
  #begintime<-Sys.time()   
  mapList<-list()
  filelist_length<-length(filelist)
  jj<-0
  if(filelist_length>=1){
  for(j in 1:filelist_length){
	  if(verbose==TRUE)
         print(paste("deal with the pathway ",j," in ",filelist_length, " pathways",sep=""))
      #print(filelist[[j]])			  
      		  
		    
	  top_temp<-tryCatch(xmlTreeParse(paste(path,filelist[[j]],sep=""),error=NULL),error=function(e) "error")
    if(class(top_temp)[1]=="character"){
	   if(verbose==TRUE){
         cat(paste("warning:the pathway ",filelist[j]," don't exist or has errors.\n Therefore, it is deleted from lists.\n",sep=""))
       }		 
	}
	else{
      top<-xmlRoot(top_temp)
      pathwaylist<-list()
      pathwayAttrs<-list()
      pathwayAttrs[[1]]<-xmlGetAttr(top,"name","unknow")
      pathwayAttrs[[2]]<-xmlGetAttr(top,"number","unknow")
      pathwayAttrs[[3]]<-xmlGetAttr(top,"org","unknow")
      pathwayAttrs[[4]]<-xmlGetAttr(top,"title","unknow")
      pathwayAttrs[[5]]<-xmlGetAttr(top,"image","unknow")
      pathwayAttrs[[6]]<-xmlGetAttr(top,"link","unknow")
      names(pathwayAttrs)<-c("name","number","org","title","image","link")
      entry<-list()          
      relation<-list()
      reaction<-list()
      entryK<-0
      relationK<-0
      reactionK<-0
	  #tag1<-TRUE;tag2<-TRUE;tag3<-TRUE
      for(i in 1:length(top)){
	     #print(paste("entry",Sys.time(),sep=""))
		 top_i<-top[[i]]
		 xml_name<-xmlName(top_i)
         if(xml_name=="entry"){
		    
             entryK<-entryK+1
             entry[[entryK]]<-getEntry(top_i)   
			 #if(tag1==TRUE) {print(paste("entry",Sys.time(),sep=""));tag1=FALSE}
          }
         else if(xml_name=="relation"){
             relationK<-relationK+1
             relation[[relationK]]<-getRelation(top_i)    
             #if(tag2==TRUE) {print(paste("lat",Sys.time(),sep=""));tag2=FALSE}
             			 
          }   
          else if(xml_name=="reaction"){
		     #if(tag3==TRUE) {print(paste("reaction",Sys.time(),sep=""));tag3=FALSE}
             reactionK<-reactionK+1
             reaction[[reactionK]]<-getReaction(top_i)        
          }
      }
      if(entryK==0){entry[[1]]<-"this file don't have entrys"}
      if(relationK==0){relation[[1]]<-getUnknowRelation()}
      if(reactionK==0){reaction[[1]]<-getUnknowReaction()}
      pathwaylist[[1]]<-pathwayAttrs
      pathwaylist[[2]]<-entry
      pathwaylist[[3]]<-relation
      pathwaylist[[4]]<-reaction
      names(pathwaylist)<-c("pathwayAttrs","entry","relation","reaction")
	  jj<-jj+1
	  mapList[[jj]]<-pathwaylist
	  names(mapList)[jj]<-filelist[j]
	 }
  }#end for filelist	  
  }
  else{
     stop("should at least input one pathway file.") 
  }
  #print(Sys.time()-begintime)
  return(mapList)
}
