getRelation <-
function(relation){
     relationlist<-list()
     subtype<-list()
     subtypelength<-0
     relationlist[[1]]<-xmlGetAttr(relation,"entry1","unknow")
     relationlist[[2]]<-xmlGetAttr(relation,"entry2","unknow")
     relationlist[[3]]<-xmlGetAttr(relation,"type","unknow")
     xml_children<-xmlChildren(relation)
	 subtypelength<-length(xml_children)
	 if(subtypelength>=1){
         for(i in 1:subtypelength){
             subtype[[i]]<-getSubtype(xml_children[[i]])
         }
	 }
	 else{
	     subtype[[1]]<-getUnknowSubtype()
	 }
     relationlist[[4]]<-subtype
     names(relationlist)<-c("entry1","entry2","type","subtype")
     return(relationlist)
}
