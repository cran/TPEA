getEntry <-
function(entry){
     Enlist<-list()
     Childrenlength<-0
     componentId<-c()
     Enlist[[1]]<-xmlGetAttr(entry,"id","unknow")
     Enlist[[2]]<-xmlGetAttr(entry,"name","unknow")
     Enlist[[3]]<-xmlGetAttr(entry,"type","unknow")
     Enlist[[4]]<-xmlGetAttr(entry,"reaction","unknow")
     Enlist[[5]]<-xmlGetAttr(entry,"link","unknow")
     xml_children<-xmlChildren(entry)
     Childrenlength<-length(xml_children)
     Enlist[[6]]<-getGraphics(xml_children[[1]])
     if(Childrenlength>=2){
          for(i in 2:Childrenlength){
               componentId[i-1]<-xmlGetAttr(xml_children[[i]],"id","unknow")
          } 
     }
     if(length(componentId)==0){componentId[1]<-"unknow"}
     Enlist[[7]]<-componentId
     names(Enlist)<-c("id","name","type","reaction","link","graphics","component")
     return(Enlist)
}
