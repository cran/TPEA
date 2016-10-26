getSubtype <-
function(subtype){
     subtypelist<-list()
     subtypelist[[1]]<-xmlGetAttr(subtype,"name","unknow")
     subtypelist[[2]]<-xmlGetAttr(subtype,"value","unknow")
     names(subtypelist)<-c("name","value")
     return(subtypelist)
}
