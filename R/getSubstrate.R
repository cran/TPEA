getSubstrate <-
function(substrate){
     substratelist<-list()
     substratelist[[1]]<-xmlGetAttr(substrate,"id","unknow")
     substratelist[[2]]<-xmlGetAttr(substrate,"name","unknow")
     names(substratelist)<-c("id","name")
     return(substratelist)
}
