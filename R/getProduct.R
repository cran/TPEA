getProduct <-
function(product){
     productlist<-list()
     productlist[[1]]<-xmlGetAttr(product,"id","unknow")
     productlist[[2]]<-xmlGetAttr(product,"name","unknow")
     names(productlist)<-c("id","name")
     return(productlist)
}
