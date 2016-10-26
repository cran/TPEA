getGraphics <-
function(graphics){
         graphicslist<-list()
         graphicslist[[1]]<-xmlGetAttr(graphics,"name","unknow")
         graphicslist[[2]]<-xmlGetAttr(graphics,"fgcolor","unknow")
         graphicslist[[3]]<-xmlGetAttr(graphics,"bgcolor","unknow")
         graphicslist[[4]]<-xmlGetAttr(graphics,"type","unknow")
         graphicslist[[5]]<-xmlGetAttr(graphics,"x","unknow")
         graphicslist[[6]]<-xmlGetAttr(graphics,"y","unknow")
         graphicslist[[7]]<-xmlGetAttr(graphics,"width","unknow")
         graphicslist[[8]]<-xmlGetAttr(graphics,"height","unknow")
         graphicslist[[9]]<-xmlGetAttr(graphics,"coords","unknow")
         names(graphicslist)<-c("name","fgcolor","bgcolor","type","x","y","width","height","coords")
         return(graphicslist)
}
