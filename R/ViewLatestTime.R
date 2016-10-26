ViewLatestTime <-
function(){
temp<-getURL("http://www.kegg.jp/kegg-bin/download?entry=hsa04010&format=kgml")
UpdateTime<-substr(temp,100,151)
return(UpdateTime)
}
