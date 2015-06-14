#get list of regions
require(rvest)
require(dplyr)

basepath<-"http://www.dst.dk/valg/Valg1487635/vaelgertal/"

#landsdele
level1<- paste(basepath,"vaelgertalHL.htm",sep="") %>%
  html() %>%
  html_nodes(.,"table")

level1<-level1[[4]] %>%
  html_table() %>%
  as.data.frame() %>%
  transmute(name=X1,pop=as.numeric(gsub("\\.","",X2)))

level1links<-paste(basepath,"vaelgertalHL.htm",sep="") %>%
  html() %>%
  html_nodes(.,"a") %>%
  html_attr(.,"href") %>%
  extract(9:11) %>%
  paste(basepath,.,sep="")

level1$url<-c(level1links,NA)

#storkredse
for (i in 1:3){
  tmp<-level1$url[i] %>%
    html() %>%
    html_nodes(.,"table") %>%
    extract(4) %>%
    html_table() %>%
    as.data.frame() %>%
    transmute(region=level1$name[i],name=X1,pop=as.numeric(gsub("\\.","",X2))) %>%
    slice(-length(name))
  if (i==1){
    level2<-tmp
  }
  if (i>1){
    level2<-rbind(level2,tmp)
  }
}  

require(stringr)  

for (i in 1:length(level1links)){
  tmp<-  level1links[i] %>%
    html() %>%
    html_nodes(.,"a") %>%
    html_attr(.,"href") %>%
    extract(which(str_detect(.,"Stor"))) %>%
    paste(basepath,.,sep="")
  if (i==1){
    level2links<-tmp
  }
  if (i>1){
    level2links<-c(level2links,tmp)
  }
}
level2$url<-level2links

#opstillingskredse
for (i in 1:length(level2links)){
  tmp<-  level2links[i] %>%
    html() %>%
    html_nodes(.,"a") %>%
    html_attr(.,"href") %>%
    extract(which(str_detect(.,"Opst"))) %>%
    paste(basepath,.,sep="")
  if (i==1){
    level3links<-tmp
  }
  if (i>1){
    level3links<-c(level3links,tmp)
  }
}

for (i in 1:length(level2links)){
  tmp<-level2links[i] %>%
    html() %>%
    html_nodes(.,"table") %>%
    extract(4) %>%
    html_table() %>%
    as.data.frame() %>%
    transmute(landsdel=level2$region[i],storkreds=level2$name[i],navn=X1,pop=as.numeric(gsub("\\.","",X2))) %>%
    slice(-length(navn))
  if (i==1){
    level3<-tmp
  }
  if (i>1){
    level3<-rbind(level3,tmp)
  }
}
level3$url<-level3links

#valgsted
for (i in 1:length(level3links)){
  nodes<-level3links[i] %>%
    html() %>%
    html_nodes(.,"table") %>%
    extract(4) %>%
    html_nodes(.,"td")
  
  nodetags<-nodes %>%
    html_attr(.,"class")
  
  valgsted<-nodes %>%
    html_text() %>%
    extract(which(nodetags=="vaelgertalDetailOp"))
  
  pop<-nodes %>%
    html_text() %>%
    extract(which(nodetags=="vaelgertalNummer")) %>%
    gsub("\\.","",.) %>%
    as.numeric()
  
  tmp<-data.frame(landsdel=level3$landsdel[i],
                  storkreds=level3$storkreds[i],
                  opstillingskreds=level3$navn[i],
                  valgsted=valgsted,
                  pop=pop,
                  stringsAsFactors = F)
    if (i==1){
      level4<-tmp
    }
    if (i>1){
      level4<-bind_rows(level4,tmp)
    }
}




