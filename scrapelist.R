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
level2links<-rep(NA,10)

for (i in 1:length(level2links)){
#   level2links[i]<-level1$url[1] %>%
#     html() %>%
#     html_nodes(.,"table") %>%
#     extract(3) %>%
#     html_nodes(.,"div") %>%
#     extract(i+7) %>%
#     html_node(.,"a") %>%
#     html_attr(.,"href") %>%
  level2links[i]<-paste("http://www.dst.dk/valg/Valg1487635/valgopgmid/valgopgStor",i+9,".htm",sep="")
}

level2links

#opstillingskredse
#storkredse
level3links<-rep(NA,92)
for (i in 1:length(level3links)){
  #   level2links[i]<-level1$url[1] %>%
  #     html() %>%
  #     html_nodes(.,"table") %>%
  #     extract(3) %>%
  #     html_nodes(.,"div") %>%
  #     extract(i+7) %>%
  #     html_node(.,"a") %>%
  #     html_attr(.,"href") %>%
  level3links[i]<-paste("http://www.dst.dk/valg/Valg1487635/valgopgmid/valgopgOpst",i+19,".htm",sep="")
}

level3links[1] %>%
  html()

%>%
  html_nodes(.,"table") %>%
  extract(5) %>%
  html_table(.,fill=T,header=T)


node(.,"td")


%>%
  unlist()


%>%
  html_attrs(.,"vaelgeropg_parti")

%>%
  html_


%>%
  html_table()


for (i in 1:length(level2links)){
  tmp<-level2links[i] %>%
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


  
http://www.dst.dk/valg/Valg1487635/valgopgmid/valgopgOpst20.htm

for (i in 1:3){
  tmp<-
    

  
  %>%
    html_nodes(.,"a")
    
    html_attrs(.,"href")
  %>%
    html_table()
  
  %>%
    as.data.frame()
  
  %>%
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

setwd("~/GitHub/fv15results")
write.csv(level4,"valgsteder.csv",quote=T)

#get results from 2011
#gather all files in 1 df
csvs<-list.files(path="../fv11-valgsteder",pattern=".csv")[2:11]
allres<-data.frame(unit=NA,fv11_allvotes=NA,fv11_sd=NA,fv11_rv=NA,fv11_kf=NA,fv11_sf=NA,fv11_la=NA,fv11_kd=NA,fv11_df=NA,fv11_ve=NA,fv11_el=NA)
for (f in csvs){
  dat<-read.csv(paste("../fv11-valgsteder/",f,sep=""),sep=";",skip=5,header=F,fileEncoding="latin1")[,2:12]
  names(dat)<-names(allres)
  dat[,1]<-iconv(dat[,1],from="latin1",to="UTF-8",sub="X")
  allres<-rbind(allres,dat)
}
fv11_allres<-allres[2:nrow(allres),]
rm(allres,f,dat,csvs)

#get level 3 results
fv11_level3<-slice(fv11_allres,which(str_detect(fv11_allres$unit,"OPSTILLINGSKREDS")))
fv11_level3$unit<- fv11_level3$unit %>%
  gsub(" OPSTILLINGSKREDS","",.) %>%
  tolower() %>%
  gsub("ã\u0086","ae",.) %>%
  gsub("ã\u0098","oe",.) %>%
  gsub("ã\u0085","aa",.)

#merge into fv15 df
level3$unit <- level3$navn %>%
  tolower() %>%
  gsub("æ","ae",.) %>%
  gsub("ø","oe",.) %>%
  gsub("å","aa",.)

level3<-left_join(level3,fv11_level3,by="unit",copy=T)

#fix small typos
for (i in 1:nrow(level3)){
  if (!is.na(level3$fv11_allvotes[i]) & level3$pop[i]<level3$fv11_allvotes[i]){
    level3$pop[i]<-10*level3$pop[i]
  }
}

#create summary variables for comparison
level3out <- level3 %>%
  mutate(fv11_redvotes=fv11_sd+fv11_rv+fv11_sf+fv11_el,
         fv11_redshare=fv11_redvotes/fv11_allvotes,
         fv15_redshare=NA)

sum(level3out$fv11_redvotes,na.rm=T)/sum(level3out$fv11_allvotes,na.rm=T)

write.csv(level3out,file="level3out.csv")
