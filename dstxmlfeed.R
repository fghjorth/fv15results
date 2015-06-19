setwd("~/GitHub/fv15results")
require(rvest)
require(dplyr)
require(magrittr)

#FV11
fv11url<-"http://www.dst.dk/valg/Valg1204271/xml/valgdag.xml"

#opstillingskredse data
fv11distdat<-fv11url %>%
  xml() %>%
  xml_nodes(.,"opstillingskreds") %>%
  xml_attrs() %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()

rownames(fv11distdat)<-1:nrow(fv11distdat)

#navne
fv11distdat$name<-fv11url %>%
  xml() %>%
  xml_nodes(.,"opstillingskreds") %>%
  xml_text()

fv11distdat$fv11_sd<-NA
fv11distdat$fv11_rv<-NA
fv11distdat$fv11_kf<-NA
fv11distdat$fv11_sf<-NA
fv11distdat$fv11_la<-NA
fv11distdat$fv11_kd<-NA
fv11distdat$fv11_df<-NA
fv11distdat$fv11_ve<-NA
fv11distdat$fv11_el<-NA
names(fv11distdat)

#stemmetal
for (i in 1:nrow(fv11distdat)){
  fv11distdat[i,7:15]<-fv11distdat$filnavn[i] %>%
    as.character() %>%
    xml() %>%
    xml_nodes(.,"parti") %>%
    xml_attr(.,"stemmerpct") %>%
    as.numeric() %>%
    extract(1:9)
}


#FV15
fv15url<-"http://www.dst.dk/valg/Valg1487635/xml/valgdag.xml"

#opstillingskredse data
fv15distdat<-fv15url %>%
  xml() %>%
  xml_nodes(.,"opstillingskreds") %>%
  xml_attrs() %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()

rownames(fv15distdat)<-1:nrow(fv15distdat)

#navne
fv15distdat$name<-fv15url %>%
  xml() %>%
  xml_nodes(.,"opstillingskreds") %>%
  xml_text()

#note that this order needs to be identical to that in the xml feed
fv15distdat$fv15_sd<-NA
fv15distdat$fv15_rv<-NA
fv15distdat$fv15_kf<-NA
fv15distdat$fv15_sf<-NA
fv15distdat$fv15_la<-NA
fv15distdat$fv15_kd<-NA
fv15distdat$fv15_df<-NA
fv15distdat$fv15_ve<-NA
fv15distdat$fv15_el<-NA
fv15distdat$fv15_aa<-NA
names(fv15distdat)

#stemmetal
for (i in 1:nrow(fv15distdat)){
  fv15distdat[i,7:16]<-
    fv15distdat$filnavn[i] %>%
    as.character() %>%
    xml() %>%
    xml_nodes(.,"parti") %>%
    xml_attr(.,"stemmerpct") %>%
    as.numeric() %>%
    extract(1:10)
}

fv15distdat<-fv15distdat %>%
  select(opstillingskreds_id,storkreds_id,name,fv15_sd:fv15_aa)

fv11distdatmerge<-fv11distdat %>%
  select(opstillingskreds_id,storkreds_id,name:fv11_el)

alldistdat<-left_join(fv15distdat,fv11distdatmerge,by=c("storkreds_id","opstillingskreds_id"))

levels(alldistdat$storkreds_id)<-c("København","Kbh Omegn","Nordsjælland","Bornholm","Sjælland",
                                   "Fyn","Sydjylland","Østjylland","Vestjylland","Nordjylland")
  
require(ggplot2)
ggplot(alldistdat,aes(x=fv11_df,y=fv15_df,color=storkreds_id,group=storkreds_id)) +
  geom_smooth(method="lm",alpha=0) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  geom_point() +
  theme_minimal() +
  xlab("DF i 2011") +
  ylab("DF i 2015") +
  scale_color_discrete(name="Storkreds")

ggsave("dfcomparison.png")
