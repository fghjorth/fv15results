setwd("~/GitHub/fv15results")
require(ggplot2)
require(maptools)
require(ggmap)
require(sp)
require(rgdal) # requires sp, will use proj.4 if installed
require(maptools)
require(dplyr)

#read in map data
#dk<-readOGR(dsn="afstemningssteder",layer="afstemningssteder")
dk<-readOGR(dsn="shapefilesimple",layer="simpelt")

#prepare merge in data
dk.df<-data.frame(id=rownames(dk@data),opstilnav=dk@data$OpstilNav,afstemnav=dk@data$AfstemNav)

require(stringr)
dk.df$opst2<-dk.df$opstilnav %>%
  str_to_lower() %>%
  str_replace_all(.,"[0-9]","") %>%
  str_replace_all(.,"\\.","") %>%
  str_replace_all(.,"æ","ae") %>%
  str_replace_all(.,"ø","oe") %>%
  str_replace_all(.,"å","aa") %>%
  str_trim()

dk.df$afst2<-dk.df$afstemnav %>%
  str_to_lower() %>%
  str_replace_all(.,"[0-9]","") %>%
  str_replace_all(.,"\\.","") %>%
  str_replace_all(.,"æ","ae") %>%
  str_replace_all(.,"ø","oe") %>%
  str_replace_all(.,"å","aa") %>%
  str_trim()

#get 2015 data
#FV15
afstemres<-read.csv("valgresultat2015.csv",header=T,sep="\t",quote="\"")

afstemres$opst2<-afstemres$Opstillingskreds %>%
  str_to_lower() %>%
  str_replace_all(.,"[0-9]","") %>%
  str_replace_all(.,"\\.","") %>%
  str_replace_all(.,"æ","ae") %>%
  str_replace_all(.,"ø","oe") %>%
  str_replace_all(.,"å","aa") %>%
  str_trim()

afstemres$afst2<-afstemres$Afstemningsområde %>%
  str_to_lower() %>%
  str_replace_all(.,"[0-9]","") %>%
  str_replace_all(.,"\\.","") %>%
  str_replace_all(.,"æ","ae") %>%
  str_replace_all(.,"ø","oe") %>%
  str_replace_all(.,"å","aa") %>%
  str_trim()

afstemresr<-afstemres %>%
  mutate(avotes=A,
         vvotes=V,
         ovotes=O,
         redvotes=A+B+F+Ø+Å,
         bluevotes=C+I+K+O+V,
         redshare=redvotes/(redvotes+bluevotes)) %>%
  select(opst2,afst2,avotes,vvotes,ovotes,redvotes,bluevotes,redshare)

#check which districts are mismatched
dkmerged<-merge(dk.df,afstemresr,by=c("opst2","afst2"),all.x=T,all.y=T)
dkmerged<-subset(dkmerged, is.na(afstemnav) | is.na(redvotes))

dkmerged$afst2 %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  head(.,6)

#fix obvious mismatches
afstemresr$afst2<-afstemresr$afst2 %>%
  gsub("laesoe","byrum",.) %>%
  gsub("boelgen","bolgen",.) %>%
  gsub("mou hotel","mou - hallen",.) %>%
  gsub("loenborg,vostrup","loenborg",.) %>%
  gsub("raekker moelle","rk moelle",.) %>%
  gsub("vorgod-barde","vorgod",.) %>%
  gsub("oester hoejst","oe hoejst",.) %>%
  gsub("eggeslevmagle forsamlingshus","konfirmandstuen eggeslevmagle",.) %>%
  gsub("buskelund","buskelundskolen",.) %>%
  gsub("vodskov kultur &amp; idraetscenter","vodskov hallen",.) %>%
  gsub("gughallen","gug skole",.) %>%
  gsub("vejgaardhallen","vejgaard - hallen",.) %>%
  gsub("nibe hallen","nibe skole",.) %>%
  gsub("vejlby risskov hallen","vejlby-risskov hallen",.) %>%
  gsub("bispebjerg","utterslev",.) %>%
  gsub("grenaa idraetscenter","kattegatskolen",.) %>%
  gsub("saedding","saedding skole",.) %>%
  gsub("bryndum","bryndum skole",.) %>%
  gsub("hjerting","hjerting skole",.) %>%
  gsub("skads","skads skole",.) %>%
  gsub("tjaereborg","tjaereborg fritidscenter",.) %>%
  gsub("faaborg","faaborg oest",.) %>%
  gsub("administrationsbygningen, esp","administrationsbygningen mordrupvej",.) %>%
  gsub("helsingoer hallen","helngoer hallen",.) %>%
  gsub("kvistgaard idraetsanlaeg","kvistgaards idraetsanlaeg",.) %>%
  gsub("laden, gl vapnagaard","laden, glvapnagaard",.) %>%
  gsub("montebello, gurrevej","montebello",.) %>%
  gsub("hjoerring - centrum","hjoerring-centrum",.) %>%
  gsub("hjoerring - nord","hjoerring-nord",.) %>%
  gsub("hjoerring - syd","hjoerring-syd",.) %>%
  gsub("hjoerring - vest","hjoerring-vest",.) %>%
  gsub("hoejvangen","hoejvang",.) %>%
  gsub("avedoere idraetscenter","avedoere bibliotek",.) %>%
  gsub("nakskov idraetscenter","nakskov",.) %>%
  gsub("fjordager hallen","fjordager-hallen",.) %>%
  gsub("fks-hallen","sanderumhallen",.) %>%
  gsub("kroggaardskolen","kroggaardsskolen",.) %>%
  gsub("paarup hallen","paarup forsamlingshus",.) %>%
  gsub("ringkoebing","ringkoebing by",.) %>%
  gsub("jysk arena","silkeborghallerne",.)
  
#check which districts are mismatched
dkmerged<-merge(dk.df,afstemresr,by=c("opst2","afst2"),all.x=T,all.y=T)
dkmerged<-subset(dkmerged, is.na(afstemnav) | is.na(redvotes))

#set votes in unidentified places to zero
dk$avotes<-0
dk$vvotes<-0
dk$ovotes<-0
dk$bluevotes<-0
dk$redvotes<-0

dk$opst2<-dk$OpstilNav %>%
  str_to_lower() %>%
  str_replace_all(.,"[0-9]","") %>%
  str_replace_all(.,"\\.","") %>%
  str_replace_all(.,"æ","ae") %>%
  str_replace_all(.,"ø","oe") %>%
  str_replace_all(.,"å","aa") %>%
  str_trim()

dk$afst2<-dk$AfstemNav %>%
  str_to_lower() %>%
  str_replace_all(.,"[0-9]","") %>%
  str_replace_all(.,"\\.","") %>%
  str_replace_all(.,"æ","ae") %>%
  str_replace_all(.,"ø","oe") %>%
  str_replace_all(.,"å","aa") %>%
  str_trim()

#manual matching
for (i in 1:nrow(dk)){
  knownavotes<-subset(afstemresr,opst2==dk$opst2[i] & afst2==dk$afst2[i])$avotes
  knownvvotes<-subset(afstemresr,opst2==dk$opst2[i] & afst2==dk$afst2[i])$vvotes
  knownovotes<-subset(afstemresr,opst2==dk$opst2[i] & afst2==dk$afst2[i])$ovotes
  knownbluevotes<-subset(afstemresr,opst2==dk$opst2[i] & afst2==dk$afst2[i])$bluevotes
  knownredvotes<-subset(afstemresr,opst2==dk$opst2[i] & afst2==dk$afst2[i])$redvotes

  if (length(knownavotes)>0){
    dk$avotes[i]<-knownavotes
  }
  if (length(knownvvotes)>0){
    dk$vvotes[i]<-knownvvotes
  }
  if (length(knownovotes)>0){
    dk$ovotes[i]<-knownovotes
  }
  if (length(knownbluevotes)>0){
    dk$bluevotes[i]<-knownbluevotes
  }
  if (length(knownredvotes)>0){
    dk$redvotes[i]<-knownredvotes
  }
}

dka<-dotsInPolys(dk,dk$avotes)
dka$party="firebrick4"
dkv<-dotsInPolys(dk,dk$vvotes)
dkv$party="dodgerblue2"
dko<-dotsInPolys(dk,dk$ovotes)
dko$party="gold1"

dkb<-dotsInPolys(dk,dk$bluevotes)
dkb$bloc="blue"
dkr<-dotsInPolys(dk,dk$redvotes)
dkr$bloc="red"

#combine
dkall<-spRbind(dkr,dkb)

dkavo<-spRbind(dka,dkv)
dkavo<-spRbind(dkavo,dko)

head(dkall)

# extract the dataframe for ggplot
#bloc.df <- data.frame(coordinates(dkall)[,1:2], bloc=dkall$bloc)
bloc.df <- data.frame(coordinates(dkavo)[,1:2], party=dkavo$party)

#scramble rows
scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
bloc.df<-scramble(bloc.df)

#fortify base map
dk.fort<-fortify(dk)

bmap<-ggplot(subset(dk.fort,long<13), aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), colour = I("grey65"), size=0.2, fill = "grey85") + 
  coord_equal() +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank()) +
  xlab("") +
  ylab("")

bmap

bloc.df_samp<-bloc.df[sample(1:nrow(bloc.df),680000,replace=F),]

dotmap<-bmap +
  geom_point(data=subset(bloc.df,x<13), aes(x=x,y=y, colour = party), size=0.4,alpha=.5) + 
  scale_colour_manual(values=c("dodgerblue2","firebrick3","gold1")) +
  theme_minimal() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank())


pdf(file="dotmap.pdf",width=16,height=12)
dotmap
dev.off()

## chloropleth map

#merge in
dz<-merge(dk.df,afstemresr,by=c("opst2","afst2"))
dz<-merge(dk.fort,dz,by="id", sort = FALSE)
dz<-dz[order(dz$id,dz$order),] 
head(dz)

ggplot(subset(dz,long<13),aes(long,lat,group=group,fill=redshare)) + 
  #  geom_path(color="#dddddd") +
  geom_polygon() +  
  coord_fixed() +
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(legend.title = element_text(size=9),legend.text = element_text(size=9)) +
  scale_fill_gradientn("Red bloc \n vote share",colours=c("dark blue","red"))

ggsave("fv15map.png",width=15,height=10)


###### OLD STUFF


# 
# #fortify map data
# dk.fort<-fortify(dk)
# 


    
#set votes to 0 if missing
dk@data$redvotes[is.na(dk@data$redvotes)]<-0
dk@data$bluevotes[is.na(dk@data$bluevotes)]<-0


# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

# # DELETE NA's IN meuse AND SHOW CHANGE IN dim
# dkr<-sp.na.omit(dk)
# dim(dk)
# dim(dkr)

length(dk)
length(dk$bluevotes)

#get dots
dkb<-dotsInPolys(dk,dk$bluevotes)

as.integer(dk$bluevotes)

# #fortify map data
# dk.fort<-fortify(dk)
# dz<-merge(dk.fort,dk.df2,by="id", sort = FALSE)
# dz<-dz[order(dz$id,dz$order),] #reordering - very important
# 



row.names(dkr@data)

length(dkr@polygons)

names(dkr)

str(dkr)[[1]]
