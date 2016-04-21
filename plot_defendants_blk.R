# map homes of charged individuals (regardless of disposition status)
library(ggmap)
library(sp)
library(maps)
library(maptools)
library(PBSmapping)
library(rgdal)
library(Rmisc)
library(readr)
library(magrittr)
library(dplyr)
library(rvest)
library(gridExtra)
library(UScensus2010)
library(UScensus2010blk)

# Some references on making maps
#  * http://www.kevjohnson.org/making-maps-in-r/
#  * http://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
#  * UScensus2010: https://www.jstatsoft.org/article/view/v037i06

bond_combined2 <- read_csv("bond_combined2.csv")
data <- read_csv("court_data/defendants_geocoded.csv")

# gather the defendant geocoded data
plot_data <- bond_combined2 %>% rename(Case_Number=Warrant.No) %>% 
  left_join(data %>% ungroup,by="Case_Number") %>% 
  filter(!is.na(long) & !is.na(lat))
plot_data1 <- plot_data %>% filter(long>-85 & long< -81.5 & lat< 35.5 & lat > 34)

# analysis of census blocks ---------------------------------------------------

# turn our geocoded defendant data into spatial points data frame to work with shapefiles and over()
geoTable <- plot_data %>%
  select(one_of(c("Case_Number","lat","long"))) %>% 
  rename(X=long,Y=lat) %>% as.data.frame
geoSP <- SpatialPointsDataFrame(geoTable %>% select(one_of(c("X","Y"))),geoTable %>% select(one_of("Case_Number")),
                                proj4string = CRS("+proj=longlat"))

# load in census tract data
data("south_carolina.blk10")
# note: a boatload of demographic information is included, however names are arcane codes
# decode here: http://starr.tamu.edu/files/2013/01/Census-Codes.pdf

gvl_cty <-  county(name="lexington",state="sc",level="blk")
gvl_cty_tr <- spTransform(gvl_cty,CRS("+proj=longlat"))
# match up residences with census tract regions, merge on charge information
addr_poly <- over(geoSP,gvl_cty)
geoTable <- cbind(geoTable,addr_poly)
geoTable2 <- bond_combined2 %>% 
  select(one_of(c("Offense","Warrant.No","Year","Number.of.Days","Disposition.Simple"))) %>% 
  mutate(Disposition.Guilty=ifelse(Disposition.Simple=="Guilty",1,0)) %>% 
  rename(Case_Number=Warrant.No) %>% 
  right_join(geoTable) %>% 
  as.data.frame


# first attempt to plot choropleth ----------------------------------------


# choropleth of number of crimes, using UScensus2010 plot method
for (crime in unique(geoTable2$Offense)) {
  myTrt <- table(factor(geoTable2[geoTable2$Offense==crime,"tract"],levels=levels(as.factor(geoTable$tract))))
  myTot <- sum(myTrt)
  mapcolors <- heat.colors(max(myTrt)+1,alpha = 0.6)[max(myTrt)-myTrt+1]
  plot(gvl_cty,main="Greenville County\nResidences",xlab="",ylab="",col=mapcolors,
       sub=paste0("Crime: ",crime,", Range: 0 (light) to ",max(myTrt)," (dark); Total: ",myTot,"\nNote: data restricted to residents of Greenville County, SC"))
}
#legend("bottom",legend=max(myTrt):0,fill=heat.colors(max(myTrt)+1,alpha = 0.6),title="Crimes")


# overlay choropleth map on Google map, use ggplot2 -----------------------

# help comes from Stack Overflow item http://stackoverflow.com/questions/20039297/using-ggplot-to-create-a-choropleth-from-a-spatialpolygonsdataframe

mapImage <- ggmap(get_googlemap(c(lon=-82.394012,lat=34.852619), scale=1, zoom=10), extent="normal")
# looked up the lat/lon for Greenville, did some experimentation with scale and zoom

plot_data <- fortify(gvl_cty)
plot_data <- cbind(plot_data,gvl_cty@data[plot_data$id,])

# plot frequencies of crimes
# for prettying of graphs, see
#  * losing the axes: http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/

for (year_analyze in 2012:2014) {
  geoTable3 <- geoTable2 %>% filter(Year==year_analyze)
  for (crime in unique(geoTable2$Offense)) {
    myTrt <- table(factor(geoTable3[geoTable3$Offense==crime,"tract"],levels=levels(as.factor(geoTable$tract))))
    myTrtdf <- data.frame(myTrt)
    names(myTrtdf)[1] <- "tract"
    myTot <- sum(myTrt)
    
    plot_data2 <- plot_data %>% left_join(myTrtdf)
    this_map <- mapImage + 
      geom_polygon(aes(x=long,y=lat,group=group,fill=Freq),
                   size=0.1,color="black",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
      scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
      scale_fill_continuous(name="Number of people\ncharged") +
      ggtitle(paste0("Residence of those charged with ",crime,", Year: ",year_analyze,"\nGreenville residents only; total: ",myTot))
    print(this_map)
  }
}

# plot mean number of days to post bond  
for (year_analyze in 2012:2014) {
  for (crime in unique(geoTable2$Offense)) {
    myTrtdf <- geoTable2 %>% 
      filter(Offense==crime & Year==year_analyze & !is.na(tract)) %>% 
      group_by(tract) %>% 
      summarize(sum_stat=mean(Number.of.Days)) %>% 
      as.data.frame
    
    plot_data2 <- plot_data %>% left_join(myTrtdf)
    this_map <- mapImage + 
      geom_polygon(aes(x=long,y=lat,group=group,fill=sum_stat),
                   size=0.1,color="black",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
      scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
      scale_fill_continuous(name="Mean number of\ndays to post bond") +
      ggtitle(paste0("Residence of those charged with ",crime,", Year: ",year_analyze,"\nGreenville residents only; total charges: ",myTot))
    print(this_map)
  }
}
#  geom_point(aes(x=long,y=lat,colour=Offense),data=plot_data1)

# plot mean number of days to post bond on a grid
myTrtdf <- geoTable2 %>% 
  filter(!is.na(tract)) %>% 
  group_by(Offense,Year,tract) %>% 
  summarize(sum_stat=mean(Number.of.Days)) %>% 
  as.data.frame
plot_data2 <- plot_data %>% left_join(myTrtdf) %>% filter(!is.na(Offense)&!is.na(Year))
this_map <- mapImage + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=sum_stat),
               size=0.1,color="gray50",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
  scale_fill_gradient(name="Mean days\nto post bond",low="blue",high="red",na.value="gray10") +
  facet_grid(Year~Offense)
print(this_map)


# % guilty
myTrtdf <- geoTable2 %>% 
  filter(!is.na(tract)) %>% 
  group_by(Offense,Year,tract) %>% 
  summarise(sum_stat=mean(Disposition.Guilty)) %>% 
  as.data.frame

plot_data2 <- plot_data %>% left_join(myTrtdf) %>% filter(!is.na(Offense)&!is.na(Year))
this_map <- mapImage + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=sum_stat),
               size=0.1,color="gray50",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
  scale_fill_gradient(name="% Guilty",low="gray10",high="red",limits=c(0,1),na.value="white") +
  facet_grid(Year~Offense)
print(this_map)
#ggtitle(paste0("Residence of those charged with ",crime,", Year: ",year_analyze,"\nGreenville residents only; total charges: ",myTot))
#multiplot(plotlist=pguilty_maps,cols=3)
lapply(pguilty_maps,print)

# demography --------------------------------------------------------------
# for meanings of codes, help(south.carolina.tract10)
# note this has no crime data in it

demo_plot <-  plot_data %>% 
  mutate(wbr=P0030002/P0030003,whr=P0030002/P0040003) %>% 
  group_by(tract) %>%
  summarise(Pop=first(P0010001),wbr=first(wbr),whr=first(whr)) %>% 
  mutate(wbr_capped=ifelse(wbr>10,10,wbr))
plot_data2 <-  plot_data %>% left_join(demo_plot)

# total population

this_map <- mapImage + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=Pop),
               size=0.1,color="grey70",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
  scale_fill_continuous(name="Total population") +
  ggtitle("")
print(this_map)

# white-to-black ratio
this_map <- mapImage + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=log(wbr)/log(10)),
               size=0.1,colour="grey70",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
  scale_fill_gradient2(name="log10 White-to-black ratio") +
  ggtitle("White-to-black ratio of Greenville County\nFrom Census 2010")
print(this_map)

# white-to-hispanic ratio
this_map <- mapImage + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=log(whr)/log(10)),
               size=0.1,color="grey70",data=plot_data2,alpha=0.3) + xlab("") + ylab("") +
  scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
  scale_fill_gradient2(name="log White-to-Hispanic ratio",limits=c(-1,2)) +
  ggtitle("White-to-Hispanic Ratio of Greenville County\nFrom Census 2010")
print(this_map)


# get more demographic data -----------------------------------------------
# based on ideas here: http://notesofdabbler.bitbucket.org/2013_12_censusBlog/censusHomeValueExplore_wdesc.html
# but they get the API call wrong
# Census doc here: https://www.census.gov/data/developers/data-sets/decennial-census-data.html
# census site is down as of 3/27/2016, so ignore this crap

api_key <- "a83614a85254200e07d6e1f923d7989d55630543"
state <- 45

getCensusData=function(APIkey,state,fieldnm){
  require(RJSONIO)
  resURL=paste("http://api.census.gov/data/2010/sf1?get=",fieldnm,
               "&for=state:",state,"&key=",
               APIkey,sep="")
  cat(resURL)
  dfJSON=fromJSON(readLines(url(resURL)))
  dfJSON=dfJSON[2:length(dfJSON)]
  dfJSON_zip=sapply(dfJSON,function(x) x[3])
  dfJSON_val=sapply(dfJSON,function(x) x[1])
  df=data.frame(dfJSON_zip,as.numeric(dfJSON_val))
  names(df)=c("zip","val")
  return(df)
}

# get median age from US census 2010 for a state
fieldnm="P0010001"
dfage=getCensusData(api_key,state,fieldnm)
names(dfage)=c("zip","medAge")
head(dfage)


# zillow ------------------------------------------------------------------

zwsid <- "X1-ZWz1baxpgac6bv_2jayc"

# req_url <- paste0("http://www.zillow.com/webservice/GetDemographics.htm?zws-id=",zwsid,"&zip=",29615)
# foo <- read_html(req_url)
# 
# address <- "315 Majesty Ct"
# citystzip <- "Greenville, SC  29615"
# req_url <- paste0("http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id=",zwsid,"&address=",address,"&citystatezip=",citystzip)
# foo <- read_html(URLencode(req_url))
# foo %>% xml_nodes(xpath="//amount") %>% xml_text

# get home valuations
# today is 3/29/2016

base_url <- paste0("http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id=",zwsid)

# do some data manipulating on addresses to make them easier
# first, take our two-word towns and turn them into one word, for now
tmp_name <- gsub("Travelers Rest","Travelers_Rest",defendants$Address)
tmp_name <- gsub("Fountain Inn","Fountain_Inn",tmp_name)
ctystzip <- gsub("^.+ ([A-Za-z_]+ *[A-Z]{2} *[0-9-]{5,9})$","\\1",tmp_name,perl=TRUE)
addr <- gsub("^(.+)\\b +([A-Za-z_]+ *[A-Z]{2} *[0-9-]{5,9})$","\\1",tmp_name,perl=TRUE)
vals <- rep("",length(tmp_name))
for (i in 1:length(tmp_name)) {
  req <- paste("&address=",URLencode(addr[i]),"&citystatezip=",URLencode(ctystzip[i]),sep="")
  try(vals[i] <- paste0(base_url,req) %>% read_html %>% 
        xml_nodes(xpath="//amount") %>% xml_text,silent=TRUE)
}
addr[6]
vals[6]
defendants$zestimate <- as.numeric(vals)
defendants$owns_home <- !is.na(defendants$zestimate)
write_csv(defendants,"court_data/defendants_zestimate.csv")

qplot(Disposition.Simple,zestimate,
      data=bond_combined2 %>% rename(Case_Number=Warrant.No) %>% left_join(defendants,by="Case_Number"),
      geom="boxplot",facets = .~Offense,ylab="Zillow estimate of home value ($1000)") +
  scale_y_continuous(breaks=c(20,50,100,200,300,400,500)*1000,labels=as.character(c(20,50,100,200,300,400,500)),
                     limits=c(0,500000))

defendants %>% left_join(bond_combined2 %>% rename(Case_Number=Warrant.No),by="Case_Number") %>% 
  group_by(Offense,Disposition.Simple) %>% 
  summarise(n=n(),owns_home_freq=sum(owns_home),owns_home_frac=mean(owns_home))
