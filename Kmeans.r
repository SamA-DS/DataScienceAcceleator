library(bduk)
library(tidymodels)
library(stringr)
library(sp)
library(sf)
rm(list = ls())

options(scipen = 10000)

LB_data <- bdukBQ(
"SELECT
  CAST(UPRN AS NUMERIC) as UPRN,
  POSTCODE,
  LATITUDE,
  LONGITUDE,
  X_COORDINATE,
  Y_COORDINATE,
  CLASSIFICATION_CODE,
  Region,
  Local_Body,
  Commercial_Exchange_Flag,
  Ofcom_Premise_Download_Speed,
  percentage_of_premises_with_0_to_2Mbits_download_speed,
  percentage_of_premises_with_2_to_5Mbits_download_speed,
  percentage_of_premises_with_5_to_10Mbits_download_speed,
  percentage_of_premises_with_10_to_30Mbits_download_speed,
  percentage_of_premises_with_30_to_300Mbits_download_speed,
  percentage_of_premises_with_greater_than_300Mbits_download_speed,
  F20_Percentile_WEIGHT_CURVE,
  F20_Flag,
  SF_UPRN_delivered_flag,
  SF_postcode_delivered_flag,
  LFFN_postcode_flag,
  LFFN_UPRN_flag,
  ONS_Rurality_Code,
  fibre_premise_flag,
  fibre_postcode_flag,
  Ofcom_Area2_Flag,
  Ofcom_Area3_Flag,
  Exchange_code
FROM
  `dcms-datalake-staging.APP_DATA.SiteSummary_CurrentSpine_3`
WHERE Local_Body in('Kent')"
)

Exchange_Locations<-bdukBQ("SELECT 
Exchange_name,
Exchange as Exchange_code,
Easting,
Northing 
FROM  
`dcms-datalake-staging.GEO_OTHER.Exchange_Locations`")%>%
  mutate_if(is.numeric,as.numeric)

library(sp)
library(sf)

#BigQuery read in Easting/Northing as int64, which R doesn't like, so make them just numeric
Exchange_Locations<-
  Exchange_Locations%>%mutate_if(is_numeric,as_numeric)

#Set coord system (Eating/Northing UK zone (I think 30) is 27700) with columns
#Convert to lat/long which is 4326
makeSpatial<-function(DF,Xcoord,Ycoord,CurrentCRS,DesiredCRS,SF_or_SP){
  if(SF_or_SP=="SP"){
    DF<-SpatialPointsDataFrame(coords=DF%>%select(Xcoord,Ycoord),
                               data=DF%>%select(-Xcoord,-Ycoord))
  }
}

#With the SF package:
Exchange_Locations_sf<-Exchange_Locations%>%
  sf::st_as_sf(coords=c("Easting","Northing"), crs=27700)%>%
  st_transform(CRS("+init=epsg:4326"))
#With the SP packages
Exchange_Locations_sp<-SpatialPointsDataFrame(coords=Exchange_Locations%>%select(Easting,Northing),
                                              data=Exchange_Locations%>%select(-Easting,-Northing),
                                              proj4string=CRS("+init=epsg:27700")
                                              )%>%
  spTransform(CRS("+init=epsg:4326"))



library(leaflet)
library(leafgl)
leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addTiles(options = tileOptions(opacity = 0.8), group = "Open Street Map") %>%
  addGlPoints(data=Exchange_Locations_sp,
              radius=3
  )

SHP<-st_read("Rural areas within LEPs Nov19 updates.shp")


pnts <- sf::st_as_sf(DFCoords, coords=c("LONGITUDE_Plot","LATITUDE_Plot"), crs=4326)
map <- sf::st_as_sf(SHP, crs=4326)

Exchange_Locations = SpatialPoints(cbind(Exchange_Locations$Easting, Exchange_Locations$Northing), proj4string=CRS("+init=epsg:32748"))

"+proj=longlat"
Exchange_Points <- spTransform(Exchange_Locations, CRS("+proj=longlat"))


#ML methoods often don't run with NA's, so we check we have none
all(!is.na(LB_data$Ofcom_Premise_Download_Speed))
all(!is.na(LB_data$LATITUDE))
all(!is.na(LB_data$LONGITUDE))
all(!is.na(LB_data$F20_Percentile_WEIGHT_CURVE))


#For prems where we have no ofcom download speed, we will take postcode average
AvPostcode<-LB_data%>%select(Ofcom_Premise_Download_Speed,POSTCODE,F20_Percentile_WEIGHT_CURVE)%>%
  group_by(POSTCODE)%>%
  summarise(AvSpeed_Postcode=mean(Ofcom_Premise_Download_Speed, na.rm=TRUE),
            AvPercentile_Postcode=mean(F20_Percentile_WEIGHT_CURVE, na.rm=TRUE))%>%
  mutate(Half_Postcode=str_sub(POSTCODE, end=3))
#Still some NA's for postcodes where no premise have a download speed
AvHalfPostcode<-LB_data%>%select(Ofcom_Premise_Download_Speed,POSTCODE,F20_Percentile_WEIGHT_CURVE)%>%
  mutate(Half_Postcode=str_sub(POSTCODE, end=3))%>%
  group_by(Half_Postcode)%>%
  summarise(AvSpeed_HalfPostcode=mean(Ofcom_Premise_Download_Speed,na.rm=TRUE),
            AvPercentile_HalfPostcode=mean(F20_Percentile_WEIGHT_CURVE, na.rm=TRUE))

PostcodeAverages<-AvPostcode%>%
  left_join(AvHalfPostcode)%>%
  mutate(AvSpeed_Postcode=ifelse(is.na(AvSpeed_Postcode),AvSpeed_HalfPostcode,AvSpeed_Postcode),
         AvPercentile_Postcode=ifelse(is.na(AvPercentile_Postcode),AvPercentile_HalfPostcode,AvPercentile_Postcode))%>%
  select(POSTCODE,AvSpeed_Postcode,AvPercentile_Postcode)

all(!is.na(PostcodeAverages$AvSpeed_Postcode))
all(!is.na(PostcodeAverages$AvPercentile_Postcode))
rm(AvPostcode,AvHalfPostcode)

LB_data_clean<-LB_data%>%
  left_join(PostcodeAverages,by="POSTCODE")%>%
  mutate(Ofcom_Premise_Download_Speed=ifelse(is.na(Ofcom_Premise_Download_Speed),AvSpeed_Postcode,Ofcom_Premise_Download_Speed),
         F20_Percentile_WEIGHT_CURVE=ifelse(is.na(F20_Percentile_WEIGHT_CURVE),AvPercentile_Postcode,F20_Percentile_WEIGHT_CURVE))%>%
  select(-AvPercentile_Postcode,-AvSpeed_Postcode)

#ML methoods often don't run with NA's, so we check we have none
all(!is.na(LB_data_clean$Ofcom_Premise_Download_Speed))
all(!is.na(LB_data_clean$LATITUDE))
all(!is.na(LB_data_clean$LONGITUDE))
all(!is.na(LB_data_clean$F20_Percentile_WEIGHT_CURVE))

LB_numeric<-LB_data_clean%>%select(LATITUDE,LONGITUDE,Ofcom_Premise_Download_Speed,F20_Percentile_WEIGHT_CURVE)

############################################################
p1<-LB_numeric%>%
  ggplot( aes(x=Ofcom_Premise_Download_Speed)) +
  geom_histogram( color="blue", alpha=0.6, position = 'identity',bins=100)

p2<-LB_numeric%>%
  ggplot( aes(x=F20_Percentile_WEIGHT_CURVE)) +
  geom_histogram( color="red", alpha=0.6, position = 'identity',bins=100)

multiplot(p1,p2,cols=2)
############################################################

Num_Prems<-nrow(LB_numeric)
############################################################
set.seed(27)
#Bench- benchmarks performance
#Increase number of iterations
#Iter.max argument
#Different Kmeans methods
#Alternative types designed for high number of clusters?


Sys.time()


kclusts <-
  tibble(k = c(750))%>%
  mutate(
    kclust = map(k, ~kmeans(LB_numeric, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, LB_numeric)
  )

Sys.time()

clusters<-kclusts%>%
  unnest(cols=c(tidied))
assignments<-kclusts%>%
  unnest(c(augmented))
clusterings<-kclusts%>%
  unnest(cols=c(glanced))


ggplot(clusterings,aes(k,tot.withinss))+
  geom_line()+
  geom_point()

library(sf)
library(leaflet)
library(leafgl)
Data<-st_as_sf(LB_numeric, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addTiles(options = tileOptions(opacity = 0.8), group = "Open Street Map") %>%
  addGlPoints(data=Data,
              radius=3
  )
