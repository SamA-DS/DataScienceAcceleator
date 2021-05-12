library(dplyr)
library(ggplot2)
#rm(list=ls())
library(bduk)
source("~/bq_query.r")
source("~/multiplot.r")
options(scipen=20)

LB_data<-bdukBQ("SELECT 
CAST(UPRN AS NUMERIC) as UPRN,
POSTCODE,
LATITUDE,
LONGITUDE, 
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
Exchange_name
FROM 
`dcms-datalake-staging.APP_DATA.SiteSummary_CurrentSpine_3`
WHERE Local_Body in('Kent')")

LB_data%>%
  ggplot( aes(x=Ofcom_Premise_Download_Speed)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity')+
  facet_grid(Local_Body~.)

LB_data%>%
  ggplot( aes(x=F20_Percentile_WEIGHT_CURVE)) +
  geom_histogram()+
  facet_grid(Local_Body~.)

LB_data%>%
  select(POSTCODE,ONS_Rurality_Code,Local_Body)%>%
  group_by(POSTCODE)%>%
  ggplot( aes(x=,ONS_Rurality_Code)) +
  geom_bar()+
  facet_grid(Local_Body~.)

Kent<-LB_data%>%filter(Local_Body=="Kent")
Cheshire<-LB_data%>%filter(Local_Body=="Cheshire")
Shropshire<-LB_data%>%filter(Local_Body=="Shropshire")
East_Riding<-LB_data%>%filter(Local_Body=="East Riding of Yorkshire")
Cumbria<-LB_data%>%filter(Local_Body=="Cumbria")
# rm(LB_data)
p1<-Kent%>%
  ggplot( aes(x=Ofcom_Premise_Download_Speed)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity',bins=100)

p2<-Kent%>%
  ggplot( aes(x=F20_Percentile_WEIGHT_CURVE)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity',bins=100)

p3<-Kent%>%
  select(POSTCODE,ONS_Rurality_Code,Local_Body)%>%
  group_by(POSTCODE)%>%
  ggplot( aes(x=,ONS_Rurality_Code)) +
  geom_bar( color="#e9ecef", alpha=0.6, position = 'identity')

p4<-Kent%>%ggplot( aes(x=Ofcom_Area2_Flag)) + 
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity',bins=2)

p5<-Kent%>%ggplot( aes(x=Ofcom_Area3_Flag)) + 
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity',bins=2)

multiplot(p1,p2,p3,p4,p5,cols=2)

write.csv(LB_data,"Kent.csv")
