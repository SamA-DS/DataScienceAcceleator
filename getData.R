
test<-getData("Kent")

getData<-function(Local_Body){
  
  if(exists("Tree")){
    Tree<-Tree
  } else if(file.exists("Tree.csv")){
    Tree<-readr::read_csv(here::here("Tree.csv"))
  } else if(file.exists("Row_ID_UPRN_lookup.csv") & file.exists("NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE_Predecessors.csv")){
    UPRN_lookup<-read_csv(here::here("Row_ID_UPRN_lookup.csv"))
    
    Node_UPRNs<-UPRN_lookup%>%
      rename(Node_ID=Row_ID, 
             Node_UPRN=UPRN)
    Parent_UPRNS<-UPRN_lookup%>%
      rename(Parent_ID=Row_ID,
             Parent_UPRN=UPRN)
    
    Tree<-read_csv(here::here("NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE_Predecessors.csv"))%>%
      rename(Node_ID=Row_ID,
             Parent_ID=Predecessors_NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE,
             Num_Nodes_to_Hub=Predecessors_NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE,
             Dist_to_Hub=PredecessorDistance_NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE
             )%>%
      left_join(Node_UPRNs)%>%
      left_join(Parent_UPRNs)%>%
      select(Node_UPRN,Parent_UPRN,Num_Nodes_to_Hub,Dist_to_Hub)%>%
      mutate_all(as.numeric)
  } else {
    Query<-("
  SELECT
    Node_UPRN,
    Parent_UPRN,
    PREDECESSORS_NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE as Num_Nodes_to_Hub,
    PredecessorDistance_NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE as Dist_to_Hub
  FROM
    `dcms-datalake-staging.APP_DATA.NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE_Predecessors` as Parents
  LEFT JOIN
  (SELECT
      Row_ID,
      UPRN as Node_UPRN
  FROM
    `dcms-datalake-staging.APP_DATA.Row_ID_UPRN_lookup`) as Node_UPRNs on Parents.Row_ID=Node_UPRNs.Row_ID
  LEFT JOIN
  (SELECT
      Row_ID,
      UPRN as Parent_UPRN
  FROM
    `dcms-datalake-staging.APP_DATA.Row_ID_UPRN_lookup`) as Parent_UPRNs on Parents.PREDECESSOR_NEAREST_PCP_IF_FIBRE_OTHERWISE_PARENT_EXCHANGE=Parent_UPRNs.Row_ID
 ")%>%
      mutate_all(as.numeric)
    
    bigrquery::bq_auth(path = "dcms-datalake-staging_bigquery.json")
    billing <- "dcms-datalake-staging"
    bq_object <- function(billing, sql){
      tb <- bigrquery::bq_project_query(billing, sql)
      OBJ <- bigrquery::bq_table_download(tb)
    }
    Tree<-bq_object(billing,Query)
  }
  
  if(!file.exists("Tree.csv")){
    readr::write_csv(Tree,here::here("Tree.csv"))
  }
  
  if(file.exists(paste0(Local_Body,".csv"))){
    LB_data<-readr::read_csv(here::here(paste0(Local_Body,".csv")))
  } else {
    
    Query<-paste0("
  SELECT
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
  WHERE Local_Body in ('", Local_Body,"')")
    
    LB_data<-bq_object(billing,Query)
  }
  
  if(!file.exists(paste0(Local_Body,".csv"))){
    write_csv(LB_data,here::here(paste0(Local_Body,".csv")))
  }
  
  Node_Data<-LB_data%>%
    select(UPRN, POSTCODE, Local_Body, LATITUDE, LONGITUDE, F20_Percentile_WEIGHT_CURVE, Ofcom_Premise_Download_Speed)%>%
    rename(Node_UPRN=UPRN, 
           Node_Latitude=LATITUDE, 
           Node_Lonigutde=LONGITUDE, 
           Node_F20_Percentile=F20_Percentile_WEIGHT_CURVE, 
           Node_Speed=Ofcom_Premise_Download_Speed)
  
  Parent_Data<-LB_data%>%
    select(UPRN, LATITUDE, LONGITUDE, F20_Percentile_WEIGHT_CURVE, Ofcom_Premise_Download_Speed)%>%
    rename(Parent_UPRN=UPRN, 
           Parent_Latitude=LATITUDE, 
           Parent_Longitude=LONGITUDE, 
           Parent_F20_Percentile=F20_Percentile_WEIGHT_CURVE, 
           Parent_Speed=Ofcom_Premise_Download_Speed)
  
  Tree_Data<-Node_Data%>%
    left_join(Tree)
  
  Tree_data<-Tree%>%
    right_join(Node_Data, by=c("Node_UPRN"="Node_UPRN"))%>%
    right_join(Parent_Data, by=c("Parent_UPRN"="Parent_UPRN"))
  
  write_csv(Tree_data,"Tree_data.csv")
  
  return(Tree_data)
}

