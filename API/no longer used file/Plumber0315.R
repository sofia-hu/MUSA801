# plumber.R

#* Get the opa_account_num & parcel_id for an input address
#* @param addr Input address
#* @get /Parcel_Information
function(addr){
  addr = "4412%20E%20WINGOHOCKING%20ST"
  
  library(tidyverse)
  library(sf)
  library(geojsonsf)
  library(QuantPsyc)
  library(RSocrata)
  library(caret)
  library(spatstat)
  library(spdep)
  library(FNN)
  library(grid)
  library(gridExtra)
  library(kableExtra)
  library(tidycensus)

  library(httr)
  library(dplyr)
  library(stringr)
  
  # Request opa -------------------------------------------------
  base_url <- "http://api.phila.gov/ais/v1/"
  endpoint <- "search/"
  key      <- "?gatekeeperKey=dc953bbc4ade9d00eabf5409f6d73d3e"
  url <- paste(base_url, endpoint, as.character(addr), key, sep="")
  response <- httr::GET(url)
  tidy_res <- httr::content(response, simplifyVector=TRUE)
  if (length(tidy_res) != 4){
    if(length(tidy_res$features$properties$opa_account_num)==2)
      opa_num <-  tidy_res$features$properties$opa_account_num[2]
    else
      opa_num <-  tidy_res$features$properties$opa_account_num[1]
    if(is.null(opa_num)){
      opa_num <- "OPA IS NULL"
    } else if(nchar(opa_num)==0) {
      opa_num <- "OPA IS ZERO LENGTH"
    }
  }else{
    opa_num <- "NONE FOUND"
  }
  
  #parcel_id---------------------------------------
  if (length(tidy_res) != 4){
    if(length(tidy_res$features$properties$dor_parcel_id)==2)
      parcel_id <-  tidy_res$features$properties$dor_parcel_id[2]
    else
      parcel_id <-  tidy_res$features$properties$dor_parcel_id[1]
    if(is.null(parcel_id)){
      parcel_id <- "PARCEL_ID IS NULL"
    } else if(nchar(parcel_id)==0) {
      parcel_id <- "0LENGTH"
    }
    #cat("Address",addr,"parcel_id#:",parcel_id,"\n")
    #print(parcel_id)
  }else{
    #cat("Address",addr,"NO ADDRESS FOUND!","\n")
    #print("NO ADDRESS FOUND!")
    parcel_id <- "NONE FOUND"
  }
  
  #Census_tract&block_num
  if(length(tidy_res) != 4){
    if(length(tidy_res$features$properties$census_tract_2010)==2){
      census_tract <-  tidy_res$features$properties$census_tract_2010[2]}
    else{
      census_tract <- tidy_res$features$properties$census_tract_2010
    }
    if(length(tidy_res$features$properties$census_block_2010)==2){
      census_block <-  tidy_res$features$properties$census_block_2010[2]}
    else{
      census_block <- tidy_res$features$properties$census_block_2010
    }
    census_tract <- as.character(census_tract)
    census_block <- as.character(census_block)
  }else{
    census_tract <- "NONE FOUND"
    census_block <- "NONE FOUND"
  }
  
  #Request parcel geometry------------------------------------------ 
  if(parcel_id != "0LENGTH" && parcel_id != "NONE FOUND" &&parcel_id != "PARCEL_ID IS NULL"){
    base <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/DOR_Parcel/FeatureServer/0/query?outFields=*&where=BASEREG%3D%27"
    BASEREG <- parcel_id
    end <- "%27&returnCentroid=true&f=pjson"
    dor_url <- paste(base, BASEREG, end, sep="")
    get_dor <- httr::GET(dor_url)
    dor_data <- httr::content(get_dor, simplifyVector=TRUE, as = "parsed", type = "application/json")
    Parcel_OBJECTID <- dor_data$features$attributes$OBJECTID
    PARCEL <- dor_data$features$attributes$PARCEL
    Shape__Area <- dor_data$features$attributes$Shape__Area
    Shape__Length <- dor_data$features$attributes$Shape__Length
    ADDR_SOURCE <- dor_data$features$attributes$ADDR_SOURCE
    Centroid_x <- dor_data$features$centroid$x
    Centroid_y <- dor_data$features$centroid$y
  }else{
    Parcel_OBJECTID <- "Null"
    PARCEL <- "Null"
    Shape__Area <- "Null"
    Shape__Length <- "Null"
    ADDR_SOURCE <- "Null"
    Centroid_x <- "Null"
    Centroid_y <- "Null"
  }
  
  
  if (Centroid_x != "Null"){
    ParcelGeom = data.frame(x = c(Centroid_x),
                            y = c(Centroid_y),
                            Parcel_OBJECTID = c(Parcel_OBJECTID),
                            parcel_id = c(parcel_id))%>%
      st_as_sf(coords = c("x","y"), crs = 3857)
    
    DOR_4326 <- ParcelGeom %>% 
      st_transform(crs = 4326)
    
    distance <- 100
    DOR_meters <- DOR_4326 %>%  
      st_transform(32618) %>% 
      cbind(st_coordinates(.)) %>% 
      mutate(Xmin = X - distance,
             Xmax = X + distance,
             Ymin = Y - distance,
             Ymax = Y + distance) 
    
    #Lower-left
    LL <- DOR_meters %>% 
      st_drop_geometry() %>% 
      dplyr::select(Xmin, Ymin, parcel_id) %>% 
      st_as_sf(coords=c("Xmin","Ymin"),
               remove = FALSE,
               crs = 32618) %>% 
      st_transform(crs = 4326) %>%
      cbind(st_coordinates(.))
    
    #Upper-right
    UR <- DOR_meters %>% 
      st_drop_geometry() %>% 
      dplyr::select(Xmax, Ymax, parcel_id) %>% 
      st_as_sf(coords=c("Xmax","Ymax"),
               remove = FALSE,
               crs = 32618)%>% 
      st_transform(crs = 4326) %>%
      cbind(st_coordinates(.))
  }else{
    x = "Null"
    y = "Null"
  }
  
  #Request properties data----------------------------------
  base_url <- "https://phl.carto.com/api/v2/"
  endpoint <- "sql"
  query    <- c("?q=SELECT%20*%20FROM%20opa_properties_public%20WHERE%20parcel_number%20=%20")
  prop_opa_num  <- paste0("%27",opa_num, "%27")
  prop_url <- paste(base_url, endpoint, query, prop_opa_num, sep="")
  response_prop <- httr::GET(prop_url)
  tidy_res_prop <- httr::content(response_prop, simplifyVector=TRUE)
  
  if (response_prop$status_code != 400){
    total_area <-  tidy_res_prop$rows$total_area
    total_livable_area <- tidy_res_prop$rows$total_livable_area
    zoning <- tidy_res_prop$rows$zoning
    category_code <- tidy_res_prop$rows$category_code
    category <- case_when(category_code == 1 ~ "Residential",
                          category_code == 2 ~ "Hotels and Apartments",
                          category_code == 3 ~ "Store with Dwelling",
                          category_code == 4 ~ "Commercial",
                          category_code == 5 ~ "Industrial",
                          category_code == 6 ~ "Vacant Land")
    interior_condition <- tidy_res_prop$rows$interior_condition
    interior <- case_when(interior_condition == 0 ~ "Not Applicable",
                          interior_condition == 2 ~ "New/Rehabbed",
                          interior_condition == 3 ~ "Above Average",
                          interior_condition == 4 ~ "Average",
                          interior_condition == 5 ~ "Below Average",
                          interior_condition == 6 ~ "Vacant",
                          interior_condition == 7 ~ "Sealed/Structurally Compromised")
  }else{
    total_area <- "NO RESPONSE"
    total_livable_area <- "NO RESPONSE"
    zoning <- "NO RESPONSE"
    category <- "NO RESPONSE"
    interior <- "NO RESPONSE"
  }
  
  #Request 311 data----------------------------------
  if(Centroid_x != "Null"){
    base311 = ("https://phl.carto.com/api/v2/sql?q=SELECT%20*%20FROM%20public_cases_fc%20WHERE%20")
    where1 = paste("requested_datetime%20%3e%3d%20%27",Sys.Date()-30,
                   "%27%20AND%20requested_datetime%20%3c%20%27", Sys.Date(),
                   "%27%20AND%20lat%20%3C%20",sep="")
    where2 = "AND%20lat%20%3E%20"
    where3 = "AND%20lon%20%3C%20"
    where4 = "AND%20lon%20%3E%20"
    
    LATmax = UR$Y
    LATmin = LL$Y
    LNGmax = UR$X
    LNGmin = LL$X
    
    url311 <- paste(base311, where1, LATmax, where2, LATmin, where3, LNGmax, where4, LNGmin, sep="")
    
    
    response311 <- httr::GET(url311)
    tidy_res311 <- httr::content(response311, simplifyVector=TRUE)
  
  
    if(length(tidy_res311$rows) != 0){
      request311 <- tidy_res311$rows %>%
        data.frame() %>%
        dplyr::select(service_request_id, status, service_name, service_code, requested_datetime, updated_datetime, address, lat, lon)
    }else{
      request311 <- data.frame(Response=c("No 311 request within 100 meters in the last 15 days"))
    }
  }else{
    request311 <- data.frame(Response=c("No 311 request is found because the location of this parcel is unknown"))
  }
  
  
  
  
  #Output-----------------------------------------------
  parcel_df <- 
    data.frame(Address = c(addr),               
             Opa_account_num = c(opa_num), 
             Parcel_id= c(parcel_id),
             Parcel_centroid_lat = c(x),
             Parcel_centroid_lng = c(y),
             Parcel_shape__Area = c(Shape__Area),
             Parcel_Shape__Length = c(Shape__Length)
  )
  
  properties_df <-
    data.frame(total_area = c(total_area),
               total_livable_area = c(total_livable_area),
               zoning = c(zoning),
               category = c(category),
               interior = c(interior))
  
  census_df <-
    data.frame(census_tract = c(census_tract),
               census_block = c(census_block))
  
  
  res <- list(status = "SUCCESS", code = "200", 
              parcel_df = parcel_df, properties_df= properties_df,
              census_df= census_df, request311= request311
              )
}