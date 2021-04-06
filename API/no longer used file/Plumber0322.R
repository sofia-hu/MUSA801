# plumber.R

#* Get the opa_account_num & parcel_id for an input address
#* @param addr Input address
#* @get /Parcel_Information
function(addr){
  #addr = "4412%20E%20WINGOHOCKING%20ST"
  
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
      opa_account_num <-  tidy_res$features$properties$opa_account_num[2]
    else
      opa_account_num <-  tidy_res$features$properties$opa_account_num[1]
    if(is.null(opa_account_num)){
      opa_account_num <- "OPA IS NULL"
    } else if(nchar(opa_account_num)==0) {
      opa_account_num <- "OPA IS ZERO LENGTH"
    }
  }else{
    opa_account_num <- "NONE FOUND"
  }
  
  #Parcel_Id---------------------------------------
  if (length(tidy_res) != 4){
    if(length(tidy_res$features$properties$dor_parcel_id)==2)
      Parcel_Id <-  tidy_res$features$properties$dor_parcel_id[2]
    else
      Parcel_Id <-  tidy_res$features$properties$dor_parcel_id[1]
    if(is.null(Parcel_Id)){
      Parcel_Id <- "PARCEL_ID IS NULL"
    } else if(nchar(Parcel_Id)==0) {
      Parcel_Id <- "0LENGTH"
    }
  }else{
    Parcel_Id <- "NONE FOUND"
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
  if(Parcel_Id != "0LENGTH" && Parcel_Id != "NONE FOUND" &&Parcel_Id != "PARCEL_ID IS NULL"){
    base <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/DOR_Parcel/FeatureServer/0/query?outFields=*&where=BASEREG%3D%27"
    BASEREG <- Parcel_Id
    end <- "%27&returnCentroid=true&f=pjson"
    dor_url <- paste(base, BASEREG, end, sep="")
    get_dor <- httr::GET(dor_url)
    dor_data <- httr::content(get_dor, simplifyVector=TRUE, as = "parsed", type = "application/json")
    Parcel_OBJECTID <- dor_data$features$attributes$OBJECTID
    PARCEL <- dor_data$features$attributes$PARCEL
    #Shape__Area <- dor_data$features$attributes$Shape__Area
    #Shape__Length <- dor_data$features$attributes$Shape__Length
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
                            Parcel_Id = c(Parcel_Id))%>%
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
      dplyr::select(Xmin, Ymin, Parcel_Id) %>% 
      st_as_sf(coords=c("Xmin","Ymin"),
               remove = FALSE,
               crs = 32618) %>% 
      st_transform(crs = 4326) %>%
      cbind(st_coordinates(.))
    
    #Upper-right
    UR <- DOR_meters %>% 
      st_drop_geometry() %>% 
      dplyr::select(Xmax, Ymax, Parcel_Id) %>% 
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
  prop_opa_account_num  <- paste0("%27",opa_account_num, "%27")
  prop_url <- paste(base_url, endpoint, query, prop_opa_account_num, sep="")
  response_prop <- httr::GET(prop_url)
  tidy_res_prop <- httr::content(response_prop, simplifyVector=TRUE)
  
  if (response_prop$status_code != 400){
    #total_area <-  tidy_res_prop$rows$total_area
    #total_livable_area <- tidy_res_prop$rows$total_livable_area
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
  
  #Request code violation--------------------------------
  base_url <- "https://phl.carto.com/api/v2/"
  endpoint <- "sql"
  query    <- c("?q=SELECT%20*%20FROM%20violations%20WHERE%20opa_account_num%20=%20")
  opa_account_num  <- paste0("%27",opa_account_num,"%27")
  url <- paste(base_url, endpoint, query, opa_account_num, sep="")
  response <- httr::GET(url)
  tidy_res <- httr::content(response, simplifyVector=TRUE)
  
  if (response$status_code != 400){
    if(length(tidy_res$rows$violationcode)==1){
      vio_code <-  tidy_res$rows$violationcode
      vio_title <- tidy_res$rows$violationcodetitle
      }else{
      vio_code <- "NO CODE VIOLATION"
      vio_title <- "NO CODE VIOLATION"
    }
  }
  else{
    vio_code <- "NO RESPONSE"
    vio_title <- "NO RESPONSE"
  }
  
  #Request 311 data(within 100m)----------------------------------
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
      Request311 <- tidy_res311$rows %>%
        data.frame() %>%
        dplyr::select(service_name, requested_datetime, address, lat, lon)
    }else{
      Request311 <- data.frame(Response=c("No 311 request within 100 meters in the last 15 days"))
    }
  }else{
    Request311 <- data.frame(Response=c("No 311 request is found because the location of this parcel is unknown"))
  }
  
  #Request 311 data(nn5)--------------------------------
  if(Centroid_x != "Null"){
    base311 = ("https://phl.carto.com/api/v2/sql?q=SELECT%20*%20FROM%20public_cases_fc%20WHERE%20")
    where = paste("requested_datetime%20%3E=%20%27",Sys.Date()-730,
                  "%27%20AND%20requested_datetime%20%3C%20%27", Sys.Date(),
                  "%27%20AND%20service_name%20IN%20(%27Alley%20Light%20Outage%27,%20%27No%20Heat%20(Residential)%27%20,%20%27Fire%20Residential%20or%20Commercial%27%20,%20%27Infestation%20Residential%27%20,%20%20%27Smoke%20Detector%27,%20%27Building%20Dangerous%27)",sep="")
    
    url311 <- paste(base311, where, sep="")
    
    response311 <- httr::GET(url311)
    tidy_res311 <- httr::content(response311, simplifyVector=TRUE)
    
    
    if(length(tidy_res311$rows) != 0){
      request311 <- tidy_res311$rows %>%
        data.frame() %>%
        dplyr::select(service_request_id, status, service_name, service_code, requested_datetime, updated_datetime, address, lat, lon)
    }else{
      request311 <- data.frame(Response=c("No relevant 311 request in the past year"))
    }
    
    request311.sf <- request311%>%
      drop_na(lat)%>%
      st_as_sf(coords = c("lon","lat"), crs = 4326)%>%
      st_transform(crs=3857)
    
    nn_function <- function(measureFrom,measureTo,k) {
      measureFrom_Matrix <- as.matrix(measureFrom)
      measureTo_Matrix <- as.matrix(measureTo)
      nn <-   
        get.knnx(measureTo, measureFrom, k)$nn.dist
      output <-
        as.data.frame(nn) %>%
        rownames_to_column(var = "thisPoint") %>%
        gather(points, point_distance, V1:ncol(.)) %>%
        arrange(as.numeric(thisPoint)) %>%
        group_by(thisPoint) %>%
        summarize(pointDistance = mean(point_distance)) %>%
        arrange(as.numeric(thisPoint)) %>% 
        dplyr::select(-thisPoint) %>%
        pull()
      
      return(output)  
    }
    
    light.sf<-request311.sf %>% 
      filter(service_name == 'Alley Light Outage')
    heat.sf<-request311.sf %>% 
      filter(service_name == 'No Heat (Residential)') 
    infestation.sf<- request311.sf %>%
      filter(service_name == 'Infestation Residential')
    Detector.sf<- request311.sf %>%
      filter(service_name == 'Smoke Detector')
    Dangerous.sf<- request311.sf %>%
      filter(service_name == 'Building Dangerous') 
    
    
    request311 <- data.frame(
             light.nn5 =  c(nn_function(st_coordinates(ParcelGeom),st_coordinates(light.sf), 5)),
             heat.nn5 = c(nn_function(st_coordinates(ParcelGeom),st_coordinates(heat.sf), 5)),
             infestation.nn5= c(nn_function(st_coordinates(ParcelGeom),st_coordinates(infestation.sf), 5)),
             Detector.nn5 = c(nn_function(st_coordinates(ParcelGeom),st_coordinates(Detector.sf), 5)),
             Dangerous.nn5 = c(nn_function(st_coordinates(ParcelGeom),st_coordinates(Dangerous.sf), 5))
      )
  }else{
    request311 <- data.frame(Response=c("No 311 request is found because the location of this parcel is unknown"))
  }
  
  #Request census data---------------------------------
  census_api_key("bdc91afe8f1e229bfd29314d696345b8365818b6", overwrite = TRUE)
  
  censusdat<- 
    get_acs(geography = "tract",
            variables = c(pop="B01003_001",
                          whitepop="B01001A_001",
                          medinc="B06011_001",
                          blackpop = "B02001_003"),
            year=2019,
            state = "PA", 
            geometry = TRUE, 
            county="Philadelphia",
            output = "wide")
  
  censusdat.sf <- censusdat %>%
    st_transform( crs = 3857)
  
  if(Centroid_x != "Null"){
    census <- 
      st_join(ParcelGeom, censusdat.sf,
              join=st_intersects,
              left = TRUE,
              largest = FALSE)
    
    population <- census$popE
    whitepop <- census$whitepopE
    blackpop <- census$blackpopE
    medianIncome <- census$medincE
  }else{
    population <- "unknown"
    whitepop <- "unknown"
    blackpop <- "unknown"
    medianIncome <- "unknown"
  }
  
  
  #Output-----------------------------------------------
  parcel_df <- 
    data.frame(Address = c(addr),               
             Opa_account_num = c(opa_account_num), 
             Parcel_Id= c(Parcel_Id)
             #Parcel_centroid_lat = c(x),
             #Parcel_centroid_lng = c(y)
             #Parcel_shape__Area = c(Shape__Area),
             #Parcel_Shape__Length = c(Shape__Length)
  )
  
  properties_df <-
    data.frame(#total_area = c(total_area),
               #total_livable_area = c(total_livable_area),
               zoning = c(zoning),
               category = c(category),
               interior = c(interior))
  
  violation_df <-
    data.frame(vio_code = c(vio_code),
               vio_title = c(vio_title))
  
  census_df <-
    data.frame(census_tract = c(census_tract),
               census_block = c(census_block),
               population = c(population),
               white_population = c(whitepop),
               black_population = c(blackpop),
               median_income = c(medianIncome)
               )
  
  
  res <- list(#status = "SUCCESS", code = "200", 
              parcel_df = parcel_df, properties_df= properties_df,
              violation_df = violation_df,
              census_df= census_df, request311 = Request311,
              request311.nn5= request311
              )
}