# plumber.R

#* A Fire Response Situational Awareness API
#* @param addr Input address
#* @get /parcel_info
#* @serializer json list(digits = 14)
function(addr){
  #addr = "1200%20W%20VENANGO%20ST"
  #addr = "7701%20%20LINDBERGH%20BLVD"
  #addr = "4054 1/2%20LANCASTER%20AV"
  #addr = "RandomInput"
  library(tidyverse)
  library(sf)
  library(geojsonsf)
  library(FNN)
  library(tidycensus)
  library(httr)
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
  opa_output <- opa_account_num
  
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
    } else if(nchar(Parcel_Id)>10) {
      id <- tidy_res$features$properties$dor_parcel_id
      Parcel_Id <- str_sub(id, 1, 10)
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
    base <-  "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/DOR_Parcel/FeatureServer/0/query?where=BASEREG%3D%27"
    ##base <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/DOR_Parcel/FeatureServer/0/query?outFields=*&where=BASEREG%3D%27"
    BASEREG <- Parcel_Id
    ##end <- "%27&returnCentroid=true&f=pjson"
    end <- "%27&objectIds=&time=&geometry=&geometryType=esriGeometryPolygon&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=false&quantizationParameters=&sqlFormat=none&f=pgeojson&token="
    dor_url <- paste(base, BASEREG, end, sep="")
    
    parcel.sf <- geojson_sf(dor_url)
    parcel3857 <- parcel.sf %>% 
      st_transform(crs = 3857) %>%
      dplyr::select(OBJECTID, geometry)
    
    parcel_centroid.sf <-parcel3857 %>%
      st_centroid()%>%
      cbind(st_coordinates(.))
    
    if(nrow(parcel.sf)==1){
      Centroid_x <- parcel_centroid.sf$X
      Centroid_y <- parcel_centroid.sf$Y
      
      Parcel_OBJECTID <- parcel.sf$OBJECTID
      PARCEL <- parcel.sf$PARCEL
      ADDR_SOURCE <- parcel.sf$ADDR_STD
    }else if(nrow(parcel.sf) > 1){
      Centroid_x <- parcel_centroid.sf$X[1]
      Centroid_y <- parcel_centroid.sf$Y[1]
      
      Parcel_OBJECTID <- parcel.sf$OBJECTID[1]
      PARCEL <- parcel.sf$PARCEL[1]
      ADDR_SOURCE <- parcel.sf$ADDR_STD[1]
    }else{
      Centroid_x <- "None Found"
      Centroid_y <- "None Found"
      
      Parcel_OBJECTID <- "None Found"
      PARCEL <- "None Found"
      ADDR_SOURCE <- "None Found"
    }

  }else{
    Parcel_OBJECTID <- "Null"
    PARCEL <- "Null"
    ADDR_SOURCE <- "Null"
    Centroid_x <- "Null"
    Centroid_y <- "Null"
    parcel.sf <- "Null"
  }
  
  
  if (is.null(Centroid_x) != TRUE && Centroid_x != "Null" && Centroid_x != "None Found"){
    ParcelGeom = data.frame(x0 = c(Centroid_x),
                            y0 = c(Centroid_y),
                            Parcel_OBJECTID = c(Parcel_OBJECTID),
                            Parcel_Id = c(Parcel_Id))%>%
      st_as_sf(coords = c("x0","y0"), crs = 3857)
    
    x0 = Centroid_x
    y0 = Centroid_y
    
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
    
    #Get Lat & Lng
    DOR_latlng <- DOR_meters %>% 
      st_drop_geometry() %>% 
      dplyr::select(X, Y, Parcel_Id) %>% 
      st_as_sf(coords=c("X","Y"),
               remove = FALSE,
               crs = 32618) %>% 
      st_transform(crs = 4326) %>%
      cbind(st_coordinates(.)) %>%
      rename(LNG = X.1, LAT = Y.1)
    
    LNG<- DOR_latlng$LNG
    LAT<- DOR_latlng$LAT
    
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
    x0 = "Null"
    y0 = "Null"
    LNG<- "Null"
    LAT<- "Null"
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
    exterior_condition <-  tidy_res_prop$rows$exterior_condition
    fireplaces <-  tidy_res_prop$rows$fireplaces
    frontage	 <-  tidy_res_prop$rows$frontage	
    fuel <-  tidy_res_prop$rows$fuel
    number_stories <- tidy_res_prop$rows$number_stories
    other_building <- tidy_res_prop$rows$other_building
    quality_grade <- tidy_res_prop$rows$quality_grade
    number_of_bathrooms <- tidy_res_prop$rows$number_of_bathrooms
    number_of_bedrooms <- tidy_res_prop$rows$number_of_bedrooms
    number_of_rooms <- tidy_res_prop$rows$number_of_rooms
    year_built<- tidy_res_prop$rows$year_built
    
    total_livable_area <- tidy_res_prop$rows$total_livable_area
    total_area <-  tidy_res_prop$rows$total_area
    ##is RMI/is CMX2
    zoning <- tidy_res_prop$rows$zoning
    isRM1<-ifelse(is.na(zoning)|zoning!='RM1',0,1)
    isCMX2<-ifelse(is.na(zoning)|zoning!='CMX2',0,1)
    category_code <- tidy_res_prop$rows$category_code
    ##is_com/is_hot
    category <- case_when(category_code == 1 ~ "Residential",
                          category_code == 2 ~ "Hotels and Apartments",
                          category_code == 3 ~ "Store with Dwelling",
                          category_code == 4 ~ "Commercial",
                          category_code == 5 ~ "Industrial",
                          category_code == 6 ~ "Vacant Land")
    iscom<-ifelse(category=='Commercial',1,0)
    ishotel<-ifelse(category=='Hotels and Apartments',1,0)
  
    ## is sealed/is below
    interior_condition <- tidy_res_prop$rows$interior_condition
    interior <- case_when(interior_condition == 0 ~ "Not Applicable",
                          interior_condition == 2 ~ "New/Rehabbed",
                          interior_condition == 3 ~ "Above Average",
                          interior_condition == 4 ~ "Average",
                          interior_condition == 5 ~ "Below Average",
                          interior_condition == 6 ~ "Vacant",
                          interior_condition == 7 ~ "Sealed/Structurally Compromised")
    issealed<-ifelse(is.na(interior)|interior!='Sealed / Structurally Compromised',0,1)
    isbelow<-ifelse(is.na(interior)|interior!='Below Average',0,1)
  }else{
    exterior_condition <-  "NO RESPONSE"
    fireplaces <-  "NO RESPONSE"
    frontage	 <-  "NO RESPONSE"
    fuel <-  "NO RESPONSE"
    number_stories <- "NO RESPONSE"
    other_building <- "NO RESPONSE"
    quality_grade <- "NO RESPONSE"
    number_of_bathrooms <- "NO RESPONSE"
    number_of_bedrooms <- "NO RESPONSE"
    number_of_rooms <- "NO RESPONSE"
    year_built<- "NO RESPONSE"
    
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
    if(length(tidy_res$rows$violationcode)>=1){
      violation <- tidy_res$rows%>%
        data.frame()%>%
        dplyr::select(violationcode, violationcodetitle,opa_account_num)
      ##unsafe&unequip variables
      violation <-mutate(violation, violation_summary = case_when(
        str_detect(violationcodetitle, "UNSAFE STRUCTURE") == TRUE | str_detect(violationcodetitle, "UNSAFE CONDITIONS") == TRUE | str_detect(violationcodetitle, "INTERIOR UNSAFE") == TRUE | str_detect(violationcodetitle, "VACANT PROP UNSAFE") == TRUE ~ "UNSAFE STRUCTURE",
        str_detect(violationcode, "FC-13") == TRUE | str_detect(violationcode, "FC-907.3") == TRUE | 
          violationcodetitle == "ELECTRICAL -FIRE DAMAGED"~ "PROBLEMS WITH FIRE EQUIPTEMENT",
        TRUE ~ "OTHERS"))
      ## violation count
      violation_count<-violation%>% 
        group_by(opa_account_num)%>%
        summarize(viol_count=n())
      violations_clean <- left_join(violation,violation_count, by='opa_account_num')
      ## related violation
      violation_count_related<-violation %>%
        filter(violation_summary == 'UNSAFE STRUCTURE'|violation_summary =='PROBLEMS WITH FIRE EQUIPTEMENT')%>%
        group_by(opa_account_num) %>%
        summarize(related_violation=n())
      violations_clean <- left_join(violations_clean,violation_count_related, by='opa_account_num')
      violations_clean$related_violation<-ifelse(!is.na(violations_clean$related_violation),
                                                 violations_clean$related_violation, 0)
      ##ext/int
      violations_clean$exterior<-ifelse(grepl("EXT", violations_clean$violationcodetitle), 1,0)
      violations_clean$interior<-ifelse(grepl("INT", violations_clean$violationcodetitle), 1,0)
      violations_summed <-
        violations_clean %>%
        group_by(opa_account_num) %>%
        summarise(had_extvio = sum(exterior),
                  had_intvio = sum(interior))
      violation_df<-left_join(violations_clean,violations_summed, by='opa_account_num')
      
      ##isunsafe/noequip
      violation_distinct <- violation_df %>% 
        distinct(opa_account_num,violationcodetitle, .keep_all = TRUE)
      violation_unsafe = violation_distinct %>%filter(violation_summary=="UNSAFE STRUCTURE")
      violation_equip = violation_distinct %>%filter(violation_summary=="PROBLEMS WITH FIRE EQUIPTEMENT")
      
      violation_unsafe <- violation_unsafe %>% 
        distinct(opa_account_num,violation_summary, .keep_all = TRUE)
      violation_equip <- violation_unsafe %>% 
        distinct(opa_account_num,violation_summary, .keep_all = TRUE)
      violation_unsafe_sub <- subset(violation_unsafe,select=c(violation_summary,opa_account_num))
      violation_unsafe_sub  <- rename(violation_unsafe_sub, vio_unsafe = violation_summary)
      violation_equip_sub <- subset(violation_equip,select=c(violation_summary,opa_account_num))
      violation_equip_sub <- rename(violation_equip_sub, vio_equip = violation_summary)
      
      violation_df <- left_join(violation_df,violation_unsafe_sub,by='opa_account_num')
      violation_df <- left_join(violation_df,violation_equip_sub,by='opa_account_num')
      violation_df$isunsafe<-ifelse(is.na(violation_df$vio_unsafe),0,1)
      violation_df$noequip<-ifelse(is.na(violation_df$vio_equip),0,1)
      }else{
      vio_code <- "NO CODE VIOLATION"
      vio_title <- "NO CODE VIOLATION"
      violation_df <-
        data.frame(vio_code = c(vio_code),
                   vio_title = c(vio_title))
    }
  }else{
    vio_code <- "NO RESPONSE"
    vio_title <- "NO RESPONSE"
    violation_df <-
      data.frame(vio_code = c(vio_code),
                 vio_title = c(vio_title))
  }
  
  #Request 311 data(within 100m)----------------------------------
  if(is.null(Centroid_x) != TRUE && Centroid_x != "Null" && Centroid_x != "None Found"){
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
  if(is.null(Centroid_x) != TRUE && Centroid_x != "Null" && Centroid_x != "None Found"){
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
  #census_api_key("bdc91afe8f1e229bfd29314d696345b8365818b6", overwrite = TRUE)
  
  #censusdat<- 
  #  get_acs(geography = "tract",
  #          variables = c(pop="B01003_001",
  #                        whitepop="B01001A_001",
  #                        medinc="B06011_001",
  #                        blackpop = "B02001_003"),
  #          year=2019,
  #          state = "PA", 
  #          geometry = TRUE, 
  #          county="Philadelphia",
  #          output = "wide")
  
  #censusdat.sf <- censusdat %>%
  #  st_transform( crs = 3857)
  
  #if(is.null(Centroid_x) != TRUE && Centroid_x != "Null" && Centroid_x != "None Found"){
  #  census <- 
  #    st_join(ParcelGeom, censusdat.sf,
  #            join=st_intersects,
  #            left = TRUE,
  #            largest = FALSE)
  
  #  population <- census$popE
  #  whitepop <- census$whitepopE
  #  blackpop <- census$blackpopE
  #  medianIncome <- census$medincE
  #}else{
  #  population <- "unknown"
  #  whitepop <- "unknown"
  #  blackpop <- "unknown"
  
  #  medianIncome <- "unknown"
  #}
  
  
  #Nearby Parcel---------------------------------------
  if(is.null(Centroid_x) != TRUE && Centroid_x != "Null" && Centroid_x != "None Found"){
    #Request nearby properties data (including opa_account_num & Parcel_Id)
    base_near_prop <- "https://phl.carto.com/api/v2/sql?q=SELECT%20*%20FROM%20opa_properties_public%20WHERE%20ST_DWithin(the_geom::geography,%20ST_GeographyFromText(%27POINT("
    end_near_prop <- ")%27),%2050)"
    near_prop_url <- paste(base_near_prop, LNG,"%20" ,LAT, end_near_prop, sep="")
  
    response_near_prop <- httr::GET(near_prop_url)
    tidy_res_near_prop <- httr::content(response_near_prop, simplifyVector=TRUE)
  }else{
    near_prop <- 
      data.frame(response = c("No nearby parcel is found because the location of this parcel is unknown"))     
  }
  
  if(is.null(Centroid_x) != TRUE && Centroid_x != "Null" && Centroid_x != "None Found" && length(tidy_res_near_prop$rows)!=0){
    near_prop <- tidy_res_near_prop$rows %>%
      data.frame() %>%
      dplyr::select(cartodb_id, parcel_number, registry_number, total_area, year_built, zoning, category_code, interior_condition) %>%
      mutate(category = case_when(category_code == 1 ~ "Residential",
                                  category_code == 2 ~ "Hotels and Apartments",
                                  category_code == 3 ~ "Store with Dwelling",
                                  category_code == 4 ~ "Commercial",
                                  category_code == 5 ~ "Industrial",
                                  category_code == 6 ~ "Vacant Land"),
             interior = case_when(interior_condition == 0 ~ "Not Applicable",
                                  interior_condition == 2 ~ "New/Rehabbed",
                                  interior_condition == 3 ~ "Above Average",
                                  interior_condition == 4 ~ "Average",
                                  interior_condition == 5 ~ "Below Average",
                                  interior_condition == 6 ~ "Vacant",
                                  interior_condition == 7 ~ "Sealed/Structurally Compromised",
                                  is.na(interior_condition) == TRUE ~ "None Found")) %>%
      rename(Parcel_Id = registry_number, opa_account_num = parcel_number) 
    
    #Request nearby parcels' geometry-----------------------
    for (i in 1:nrow(near_prop)) {
      if(near_prop$Parcel_Id[[i]] != "0LENGTH" && near_prop$Parcel_Id[[i]] != "NONE FOUND" &&near_prop$Parcel_Id[[i]] != "Parcel_Id IS NULL"){
        base <- "https://services.arcgis.com/fLeGjb7u4uXqeF9q/arcgis/rest/services/DOR_Parcel/FeatureServer/0/query?outFields=*&where=BASEREG%3D%27"
        BASEREG <- near_prop$Parcel_Id[[i]]
        end <- "%27&returnCentroid=true&f=pjson"
        dor_url <- paste(base, BASEREG, end, sep="")
        get_dor <- httr::GET(dor_url)
        dor_data <- httr::content(get_dor, simplifyVector=TRUE, as = "parsed", type = "application/json")
        if(length(dor_data$features$attributes$OBJECTID)==1){
        near_prop[i,"Parcel_OBJECTID"] <- dor_data$features$attributes$OBJECTID
        near_prop[i,"PARCEL"] <- dor_data$features$attributes$PARCEL
        #near_prop[i,"Shape__Area"] <- dor_data$features$attributes$Shape__Area
        #near_prop[i,"Shape__Length"] <- dor_data$features$attributes$Shape__Length
        near_prop[i,"ADDR_SOURCE"] <- dor_data$features$attributes$ADDR_SOURCE
        near_prop[i,"x"] <- dor_data$features$centroid$x
        near_prop[i,"y"] <- dor_data$features$centroid$y
        #near_prop[i,"geometry"] <- dor_data$features$geometry
        }else if(length(dor_data$features$attributes$OBJECTID)>1){
          near_prop[i,"Parcel_OBJECTID"] <- dor_data$features$attributes$OBJECTID[1]
          near_prop[i,"PARCEL"] <- dor_data$features$attributes$PARCEL[1]
          #near_prop[i,"Shape__Area"] <- dor_data$features$attributes$Shape__Area[1]
          #near_prop[i,"Shape__Length"] <- dor_data$features$attributes$Shape__Length[1]
          near_prop[i,"ADDR_SOURCE"] <- dor_data$features$attributes$ADDR_SOURCE[1]
          near_prop[i,"x"] <- dor_data$features$centroid$x[1]
          near_prop[i,"y"] <- dor_data$features$centroid$y[1]
          #near_prop[i,"geometry"] <- dor_data$features$geometry[1]
        }else{
          near_prop[i,"Parcel_OBJECTID"] <- "None Found"
          near_prop[i,"PARCEL"] <- "None Found"
          #near_prop[i,"Shape__Area"] <- "None Found"
          #near_prop[i,"Shape__Length"] <- "None Found"
          near_prop[i,"ADDR_SOURCE"] <- "None Found"
          near_prop[i,"x"] <- "None Found"
          near_prop[i,"y"] <- "None Found"
          #near_prop[i,"geometry"] <- dor_data$features$geometry[1]
        }
      }else{
        near_prop[i,"Parcel_OBJECTID"] <- "N/A"
        near_prop[i,"PARCEL"] <- "N/A"
        #near_prop[i,"Shape__Area"] <- "N/A"
        #near_prop[i,"Shape__Length"] <- "N/A"
        near_prop[i,"ADDR_SOURCE"] <- "N/A"
        near_prop[i,"x"] <- "N/A"
        near_prop[i,"y"] <- "N/A"
      }
      
     }
    
    near_prop <- near_prop[near_prop$y != "None Found" & near_prop$y != "N/A",]
    near_prop.sf <- near_prop%>%
      drop_na(x)%>%
      st_as_sf(coords = c("x","y"), crs = 3857)
    
    near_prop_latlng <- near_prop.sf %>%
      st_transform(crs = 4326) %>%
      cbind(st_coordinates(.)) %>%
      rename(LNG = X, LAT = Y)
    
    # Nearby L&I violation---------------------------------
    for (i in 1:nrow(near_prop_latlng)) {
      base_url <- "https://phl.carto.com/api/v2/"
      endpoint <- "sql"
      query    <- c("?q=SELECT%20*%20FROM%20violations%20WHERE%20opa_account_num%20=%20")
      opa_num  <- paste0("%27",near_prop_latlng$opa_account_num[[i]],"%27")
      url <- paste(base_url, endpoint, query, opa_num, sep="")
      response <- httr::GET(url)
      tidy_res <- httr::content(response, simplifyVector=TRUE)
      
      if (response$status_code != 400){
        if(length(tidy_res$rows$violationcode)==1){
          vio_code <-  tidy_res$rows$violationcode
          vio_title <- tidy_res$rows$violationcodetitle
          
          near_prop_latlng$vio_code[[i]] <- vio_code
          near_prop_latlng$vio_title[[i]] <- vio_title
        
          #cat("Address",i,vio_code, vio_title, "\n")
          }else{
          near_prop_latlng$vio_code[[i]] <- "NO CODE VIOLATION"
          near_prop_latlng$vio_title[[i]] <- "NO CODE VIOLATION"
          #cat("Address",i,"NO CODE VIOLATION\n")
        }
      }
      else{
        near_prop_latlng$vio_code[[i]] <- "NO RESPONSE"
        near_prop_latlng$vio_title[[i]] <- "NO RESPONSE"
        #cat("Address",i,"NO RESPONSE\n")
      }
    }
    
  }else{
    near_prop_latlng <- 
      data.frame(response = c("No parcel is found within 50 meters"))     
  }
  
  # Make Prediction-----------------------------------------
  #fire_model <- readr::read_rds("xxx.rds")
  
  
  #Output-----------------------------------------------
  parcel_df <- 
    data.frame(Input = c(addr),
             Opa_account_num = c(opa_output), 
             Parcel_Id= c(Parcel_Id),
             ADDR_SOURCE = c(ADDR_SOURCE),
             PARCEL = c(PARCEL),
             Parcel_centroid_lat = c(LAT),
             Parcel_centroid_lng = c(LNG),
             X = c(x0),
             Y = c(y0)
  )
  
  properties_df <-
    data.frame(total_area = c(total_area),
               total_livable_area = c(total_livable_area),
               zoning = c(zoning),
               category = c(category),
               interior = c(interior),
               exterior_condition = c(exterior_condition),
               fireplaces =  c(fireplaces),
               frontage	 =  c(frontage),
               fuel =  c(fuel),
               number_stories = c(number_stories),
               other_building = c(other_building),
               quality_grade = c(quality_grade),
               number_of_bathrooms = c(number_of_bathrooms),
               number_of_bedrooms = c(number_of_bedrooms),
               number_of_rooms = c(number_of_rooms),
               year_built = c(year_built),
               isRM1 = c(isRM1),
               isCMX2 = c(isCMX2),
               iscom = c(iscom),
               ishotel = c(ishotel),
               issealed = c(issealed),
               isbelow = c(isbelow))

  
  "violation_df <-
    data.frame(vio_code = c(vio_code),
               vio_title = c(vio_title))"
  
  #census_df <-
  #  data.frame(census_tract = c(census_tract),
  #             census_block = c(census_block),
  #             population = c(population),
  #             white_population = c(whitepop),
  #             black_population = c(blackpop),
  #             median_income = c(medianIncome)
  #             )
  
  
  res <- list(#status = "SUCCESS", code = "200", 
              parcel_df = parcel_df, 
              parcel_geometry = parcel.sf,
              properties_df= properties_df,
              violation_df = violation_df,
              #census_df= census_df, 
              request311_within100m = Request311,
              request311.nn5= request311,
              nearby_parcel_df = near_prop_latlng
              )
}