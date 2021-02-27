Pre1 - EDA
================

## Use Case

Our tool targets the fire department and residents.

The Philadelphia Fire Department responds to hundreds or even thousands
of locations everyday to quell an array of emergencies. Currently, they
have no ‘situational awareness’ of fire risk for a given location when
an emergency call comes in. Therefore, we are going to help them create
such a tool, by providing a parcel-level (building) fire risk score
prediction for each property in the City;

![Ghost\_ship\_warehouse\_interior](1440px-Ghost_ship_warehouse_interior.jpg)

In addition, we want to let residents get a real-time update of the fire
risk of their houses so they will have a situational awareness on risk
for each property citywide.

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.0 ─

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.6     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.4, PROJ 6.3.1

``` r
library(QuantPsyc)
```

    ## Loading required package: boot

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## 
    ## Attaching package: 'QuantPsyc'

    ## The following object is masked from 'package:base':
    ## 
    ##     norm

``` r
library(RSocrata)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'lattice'

    ## The following object is masked from 'package:boot':
    ## 
    ##     melanoma

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(spatstat)
```

    ## Loading required package: spatstat.data

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## Loading required package: rpart

    ## Registered S3 method overwritten by 'spatstat':
    ##   method     from
    ##   print.boxx cli

    ## 
    ## spatstat 1.64-1       (nickname: 'Help you I can, yes!') 
    ## For an introduction to spatstat, type 'beginner'

    ## 
    ## Note: spatstat version 1.64-1 is out of date by more than 9 months; we recommend upgrading to the latest version.

    ## 
    ## Attaching package: 'spatstat'

    ## The following object is masked from 'package:lattice':
    ## 
    ##     panel.histogram

    ## The following object is masked from 'package:MASS':
    ## 
    ##     area

    ## The following object is masked from 'package:boot':
    ## 
    ##     envelope

``` r
library(spdep)
```

    ## Loading required package: sp

    ## Loading required package: spData

    ## To access larger datasets in this package, install the spDataLarge
    ## package with: `install.packages('spDataLarge',
    ## repos='https://nowosad.github.io/drat/', type='source')`

``` r
library(FNN)
library(grid)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(knitr)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(tidycensus)

library(mapview)
```

    ## GDAL version >= 3.1.0 | setting mapviewOptions(fgb = TRUE)

``` r
library(httr)
```

    ## 
    ## Attaching package: 'httr'

    ## The following object is masked from 'package:caret':
    ## 
    ##     progress

``` r
library(dplyr)
library(readxl)
library(stringr)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

#Nearest neighbor (NND) function
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
```

``` r
#Load data------------------------------------------------------------------
library(readxl)
fire <- read_excel("./data/fire.xlsx")
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J2144 / R2144C10: got 'N'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K2929 / R2929C11: got 'Tioga Marine Terminal'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K3678 / R3678C11: got '6543 WOODSTOCK ST.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K4214 / R4214C11: got 'Bldg. 8'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K4362 / R4362C11: got 'AKA 5550 Whitaker Ave.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J4593 / R4593C10: got 'S'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K5264 / R5264C11: got 'Bldg. D'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K5499 / R5499C11: got '6525 N. 6th St'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J6421 / R6421C10: got 'S'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K8402 / R8402C11: got '6817 HORROCKS'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K9521 / R9521C11: got 'BLDG A'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J9591 / R9591C10: got 'S'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K10826 / R10826C11: got '4711 Cooper St.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K11463 / R11463C11: got 'Levin Bldg'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K12803 / R12803C11: got '5511 Hazel Av'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K13583 / R13583C11: got 'Super Clean At Erie'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K13972 / R13972C11: got 'Building CE'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K14469 / R14469C11: got 'Building C'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J14713 / R14713C10: got 'S'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J15966 / R15966C10: got 'S'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K19588 / R19588C11: got '6522 Vine Street'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K20186 / R20186C11: got '3200 TULIP ST'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K21224 / R21224C11: got '1121 N 66th Street'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K21257 / R21257C11: got 'BLDG "O"'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K21385 / R21385C11: got 'Building 67'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in J22225 / R22225C10: got 'S'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K23578 / R23578C11: got '6522 Vine Street'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K23600 / R23600C11: got '2012 Sanford St, (Address of
    ## Fire)'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K26414 / R26414C11: got '3200 TULIP ST'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K28259 / R28259C11: got 'Glicks Rib Shack'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K30488 / R30488C11: got '531-51 E WASHINGTON LN'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K30709 / R30709C11: got '10149 VERREE'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K31503 / R31503C11: got '1837 Dudley St.'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K31757 / R31757C11: got '6522 Vine Street'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K32275 / R32275C11: got 'McDonalds'

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting logical in K32597 / R32597C11: got '1400 N. Broad St'

``` r
library(stringr)

fire1 = fire %>%
  filter(addr_type ==1)
fire1$address <- paste(ifelse(is.na(fire1$number)==FALSE,fire1$number,''),
                       "%20",
                       ifelse(is.na(fire1$st_prefix)==FALSE,fire1$st_prefix,''),
                       "%20",
                       ifelse(is.na(fire1$street)==FALSE,fire1$street,''),
                       "%20",
                       ifelse(is.na(fire1$st_type)==FALSE,fire1$st_type,''), sep = "")
fire2 = fire %>%
  filter(addr_type ==2)
fire2$address <- paste(ifelse(is.na(fire2$xst_prefix)==FALSE,fire2$xst_prefix,''),
                       ifelse(fire2$xst_prefix!='',"%20",''),
                       ifelse(is.na(fire2$xstreet)==FALSE,fire2$xstreet,''),
                       "%20",
                       ifelse(is.na(fire2$xst_type)==FALSE,fire2$xst_type,''),
                       "%20",
                       "&",
                       "%20",
                       ifelse(is.na(fire2$st_prefix)==FALSE,fire2$st_prefix,''), 
                       ifelse(fire2$st_prefix!='',"%20",''),
                       ifelse(is.na(fire2$street)==FALSE,fire2$street,''), 
                       "%20",
                       ifelse(is.na(fire2$st_type)==FALSE,fire2$st_type,''),
                       sep = "")  
fireData <- rbind(fire1, fire2)

fireData$MUSA_ID <- paste0("MUSA_",1:nrow(fireData))
```

## Building Features

We looked into all the properties in philadelphia to examine whether
there is a higher percentage of fire occurrence in the properties with
certain features.

OPA & Properties

``` r
#OPA
opa <- read_csv("./data/Fire_OPA_Parcel.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## ─ Column specification ────────────────────────────
    ## cols(
    ##   X1 = col_double(),
    ##   OPA_Num = col_character(),
    ##   Parcel_Id = col_character(),
    ##   MUSA_ID = col_character()
    ## )

``` r
opa <- opa %>%
  mutate(Parcel_Id = ifelse(Parcel_Id=="parcel_id IS ZERO LENGTH","0Length",Parcel_Id))%>%
  mutate(Parcel_Id = ifelse(str_count(Parcel_Id)==10,Parcel_Id,substr(Parcel_Id,1,10)))


fireData <- merge(fireData, opa, by = "MUSA_ID")
names(fireData)[names(fireData) =="OPA_Num"] <-"opa_account_num"

property <- read_csv("./data/opa_properties_public.csv")
```

    ## 
    ## ─ Column specification ────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   assessment_date = col_datetime(format = ""),
    ##   category_code = col_double(),
    ##   date_exterior_condition = col_datetime(format = ""),
    ##   depth = col_double(),
    ##   exempt_building = col_double(),
    ##   exempt_land = col_double(),
    ##   exterior_condition = col_double(),
    ##   fireplaces = col_double(),
    ##   frontage = col_double(),
    ##   garage_spaces = col_double(),
    ##   homestead_exemption = col_double(),
    ##   interior_condition = col_double(),
    ##   market_value = col_double(),
    ##   market_value_date = col_logical(),
    ##   number_of_bathrooms = col_double(),
    ##   number_of_bedrooms = col_double(),
    ##   number_of_rooms = col_double(),
    ##   number_stories = col_double(),
    ##   off_street_open = col_double(),
    ##   other_building = col_logical()
    ##   # ... with 14 more columns
    ## )
    ## ℹ Use `spec()` for the full column specifications.

    ## Warning: 4217 parsing failures.
    ##  row            col           expected actual                               file
    ## 2409 other_building 1/0/T/F/TRUE/FALSE      N './data/opa_properties_public.csv'
    ## 3391 other_building 1/0/T/F/TRUE/FALSE      N './data/opa_properties_public.csv'
    ## 4876 other_building 1/0/T/F/TRUE/FALSE      N './data/opa_properties_public.csv'
    ## 7070 unfinished     1/0/T/F/TRUE/FALSE      U './data/opa_properties_public.csv'
    ## 7071 unfinished     1/0/T/F/TRUE/FALSE      U './data/opa_properties_public.csv'
    ## .... .............. .................. ...... ..................................
    ## See problems(...) for more details.

``` r
parcel0 <- st_read("./data/DOR_Parcel/DOR_Parcel.shp")
```

    ## Reading layer `DOR_Parcel' from data source `/Users/tangtown/Documents/MUSA 801/data/DOR_Parcel/DOR_Parcel.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 604888 features and 40 fields (with 9 geometries empty)
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -75.27776 ymin: 39.87055 xmax: -74.95569 ymax: 40.13786
    ## geographic CRS: WGS 84

``` r
parcel <- parcel0 %>%
  dplyr::distinct(BASEREG, MAPREG)

names(parcel)[names(parcel) =="BASEREG"] <-"registry_number"

parcel.sf <- parcel %>% 
  st_as_sf(coords = st_centroid(parcel.sf), crs = 4326) %>%
  st_transform('ESRI:102729')
```

fireData + property

``` r
names(property)[names(property) =="parcel_number"] <-"opa_account_num"

property_fire <- property %>%
  left_join(fireData, by="opa_account_num")
```

fireData + property + parcel

``` r
prop_fire_parcel <- parcel %>%
  left_join(property_fire, by="registry_number")

prop_fire_parcel1 <- property_fire  %>%
  left_join(parcel, by="registry_number")
```

``` r
#property <- rename(property, OPA_Num = parcel_number)
fire_opa <- left_join(fireData,opa,by='MUSA_ID')
property_fire <- left_join(property,fire_opa,by='opa_account_num')

##Feature engineering
fire_property_trim <-mutate(property_fire, category = case_when(
  category_code == 1  ~ "Residential",
  category_code == 2  ~ "Hotels and Apartments",
  category_code == 3  ~ "Store with Dwelling",
  category_code == 4  ~ "Commercial",
  category_code == 5  ~ "Industrial",
  category_code == 6  ~ "Vacant Land"))
fire_property_trim <-mutate(fire_property_trim, interior = case_when(
  interior_condition == 0  ~ "Not Applicable",
  interior_condition == 2  ~ "New/Rehabbed",
  interior_condition == 3  ~ "Above Average",
  interior_condition == 4  ~ "Average",
  interior_condition == 5  ~ "Below Average",
  interior_condition == 6  ~ "Vacant",
  interior_condition == 7  ~ "Sealed / Structurally Compromised"))
fire_property_trim$year_built <- as.numeric(as.character(fire_property_trim$year_built))
```

    ## Warning: 强制改变过程中产生了NA

``` r
fire_property_trim <-mutate(fire_property_trim, year_built_cat = case_when(
  year_built >= 1600 & year_built < 1800 ~ "1600-1800",
  year_built >= 1800 & year_built < 1900 ~ "1800s",
  year_built >= 1900 & year_built < 2000  ~ "1900s",
  year_built >= 2000 & year_built < 2010  ~ "2000-2010",
  year_built >= 2010 & year_built < 2020  ~ "2010-2020",
  year_built < 1000 | year_built >2020 ~ "Unknown"))
fire_property_trim <-mutate(fire_property_trim, isfire = case_when(
  is.na(inci_no) == FALSE  ~ "Have fire",
  is.na(inci_no) == TRUE  ~ "No fire"))
```

``` r
###EDA plot
#category
fire_property_trim  %>% 
  ggplot(aes(x = category, fill = isfire)) + 
  geom_bar(position = position_fill()) + 
  theme_classic() + 
  labs(y = 'Percent') + 
  coord_flip() #rotate the axis
```

![](Pre1_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#zoning
fire_property_trim  %>% 
  ggplot(aes(x = zoning, fill = isfire)) + 
  geom_bar(position = position_fill()) + 
  theme_classic() + 
  labs(y = 'Percent') + 
  coord_flip() 
```

![](Pre1_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
#interior
fire_property_trim  %>% 
  ggplot(aes(x = interior, fill = isfire)) + 
  geom_bar(position = position_fill()) + 
  theme_classic() + 
  labs(y = 'Percent') + 
  coord_flip() 
```

![](Pre1_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
#year build
fire_property_trim  %>% 
  ggplot(aes(x = year_built_cat, fill = isfire)) + 
  geom_bar(position = position_fill()) + 
  theme_classic() + 
  labs(y = 'Percent') + 
  coord_flip() 
```

![](Pre1_files/figure-gfm/unnamed-chunk-7-4.png)<!-- --> As to property
category, there is a higher percentage of fire occurrence in commercial,
hotels and apartments properties.

As to different zoning type. There is significantly higher propertion of
fire occurred in the buildings of SPENT category.

Slightly more fire occurred in the properties which are sealed or
structurally compromised.

There is no significant difference for fire occurrence in properties
with different age.

## Environment Factors

(Haoheng’s text)

## Correlation

### Code violation

``` r
violation <- read_csv("./data/violations 2019-2020.csv")
```

    ## 
    ## ─ Column specification ────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   objectid = col_double(),
    ##   addressobjectid = col_double(),
    ##   parcel_id_num = col_logical(),
    ##   casenumber = col_double(),
    ##   casecreateddate = col_datetime(format = ""),
    ##   casecompleteddate = col_datetime(format = ""),
    ##   casetype = col_logical(),
    ##   violationnumber = col_double(),
    ##   violationdate = col_datetime(format = ""),
    ##   violationresolutiondate = col_logical(),
    ##   violationresolutioncode = col_logical(),
    ##   mostrecentinvestigation = col_datetime(format = ""),
    ##   unit_type = col_logical(),
    ##   unit_num = col_logical(),
    ##   censustract = col_double(),
    ##   geocode_x = col_double(),
    ##   geocode_y = col_double(),
    ##   lat = col_double(),
    ##   lng = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

    ## Warning: 394239 parsing failures.
    ##  row                     col           expected              actual                              file
    ## 1238 unit_num                1/0/T/F/TRUE/FALSE A                   './data/violations 2019-2020.csv'
    ## 4025 casenumber              a double           CF-2020-082351      './data/violations 2019-2020.csv'
    ## 4025 casetype                1/0/T/F/TRUE/FALSE NOTICE OF VIOLATION './data/violations 2019-2020.csv'
    ## 4025 violationnumber         a double           VI-2020-049232      './data/violations 2019-2020.csv'
    ## 4025 violationresolutiondate 1/0/T/F/TRUE/FALSE 2021-01-05 00:00:00 './data/violations 2019-2020.csv'
    ## .... ....................... .................. ................... .................................
    ## See problems(...) for more details.

``` r
prop_fire_parcel_vio <- prop_fire_parcel1 %>%
  left_join(violation%>%
              dplyr::select(-lng,-lat,-objectid), by = "opa_account_num")
```

``` r
prop_fire_parcel_vio <- prop_fire_parcel_vio %>%
  mutate(y = ifelse(is.na(MUSA_ID)==TRUE, "no fire", "fire"),
         code_vio = ifelse(is.na(violationcode)==TRUE, "no code violation", "code violation")
         )

prop_fire_parcel_vio %>%
    dplyr::select(y, code_vio) %>%
    gather(Variable, value, -y) %>%
    count(Variable, value, y) %>%
      ggplot(., aes(value, n, fill = y)) +   
        geom_bar(position = "dodge", stat="identity") +
        #facet_wrap(~Variable, scales="free") +
        #scale_fill_manual(values = palette2) +
        labs(x="Code violation", y="Count",
             title = "Feature associations with the likelihood of fire",
             subtitle = "Code Violation") +
        theme(axis.text.x = element_text(hjust = 1)) +
        plotTheme()
```

![](Pre1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
prop_fire_parcel_vio  %>% 
  ggplot(aes(x = code_vio, fill = y)) + 
  geom_bar(position = position_fill()) + 
  theme_classic() + 
  labs(y = 'Percent') + 
  coord_flip()         
```

![](Pre1_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
vio_rank <-prop_fire_parcel_vio %>%
  mutate(violationcodetitle = ifelse(is.na(violationcodetitle)==TRUE, "NO CODE VIOLATION",violationcodetitle))%>%
  group_by(violationcodetitle) %>%
  summarize(count = n()) 

violation_list <- list("CLIP VIOLATION NOTICE", "HIGH WEEDS-CUT", "EXTERIOR AREA WEEDS",
                       "EXT A-VACANT LOT CLEAN/MAINTAI", "RUBBISH/GARBAGE EXTERIOR-OWNER",
                       "VACANT STRUCTURE LICENSE", "EXTERIOR AREA SANITATION", 
                       "VACANT STRUCTURE & LAND", "UNSAFE STRUCTURE", "INTERIOR SURFACES",
                       "RUBBISH & GARBAGE", "LICENSE - RENTAL PROPERTY","VACANT STRUCTURE AND LAND")
```

``` r
prop_fire_parcel_vio %>%
    filter(violationcodetitle == "CLIP VIOLATION NOTICE" |
          violationcodetitle == "HIGH WEEDS-CUT" |
          violationcodetitle == "EXTERIOR AREA WEEDS" |
          violationcodetitle == "EXT A-VACANT LOT CLEAN/MAINTAI" |
          violationcodetitle == "RUBBISH/GARBAGE EXTERIOR-OWNER" |
          violationcodetitle == "VACANT STRUCTURE LICENSE" |
          violationcodetitle == "EXTERIOR AREA SANITATION" |
          violationcodetitle == "VACANT STRUCTURE & LAND" |
          violationcodetitle == "UNSAFE STRUCTURE" |
          violationcodetitle == "INTERIOR SURFACES" |
          violationcodetitle == "RUBBISH & GARBAGE" |
          violationcodetitle == "LICENSE - RENTAL PROPERTY" |
          violationcodetitle == "VACANT STRUCTURE AND LAND"
           )%>%
    dplyr::select(y, violationcodetitle) %>%
    gather(Variable, value, -y) %>%
    count(Variable, value, y) %>%
      ggplot(., aes(value, n, fill = y)) +   
        geom_bar(position = "dodge", stat="identity") +
        #facet_wrap(~Variable, scales="free") +
        #scale_fill_manual(values = palette2) +
        labs(x="Violation code title", y="Count",
             title = "Feature associations with the likelihood of fire",
             subtitle = "Code Violation") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6))+
        plotTheme()
```

![](Pre1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
prop_fire_parcel_vio %>%
    filter(violationcodetitle == "CLIP VIOLATION NOTICE" |
          violationcodetitle == "HIGH WEEDS-CUT" |
          violationcodetitle == "EXTERIOR AREA WEEDS" |
          violationcodetitle == "EXT A-VACANT LOT CLEAN/MAINTAI" |
          violationcodetitle == "RUBBISH/GARBAGE EXTERIOR-OWNER" |
          violationcodetitle == "VACANT STRUCTURE LICENSE" |
          violationcodetitle == "EXTERIOR AREA SANITATION" |
          violationcodetitle == "VACANT STRUCTURE & LAND" |
          violationcodetitle == "UNSAFE STRUCTURE" |
          violationcodetitle == "INTERIOR SURFACES" |
          violationcodetitle == "RUBBISH & GARBAGE" |
          violationcodetitle == "LICENSE - RENTAL PROPERTY" |
          violationcodetitle == "VACANT STRUCTURE AND LAND"
           )  %>% 
  ggplot(aes(x = violationcodetitle, fill = y)) + 
  geom_bar(position = position_fill()) + 
  theme_classic() + 
  labs(y = 'Percent') + 
  coord_flip()
```

![](Pre1_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
prop_fire_parcel_vio %>%
ggplot(.)+
  geom_point(aes(x = lat, y = lng, 
                 color = violationcode,
                 ), 
             fill = "transparent", alpha = 0.4, size=0.4)+
  scale_colour_viridis(direction = -1,
  discrete = TRUE, option = "D")+
  ylim(min(prop_fire_parcel_vio$lng), max(prop_fire_parcel_vio$lng))+
  xlim(min(prop_fire_parcel_vio$lat), max(prop_fire_parcel_vio$lat))+
  labs(title="Code Violations in Philadelphia")+
  mapTheme()+
  theme(legend.position = "none")
```

    ## Warning: Removed 574636 rows containing missing values (geom_point).

![](Pre1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## spatial

``` r
request <- read_csv("./data/311 Requests 2019.csv") %>%
  drop_na(lat)
```

    ## 
    ## ─ Column specification ────────────────────────────
    ## cols(
    ##   objectid = col_double(),
    ##   service_request_id = col_double(),
    ##   status = col_character(),
    ##   status_notes = col_character(),
    ##   service_name = col_character(),
    ##   service_code = col_character(),
    ##   agency_responsible = col_character(),
    ##   service_notice = col_character(),
    ##   requested_datetime = col_datetime(format = ""),
    ##   updated_datetime = col_datetime(format = ""),
    ##   expected_datetime = col_datetime(format = ""),
    ##   address = col_character(),
    ##   zipcode = col_double(),
    ##   media_url = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double()
    ## )

    ## Warning: 100 parsing failures.
    ##   row     col               expected     actual                           file
    ##  4151 zipcode a double               Handling S './data/311 Requests 2019.csv'
    ##  4282 zipcode no trailing characters 10026-1579 './data/311 Requests 2019.csv'
    ##  5118 zipcode no trailing characters 1509 solly './data/311 Requests 2019.csv'
    ## 10543 zipcode a double               F          './data/311 Requests 2019.csv'
    ## 12073 zipcode a double               li request './data/311 Requests 2019.csv'
    ## ..... ....... ...................... .......... ..............................
    ## See problems(...) for more details.

``` r
request.sf    <- request %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform('ESRI:102729')
```

``` r
prop_fire_parcel.sf <- prop_fire_parcel%>% 
  st_as_sf(coords = st_centroid(prop_fire_parcel), crs = 4326) %>%
  st_transform('ESRI:102729')

st_c <- st_coordinates

"prop_fire_parcel <-
  prop_fire_parcel %>% 
  mutate(
    request_nn1 = nn_function(st_c(st_centroid(prop_fire_parcel.sf)), st_c(request.sf), 1)) "
```

    ## [1] "prop_fire_parcel <-\n  prop_fire_parcel %>% \n  mutate(\n    request_nn1 = nn_function(st_c(st_centroid(prop_fire_parcel.sf)), st_c(request.sf), 1)) "

``` r
footprint <- st_read("./data/LI_BUILDING_FOOTPRINTS-shp/LI_BUILDING_FOOTPRINTS.shp")
```

    ## Reading layer `LI_BUILDING_FOOTPRINTS' from data source `/Users/tangtown/Documents/MUSA 801/data/LI_BUILDING_FOOTPRINTS-shp/LI_BUILDING_FOOTPRINTS.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 543394 features and 12 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -75.27943 ymin: 39.87237 xmax: -74.9573 ymax: 40.13778
    ## geographic CRS: WGS 84

``` r
footprint.sf <- footprint %>% 
  st_as_sf(coords = st_centroid(footprint.sf), crs = 4326) %>%
  st_transform('ESRI:102729')

st_c <- st_coordinates

footprint.sf <-
  footprint.sf %>% 
  mutate(
    request_nn1 = nn_function(st_c(st_centroid(footprint.sf)), st_c(request.sf), 1)) 
```

    ## Warning in st_centroid.sf(footprint.sf): st_centroid assumes attributes are
    ## constant over geometries of x

## Previous Fire

The third risk factor is the spatial correlation of fire incidents. To
understand if fires have a tendency to cluster in Philadelphia, we
examined the average distance to 5 nearest fire event for each parcel.
The results indicate that buildings with a smaller distance to past
fires have a higher risk for future fire event.

``` r
#limit <- 
 # st_read("./data/City_Limits-shp/city_limits.shp") %>%
  #st_transform(2272)

#ggplot() + 
#  geom_sf(data = limit)
```

``` r
# load the fire data
fire.sf <- 
  property_fire%>% 
  filter(lat < 0 & lng > 0) %>%
  st_as_sf(coords = c("lat", "lng"), crs = 4326, agr = "constant") %>%
  st_set_crs(4326) %>%
  st_transform(2272)

"fire.sf <- 
  st_intersection(fire.sf, limit)

library(lubridate)

fire.sf <- 
  fire.sf %>%
  mutate(date=dmy(alm_date),
         Year=year(date),
         Month=month(date))

fire.sf_19 <- fire.sf[fire.sf$Year==2019,]
fire.sf_192 <- fire.sf_19[!is.na(fire.sf_19$Year),]

fire.sf_18 <- fire.sf[fire.sf$Year==2018,]
fire.sf_182 <- fire.sf_18[!is.na(fire.sf_18$Year),]

fire.sf_17 <- fire.sf[fire.sf$Year==2017,]
fire.sf_172 <- fire.sf_18[!is.na(fire.sf_17$Year),]

fire.sf_16 <- fire.sf[fire.sf$Year==2016,]
fire.sf_162 <- fire.sf_18[!is.na(fire.sf_16$Year),]

fire.sf_15 <- fire.sf[fire.sf$Year==2015,]
fire.sf_152 <- fire.sf_18[!is.na(fire.sf_15$Year),]

# the distance from the nearest 5 fires to the center of the grid for each year(2015-2018)
fire.sf$lagfire19.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_192),5) 
fire.sf$lagfire18.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_182),5) 
#fire.sf$lagfire17.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_172),5) 
#fire.sf$lagfire16.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_162),5) 
#fire.sf$lagfire15.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_152),5) "
```

    ## [1] "fire.sf <- \n  st_intersection(fire.sf, limit)\n\nlibrary(lubridate)\n\nfire.sf <- \n  fire.sf %>%\n  mutate(date=dmy(alm_date),\n         Year=year(date),\n         Month=month(date))\n\nfire.sf_19 <- fire.sf[fire.sf$Year==2019,]\nfire.sf_192 <- fire.sf_19[!is.na(fire.sf_19$Year),]\n\nfire.sf_18 <- fire.sf[fire.sf$Year==2018,]\nfire.sf_182 <- fire.sf_18[!is.na(fire.sf_18$Year),]\n\nfire.sf_17 <- fire.sf[fire.sf$Year==2017,]\nfire.sf_172 <- fire.sf_18[!is.na(fire.sf_17$Year),]\n\nfire.sf_16 <- fire.sf[fire.sf$Year==2016,]\nfire.sf_162 <- fire.sf_18[!is.na(fire.sf_16$Year),]\n\nfire.sf_15 <- fire.sf[fire.sf$Year==2015,]\nfire.sf_152 <- fire.sf_18[!is.na(fire.sf_15$Year),]\n\n# the distance from the nearest 5 fires to the center of the grid for each year(2015-2018)\nfire.sf$lagfire19.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_192),5) \nfire.sf$lagfire18.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_182),5) \n#fire.sf$lagfire17.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_172),5) \n#fire.sf$lagfire16.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_162),5) \n#fire.sf$lagfire15.nn5 <-nn_function(st_coordinates(fire.sf),st_coordinates(fire.sf_152),5) "

``` r
fire_property_trim <-mutate(fire.sf, isfire = case_when(
  is.na(inci_no) == FALSE  ~ "Have fire",
  is.na(inci_no) == TRUE  ~ "No fire"))

#fire_property_trim %>%
#    ggplot(aes(isfire, lagfire19.nn5, fill=isfire)) + 
#      geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
#      #scale_fill_manual(values = palette2) +
#      labs(x="isfire", y="Value", 
#           title = "Nearby fire associations with the likelihood of fire (2019, nearest 5)",
#           subtitle = "(continous outcomes)") +
#      theme(legend.position = "none")
```

``` r
#fire.sample <- fire.sf[sample(nrow(fire.sf), 3000), ]

#ggplot() + 
#  geom_sf(data = limit) +
#  geom_sf(data = fire.sample, colour="red", size=0.1, show.legend = "point") +
#  labs(title= "Fire, Philadelphia") +
#  mapTheme()
```

``` r
#fire.sf_182.sample <- fire.sf_182[sample(nrow(fire.sf_182), 3000), ]
#fire.sf_192.sample <- fire.sf_192[sample(nrow(fire.sf_192), 3000), ]

#ggplot() + 
#  geom_sf(data = limit) +
#  geom_sf(data = fire.sf_182.sample, colour="firebrick1", size=0.1, show.legend = "point") +
#  geom_sf(data = fire.sf_192.sample, colour="firebrick4", size=0.1, show.legend = "point") +
#  labs(title= "Fire 2018/2019, Philadelphia") +
#  mapTheme()
```

``` r
#fire.sf_b <- fire.sf%>%filter(descript == 'Building fire')

#fire.sf_n <- fire.sf%>%filter(descript != 'Building fire')
##fire.sf_n.sample <- fire.sf_n[sample(nrow(fire.sf_192), 3000), ]

#ggplot() + 
#  geom_sf(data = limit) +
#  geom_sf(data = fire.sf_b, colour="red", size=0.1, show.legend = "point") +
#  geom_sf(data = fire.sf_n, colour="grey", size=0.1, show.legend = "point") +
#  labs(title= "Building fire, Philadelphia") +
#  mapTheme()
```

## Next Steps

-   Model
-   API \#\#\# API

``` {r
#Request OPA number for each address-------------------------------------
for (i in 1:nrow(fireData)) {
  # cat(i,"\n")
  address  <- fireData$address[[i]]
  base_url <- "http://api.phila.gov/ais/v1/"
  endpoint <- "search/"
  key      <- "?gatekeeperKey=dc953bbc4ade9d00eabf5409f6d73d3e"
  url <- paste(base_url, endpoint, address, key, sep="")
  response <- httr::GET(url)
  tidy_res <- httr::content(response, simplifyVector=TRUE)
  if (length(tidy_res) != 4){
    if(length(tidy_res$features$properties$opa_account_num)==2)
      opa_num <-  tidy_res$features$properties$opa_account_num[2]
    else
      opa_num <-  tidy_res$features$properties$opa_account_num[1]
  # fireData$opa[[i]] <- tidy_res$features$properties$opa_account_num[1]
    if(is.null(opa_num)){
      opa_num <- "OPA IS NULL"
    } else if(nchar(opa_num)==0) {
      opa_num <- "OPA IS ZERO LENGTH"
    }
    cat("Address",i,"OPA#:",opa_num,"\n")
    api_res[i,"OPA_Num"] <- opa_num
  }else{
    cat("Address",i,"NO ADDRESS FOUND!","\n")
    #fireData$opa[[i]] <- "NULL"
    api_res[i,"OPA_Num"] <- "NONE FOUND"
  }
}

#Request parcel_id for each address-------------------------------------
for (i in 1:nrow(fireData)) {
  address  <- fireData$address[[i]]
  base_url <- "http://api.phila.gov/ais/v1/"
  endpoint <- "search/"
  key      <- "?gatekeeperKey=dc953bbc4ade9d00eabf5409f6d73d3e"
  url <- paste(base_url, endpoint, address, key, sep="")
  response <- httr::GET(url)
  tidy_res <- httr::content(response, simplifyVector=TRUE)
  if (length(tidy_res) != 4){
    if(length(tidy_res$features$properties$dor_parcel_id)==2)
      parcel_id <-  tidy_res$features$properties$dor_parcel_id[2]
    else
      parcel_id <-  tidy_res$features$properties$dor_parcel_id[1]
    if(is.null(parcel_id)){
      parcel_id <- "parcel_id IS NULL"
    } else if(nchar(parcel_id)==0) {
      parcel_id <- "parcel_id IS ZERO LENGTH"
    }
    cat("Address",i,"parcel_id#:",parcel_id,"\n")
    fireData[i,"parcel_id"] <- parcel_id
  }else{
    cat("Address",i,"NO ADDRESS FOUND!","\n")
    fireData[i,"parcel_id"] <- "NONE FOUND"
  }
}

#L&I API---------------------------------------------
#Request L&I violations for an address
base_url <- "https://phl.carto.com/api/v2/"
endpoint <- "sql"
query    <- c("?q=SELECT%20*%20FROM%20violations%20WHERE%20opa_account_num%20=%20")
opa_num  <- paste0("%27",871601760,"%27")
url <- paste(base_url, endpoint, query, opa_num, sep="")

response <- httr::GET(url)
tidy_res <- httr::content(response, simplifyVector=TRUE)$rows

tidy_res

#Request L&I violations for each address-------------------------------
for (i in 1:nrow(fireData)) {
  address  <- fireData$address[[i]]
  base_url <- "https://phl.carto.com/api/v2/"
  endpoint <- "sql"
  query    <- c("?q=SELECT%20*%20FROM%20violations%20WHERE%20opa_account_num%20=%20")
  opa_num  <- paste0("%27",fireData$opa_account_num[[i]],"%27")
  url <- paste(base_url, endpoint, query, opa_num, sep="")
  response <- httr::GET(url)
  tidy_res <- httr::content(response, simplifyVector=TRUE)
  
  #codes needed to be modified
  if (length(tidy_res) != 4){
    if(length(tidy_res$features$properties$dor_parcel_id)==2)
      parcel_id <-  tidy_res$features$properties$dor_parcel_id[2]
    else
      parcel_id <-  tidy_res$features$properties$dor_parcel_id[1]
    if(is.null(parcel_id)){
      parcel_id <- "parcel_id IS NULL"
    } else if(nchar(parcel_id)==0) {
      parcel_id <- "parcel_id IS ZERO LENGTH"
    }
    cat("Address",i,"parcel_id#:",parcel_id,"\n")
    fireData[i,"parcel_id"] <- parcel_id
  }else{
    cat("Address",i,"NO ADDRESS FOUND!","\n")
    fireData[i,"parcel_id"] <- "NONE FOUND"
  }
}
```

![Project Plan](plan.png)
