library(ggplot2)
library(dplyr)

## Data filenames ##
prescription_data <- "/Users/shrividya//nhs-prescriptions-presentation-level-aug13-aug14/T201408PDPI BNFT.CSV"
prescriber_data <- "/Users/shrividya/Documents/healthcare-hackathon-2014/data/nhs-prescriptions-presentation-level-aug13-aug14/T201308ADDR BNFT.CSV"
drug_info_data <- "/Users/shrividya/Documents/healthcare-hackathon-2014/data/unique_drug_info_table.csv"
postcode_data <- "/Users/shrividya/Documents/healthcare-hackathon-2014/data/uk-postcode-database-csv.csv"

## Read NHS Datasets ##
prescriptions <- read.csv(prescription_data)
prescriber <- read.csv(prescriber_data)
drug_info <- read.csv(drug_info_data) # drug info data aggregated from all xls files

## Read external datasets ##
postcode <- read.csv(postcode_data,
                     header=F,
                     col.names = c("district",
                         "x1",
                         "y1",
                         "lat",
                         "long",
                         "town",
                         "county_name"))
 
## Transformation functions ##
GetDistrictFromPostcode <- function(postcode){
    strsplit(as.character(postcode), " ")[[1]][1]}
    
GetCountyFromDistrict <- function(district){
    expression <- regexpr("[A-Z]+", as.character(district))
    regmatches(district, expression)}

GetCountyFromPostcode <- function(postcode){
    district <- strsplit(as.character(postcode), " ")[[1]][1]
    expression <- regexpr("[A-Z]+", as.character(district))
    regmatches(district, expression)}

GetMonth <- function(time){
    time <- as.character(time)
    month <- substr(time, 5, 6)
    return(month)}

GetYear <- function(time){
    time <- as.character(time)
    month <- substr(time, 1, 4)
    return(month)}


## Select columns ##
## prescription data transformation
prescriptions_mod <- prescriptions %>%
    select(BNF.CODE, PRACTICE, SHA, PCT, ITEMS) %>%
        group_by(BNF.CODE, PRACTICE, SHA, PCT) %>%
            summarise(sum_items = sum(ITEMS))

## prescriber data transformation
names(prescriber) <- c("time", "PRACTICE", "practice_name", "name2", "street_name", "town", "county", "postcode")
prescriber_mod <- prescriber %>%
    select(PRACTICE, time, postcode)
prescriber_mod$district <- sapply(prescriber_mod$postcode, GetDistrictFromPostcode)
prescriber_mod$month <- sapply(prescriber_mod$time, GetMonth)

## postcode data transformation
postcode$county <- sapply(postcode$district, GetCountyFromDistrict)

## Merge Datasets ##
## Merge prescriptions with drug info
merge_drug <- merge(prescriptions_mod, drug_info, by = "BNF.CODE")
## Merge prescriber data  with prescriptions + drug_info
merge_prescriber <- merge(merge_drug, prescriber_mod, by = "PRACTICE") %>%
    arrange(desc(sum_items))
## Merge postcode data with prescriptions + drug_info + prescriber
merge_postcode <- merge(merge_prescriber, postcode, by = "district")

## Final data transformation ##
final_data <- merge_postcode %>%
    group_by(county, BNF.Section.Name) %>%
        summarise(month = month[1],
                  county_name = county_name[1],
                  items = sum(sum_items))
                 
