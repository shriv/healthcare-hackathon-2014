library(ggplot2)
library(dplyr)

## Read merged NHS + postcode data
merged_nhs_postcode<- "~/Documents/healthcare-hackathon/exploration/all_merged_data_cleaned.csv"
data <- read.csv(merged_nhs_postcode)

## Merge in population data
GetCountyFromDistrict <- function(district){
    expression <- regexpr("[A-Z]+", as.character(district))
    regmatches(district, expression)}

pop <- read.csv("~/Documents/census_2011_data_by_district.csv")
pop$county <- sapply(pop$Postcode, GetCountyFromDistrict)

population_county_summary <- pop %>%
    group_by(county) %>%
        summarise(population = sum(as.numeric(All.usual.residents)))

## Add in correct population
data <- data %>%
    ## select(-population, -X, -units_per_person) %>%
        merge(., population_county_summary, by = "county") %>%
            mutate(units_per_person = items / population)

## Filter for data.
## Only use rows with > 6 months of data
fit_time_series <- data %>% 
    group_by(county, BNF.Section.Name) %>%
        summarise(rows = n()) %>%
            filter(rows > 6) %>%
                select(-rows)

## Filter for full time series data.
## Only use rows with 12 months of data
fit_time_series_full <- data %>% 
    group_by(county, BNF.Section.Name) %>%
        summarise(rows = n()) %>%
            filter(rows == 12) %>%
                select(-rows)
 
## Plot cross-county data time series by section name
fitted_data <- merge(data, fit_time_series_full, by = c("county", "BNF.Section.Name")) %>%
    group_by(date, BNF.Section.Name) %>%
        summarise(items = sum(items),
                  population = sum(as.numeric(population)),
                  units_per_person = items / as.numeric(population))
 
top_n_sections <- fitted_data %>%
    group_by(BNF.Section.Name) %>%
        summarise(total = sum(items) / sum(as.numeric(population)),
                  mean = mean(items / as.numeric(population)),
                  sd = sd(items / as.numeric(population))) %>%
                      mutate(fraction = sd / mean) %>%
                          top_n(15, fraction)
    
fitted_filtered_data <- merge(fitted_data, top_n_sections, by = "BNF.Section.Name")

## Plot of top N BNF.Section.Name by fractional variance over time
ggplot(fitted_filtered_data) +
    geom_point(aes(x = date, y = units_per_person)) +
        facet_wrap(~BNF.Section.Name, scales = "free_y", ncol = 5) +
            theme_bw() + 
                theme(axis.text.x = element_text(angle = 90, size = 10),
                      strip.text.x = element_text(size = 8),
                      axis.title.y = element_text(size = 16),
                      axis.title.x = element_text(size = 16),
                      axis.text.y = element_text(size = 8)) +
                        geom_vline(aes(xintercept = which(levels(date) %in% "2013-12"), colour = "red")) +
                            geom_vline(aes(xintercept = which(levels(date) %in% "2014-06"), colour = "blue")) +
                                xlab("Date") +
                                    ylab("Units per person")

## Analysis for plotting
## Vaccinations vs. time (dominated by influenza vaccine)
## Prove this. grep vaccines and count by Influenza
ggplot(filter(data,
              BNF.Section.Name == "Vaccines And Antisera")) +
                  geom_boxplot(aes(x = date, y = units_per_person)) +
                      theme_bw() + 
                      theme(axis.text.x = element_text(angle = 90))

## Analysis by county and section name
## fitted_data_county <- merge(data, fit_time_series, by = c("county", "BNF.Section.Name")) %>%
##     group_by(county, BNF.Section.Name) %>%
##     do(mod = lm(date ~ items, data = .)) %>%
##         do(data.frame(var = names(coef(.$mod)),
##                       coef = coef(.$mod),
##                       BNF.Section.Name = .$BNF.Section.Name,
##                       county = .$county))
                          
