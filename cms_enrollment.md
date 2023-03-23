CMS Monthly Enrollment Data Extraction - All Months
================
Sarah Forrest

``` r
library(tidyverse)
library(stringr)
library(gdata)
```

Monthly enrollment data at the contract/plan/state/county level was
downloaded from CMS here:
<https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/Monthly-Enrollment-by-Contract-Plan-State-County>

The downloaded folders contain 2 files: an enrollment data file and a
contract data file. Both files share the following common fields:
`contract_number` (= `contract_id`) and `plan_id`. These two fields
combined make up the H-number.

The function below called `enrollment_data` reading in these files and
creates 2 output files–one for MAP plans and one for MA D-SNP plans for
any given month. The output dataset is restricted to health plans
(i.e. H-numbers) of interest only. The function merges the contract data
file and the enrollment data file together using the shared fields
`contract_number` (= `contract_id`) and `plan_id`. Then, it pivots the
datasets to wide format and calculates the following “total” variables:

-   New York City Total: `nyc_total` = the sum across New York, Bronx,
    Kings, Queens, and Richmond counties
-   New York City Metro Total: `nyc_metro_total` = the sum across NYC
    counties plus Nassau, Suffolk, Westchester, and Rockland counties
-   New York State Total: `nys_total` = the sum across all counties in
    New York

Finally, the function saves the resulting datasets in the R enviornment
and creates a CSV file for both the MAP plan dataset and the MA D-SNP
plan dataset.

``` r
enrollment_data <- function(yyyy_mm) {
  
  path_enroll = str_c("data/raw_data/CPSC_Enrollment_", yyyy_mm, "/CPSC_Enrollment_Info_", yyyy_mm, ".csv")
  path_contract = str_c("data/raw_data/CPSC_Enrollment_", yyyy_mm, "/CPSC_Contract_Info_", yyyy_mm, ".csv")

  enroll <- read_csv(path_enroll) %>%
    filter(State == "NY") %>%
    janitor::clean_names()
  
  contract_map <- read_csv(path_contract) %>%
  janitor::clean_names() %>%
  mutate(h_number = str_c(contract_id, '-', plan_id)) %>%
  filter(h_number %in% c("H3359-034", "H5549-003", "H3347-007", "H2168-002", "H6988-004", "H5599-003", "H0034-002", "H1732-001", "H5992-007", "H6776-002", "H4922-010", "H0423-007", "H5599-008"))
  
  contract_madsnp = read_csv(path_contract) %>%
  janitor::clean_names() %>%
  mutate(h_number = str_c(contract_id, '-', plan_id)) %>%
  filter(h_number %in% c("H3312-069", "H4922-003", "H6988-002", "H3347-002", "H3330-042", "H5991-010", "H8432-007", "H8432-028", "H1732-003", "H0034-001", "H3359-021", "H3533-034", "H5970-026", "H0423-001", "H5992-008", "H0271-060", "H3387-014", "H3387-015", "H2168-001", "H2168-003","H5549-011", "H5599-001"))
  

map = 
  inner_join(enroll, contract_map, by = c("contract_number" = "contract_id", "plan_id" = "plan_id")) %>%
  select(h_number, contract_number, plan_id, organization_marketing_name, plan_name, plan_type, county, enrollment) %>%
  mutate(enrollment = case_when(enrollment == "*" ~ "0", enrollment != "*" ~ enrollment)) %>%
  mutate(enrollment = as.numeric(enrollment)) %>%
  pivot_wider(names_from = county, values_from = enrollment) %>%
  janitor::clean_names() %>%
  select(order(colnames(.))) %>%
  select(h_number, contract_number, plan_id, organization_marketing_name, plan_name, plan_type, everything()) %>%
  mutate(
    nyc_total = (sum = rowSums(dplyr::select(., new_york, bronx, kings, queens, richmond), na.rm = TRUE)),
    nyc_metro_total = (sum = rowSums(dplyr::select(., new_york, bronx, kings, queens, richmond, nassau, suffolk, westchester, rockland), na.rm = TRUE)),
    nys_total = (sum = rowSums(dplyr::select(., albany:westchester), na.rm = TRUE))) %>% # only to westchester here rather than yates because files from 2021 and earlier don't have a column for yates
  select(organization_marketing_name, plan_name, h_number, contract_number, plan_id, plan_type, nyc_total, nyc_metro_total, nys_total, everything())
        
madsnp = 
  inner_join(enroll, contract_madsnp, by = c("contract_number" = "contract_id", "plan_id" = "plan_id")) %>%
  select(h_number, contract_number, plan_id, organization_marketing_name, plan_name, plan_type, county, enrollment) %>%
  mutate(enrollment = case_when(enrollment == "*" ~ "0", enrollment != "*" ~ enrollment)) %>%
  mutate(enrollment = as.numeric(enrollment)) %>%
  pivot_wider(names_from = county, values_from = enrollment) %>%
  janitor::clean_names() %>%
  select(order(colnames(.))) %>%
  select(h_number, contract_number, plan_id, organization_marketing_name, plan_name, plan_type, everything()) %>%
  mutate(
    nyc_total = (sum = rowSums(dplyr::select(., new_york, bronx, kings, queens, richmond), na.rm = TRUE)),
    nyc_metro_total = (sum = rowSums(dplyr::select(., new_york, bronx, kings, queens, richmond, nassau, suffolk, westchester, rockland), na.rm = TRUE)),
    nys_total = (sum = rowSums(dplyr::select(., albany:yates), na.rm = TRUE))) %>%
  select(organization_marketing_name, plan_name, h_number, contract_number, plan_id, plan_type, nyc_total, nyc_metro_total, nys_total, everything())


map_blank <- map %>% # replicate dataset to create a blank version
  select(-organization_marketing_name, -contract_number, -plan_id, -plan_type) # select only necessary variables
map_blank <- sapply(map_blank, as.character) 
map_blank[is.na(map_blank)] <- "" # replace NA with blank

madsnp_blank <- madsnp %>% # replicate dataset to create a blank version
  select(-organization_marketing_name, -contract_number, -plan_id, -plan_type) # select only necessary variables
madsnp_blank <- sapply(madsnp_blank, as.character) 
madsnp_blank[is.na(madsnp_blank)] <- "" # replace NA with blank


df_name_map = str_c("map_", yyyy_mm)
df_name_madsnp = str_c("madsnp_", yyyy_mm)

assign(x = df_name_map, value = map, envir = globalenv()) # or map_blank
assign(x = df_name_madsnp, value = madsnp, envir = globalenv()) # or madsnp_blank

save_path_map = str_c("data/output_data/map/", df_name_map, ".csv")
save_path_madsnp = str_c("data/output_data/madsnp/", df_name_madsnp, ".csv")

write.csv(map_blank,save_path_map, row.names = TRUE)
write.csv(madsnp_blank, save_path_madsnp, row.names = TRUE)
}
```

The `enrollment_data` function above is applied to all months of data
from 2021 - present.

``` r
enrollment_data(yyyy_mm = "2023_02")
enrollment_data(yyyy_mm = "2023_01")
enrollment_data(yyyy_mm = "2022_12")
enrollment_data(yyyy_mm = "2022_11")
enrollment_data(yyyy_mm = "2022_10")
enrollment_data(yyyy_mm = "2022_09")
enrollment_data(yyyy_mm = "2022_08")
enrollment_data(yyyy_mm = "2022_07")
enrollment_data(yyyy_mm = "2022_06")
enrollment_data(yyyy_mm = "2022_05")
enrollment_data(yyyy_mm = "2022_04")
enrollment_data(yyyy_mm = "2022_03")
enrollment_data(yyyy_mm = "2022_02")
enrollment_data(yyyy_mm = "2022_01")
enrollment_data(yyyy_mm = "2021_12")
enrollment_data(yyyy_mm = "2021_11")
enrollment_data(yyyy_mm = "2021_10")
enrollment_data(yyyy_mm = "2021_09")
enrollment_data(yyyy_mm = "2021_08")
enrollment_data(yyyy_mm = "2021_07")
enrollment_data(yyyy_mm = "2021_06")
enrollment_data(yyyy_mm = "2021_05")
enrollment_data(yyyy_mm = "2021_04")
enrollment_data(yyyy_mm = "2021_03")
enrollment_data(yyyy_mm = "2021_02")
enrollment_data(yyyy_mm = "2021_01")
```

Merge the data together

NYC MAP:

``` r
# initialize nyc_map as an empty dataframe
nyc_map <- data.frame()

# iterate over each month and year
for (year in 2021:2023) {
  for (month in 1:12) {
    # create the name of the dataframe to be merged
    map_name <- paste0("map_", year, "_", sprintf("%02d", month))
    
    # Check if object exists
    if (exists(map_name)) {
      # merge the current dataframe with nyc_metro_map
      if (nrow(nyc_map) == 0) {
        # if nyc_metro_map is empty, just add the current dataframe to it
        nyc_map <- get(map_name)[ , c("h_number", "nyc_total")]
      } else {
        # if nyc_metro_map is not empty, merge the current dataframe with it
        merge_cols <- c("h_number")
        nyc_map <- merge(nyc_map, 
                         get(map_name)[ , c("h_number", "nyc_total")], 
                         by = merge_cols,
                         all = TRUE)
        # Clean up suffixes in column names
        colnames(nyc_map) <- gsub("_x$", "", colnames(nyc_map))
        colnames(nyc_map) <- gsub("_y$", "", colnames(nyc_map))
      }
    } else {
      # Object does not exist, so skip this iteration
      next
    }
  }
}
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_map, get(map_name)[, c("h_number",
## "nyc_total")], : column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result
```

NYC Metro MAP:

``` r
# initialize nyc_metro_map as an empty dataframe
nyc_metro_map <- data.frame()

# iterate over each month and year
for (year in 2021:2023) {
  for (month in 1:12) {
    # create the name of the dataframe to be merged
    map_name <- paste0("map_", year, "_", sprintf("%02d", month))
    
    # Check if object exists
    if (exists(map_name)) {
      # merge the current dataframe with nyc_metro_metro_map
      if (nrow(nyc_metro_map) == 0) {
        # if nyc_metro_metro_map is empty, just add the current dataframe to it
        nyc_metro_map <- get(map_name)[ , c("h_number", "nyc_metro_total")]
      } else {
        # if nyc_metro_metro_map is not empty, merge the current dataframe with it
        merge_cols <- c("h_number")
        nyc_metro_map <- merge(nyc_metro_map, 
                         get(map_name)[ , c("h_number", "nyc_metro_total")], 
                         by = merge_cols,
                         all = TRUE)
        # Clean up suffixes in column names
        colnames(nyc_metro_map) <- gsub("_x$", "", colnames(nyc_metro_map))
        colnames(nyc_metro_map) <- gsub("_y$", "", colnames(nyc_metro_map))
      }
    } else {
      # Object does not exist, so skip this iteration
      next
    }
  }
}
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_map, get(map_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
```

NYS MAP:

``` r
# initialize nys_map as an empty dataframe
nys_map <- data.frame()

# iterate over each month and year
for (year in 2021:2023) {
  for (month in 1:12) {
    # create the name of the dataframe to be merged
    map_name <- paste0("map_", year, "_", sprintf("%02d", month))
    
    # Check if object exists
    if (exists(map_name)) {
      # merge the current dataframe with nys_metro_map
      if (nrow(nys_map) == 0) {
        # if nys_metro_map is empty, just add the current dataframe to it
        nys_map <- get(map_name)[ , c("h_number", "nys_total")]
      } else {
        # if nys_metro_map is not empty, merge the current dataframe with it
        merge_cols <- c("h_number")
        nys_map <- merge(nys_map, 
                         get(map_name)[ , c("h_number", "nys_total")], 
                         by = merge_cols,
                         all = TRUE)
        # Clean up suffixes in column names
        colnames(nys_map) <- gsub("_x$", "", colnames(nys_map))
        colnames(nys_map) <- gsub("_y$", "", colnames(nys_map))
      }
    } else {
      # Object does not exist, so skip this iteration
      next
    }
  }
}
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nys_map, get(map_name)[, c("h_number",
## "nys_total")], : column names 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result
```

NYC MA D-SNP:

``` r
# initialize nyc_madsnp as an empty dataframe
nyc_madsnp <- data.frame()

# iterate over each month and year
for (year in 2021:2023) {
  for (month in 1:12) {
    # create the name of the dataframe to be merged
    madsnp_name <- paste0("madsnp_", year, "_", sprintf("%02d", month))
    
    # Check if object exists
    if (exists(madsnp_name)) {
      # merge the current dataframe with nyc_metro_madsnp
      if (nrow(nyc_madsnp) == 0) {
        # if nyc_metro_madsnp is empty, just add the current dataframe to it
        nyc_madsnp <- get(madsnp_name)[ , c("h_number", "nyc_total")]
      } else {
        # if nyc_metro_madsnp is not empty, merge the current dataframe with it
        merge_cols <- c("h_number")
        nyc_madsnp <- merge(nyc_madsnp, 
                         get(madsnp_name)[ , c("h_number", "nyc_total")], 
                         by = merge_cols,
                         all = TRUE)
        # Clean up suffixes in column names
        colnames(nyc_madsnp) <- gsub("_x$", "", colnames(nyc_madsnp))
        colnames(nyc_madsnp) <- gsub("_y$", "", colnames(nyc_madsnp))
      }
    } else {
      # Object does not exist, so skip this iteration
      next
    }
  }
}
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y',
## 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x',
## 'nyc_total.y', 'nyc_total.x', 'nyc_total.y', 'nyc_total.x', 'nyc_total.y' are
## duplicated in the result
```

NYC Metro MA D-SNP:

``` r
# initialize nyc_metro_madsnp as an empty dataframe
nyc_metro_madsnp <- data.frame()

# iterate over each month and year
for (year in 2021:2023) {
  for (month in 1:12) {
    # create the name of the dataframe to be merged
    madsnp_name <- paste0("madsnp_", year, "_", sprintf("%02d", month))
    
    # Check if object exists
    if (exists(madsnp_name)) {
      # merge the current dataframe with nyc_metro_metro_madsnp
      if (nrow(nyc_metro_madsnp) == 0) {
        # if nyc_metro_metro_madsnp is empty, just add the current dataframe to it
        nyc_metro_madsnp <- get(madsnp_name)[ , c("h_number", "nyc_metro_total")]
      } else {
        # if nyc_metro_metro_madsnp is not empty, merge the current dataframe with it
        merge_cols <- c("h_number")
        nyc_metro_madsnp <- merge(nyc_metro_madsnp, 
                         get(madsnp_name)[ , c("h_number", "nyc_metro_total")], 
                         by = merge_cols,
                         all = TRUE)
        # Clean up suffixes in column names
        colnames(nyc_metro_madsnp) <- gsub("_x$", "", colnames(nyc_metro_madsnp))
        colnames(nyc_metro_madsnp) <- gsub("_y$", "", colnames(nyc_metro_madsnp))
      }
    } else {
      # Object does not exist, so skip this iteration
      next
    }
  }
}
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result

## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y' are duplicated in the result
## Warning in merge.data.frame(nyc_metro_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y',
## 'nyc_metro_total.x', 'nyc_metro_total.y', 'nyc_metro_total.x',
## 'nyc_metro_total.y', 'nyc_metro_total.x', 'nyc_metro_total.y' are duplicated in
## the result
```

NYS MA D-SNP:

``` r
# initialize nys_madsnp as an empty dataframe
nys_madsnp <- data.frame()

# iterate over each month and year
for (year in 2021:2023) {
  for (month in 1:12) {
    # create the name of the dataframe to be merged
    madsnp_name <- paste0("madsnp_", year, "_", sprintf("%02d", month))
    
    # Check if object exists
    if (exists(madsnp_name)) {
      # merge the current dataframe with nys_metro_madsnp
      if (nrow(nys_madsnp) == 0) {
        # if nys_metro_madsnp is empty, just add the current dataframe to it
        nys_madsnp <- get(madsnp_name)[ , c("h_number", "nys_total")]
      } else {
        # if nys_metro_madsnp is not empty, merge the current dataframe with it
        merge_cols <- c("h_number")
        nys_madsnp <- merge(nys_madsnp, 
                         get(madsnp_name)[ , c("h_number", "nys_total")], 
                         by = merge_cols,
                         all = TRUE)
        # Clean up suffixes in column names
        colnames(nys_madsnp) <- gsub("_x$", "", colnames(nys_madsnp))
        colnames(nys_madsnp) <- gsub("_y$", "", colnames(nys_madsnp))
      }
    } else {
      # Object does not exist, so skip this iteration
      next
    }
  }
}
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the
## result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result

## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y' are duplicated in the result
## Warning in merge.data.frame(nys_madsnp, get(madsnp_name)[, c("h_number", :
## column names 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y',
## 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x',
## 'nys_total.y', 'nys_total.x', 'nys_total.y', 'nys_total.x', 'nys_total.y' are
## duplicated in the result
```

Add column names to all MAP and MA D-SNP dataframes

``` r
# create column names vector
col_name_vector_forward = c("h_number", "2021_01", "2021_02", "2021_03", "2021_04", "2021_05", "2021_06", "2021_07", "2021_08", "2021_09", "2021_10", "2021_11", "2021_12", "2022_01", "2022_02", "2022_03", "2022_04", "2022_05", "2022_06", "2022_07", "2022_08", "2022_09", "2022_10", "2022_11", "2022_12", "2023_01", "2023_02")

# applying colnames
colnames(nyc_map) = col_name_vector_forward
colnames(nyc_metro_map) = col_name_vector_forward
colnames(nys_map) = col_name_vector_forward

colnames(nyc_madsnp) = col_name_vector_forward
colnames(nyc_metro_madsnp) = col_name_vector_forward
colnames(nys_madsnp) = col_name_vector_forward
```

Add plan names to all MAP dataframes

``` r
map_plan_names <- map_2023_02 %>%
  select(plan_name, h_number) 

nyc_map <- inner_join(nyc_map, map_plan_names, by = "h_number") %>%
  select(plan_name, everything()) %>% 
  mutate(plan_name = gsub("(HMO D-SNP)$", "", plan_name))

nyc_metro_map <- inner_join(nyc_metro_map, map_plan_names, by = "h_number") %>%
  select(plan_name, everything())

nys_map <- inner_join(nys_map, map_plan_names, by = "h_number") %>%
  select(plan_name, everything())

nyc_map
##                                               plan_name  h_number 2021_01
## 1                  Hamaspik Medicare Choice (HMO D-SNP) H0034-002       0
## 2                       MetroPlus UltraCare (HMO D-SNP) H0423-007       0
## 3      Empire MediBlue HealthPlus Dual Plus (HMO D-SNP) H1732-001       0
## 4   VillageCareMAX Medicare Total Advantage (HMO D-SNP) H2168-002    2512
## 5             Elderplan Plus Long Term Care (HMO D-SNP) H3347-007    2113
## 6                  Healthfirst CompleteCare (HMO D-SNP) H3359-034   14735
## 7           AgeWell New York Advantage Plus (HMO D-SNP) H4922-010       0
## 8                          VNS Health Total (HMO D-SNP) H5549-003    2903
## 9                Wellcare Fidelis Dual Plus (HMO D-SNP) H5599-003      17
## 10               Wellcare Fidelis Dual Plus (HMO D-SNP) H5599-008       0
## 11      Senior Whole Health of New York NHC (HMO D-SNP) H5992-007     116
## 12                          RiverSpring MAP (HMO D-SNP) H6776-002       0
## 13 Centers Plan for Medicaid Advantage Plus (HMO D-SNP) H6988-004      50
##    2021_02 2021_03 2021_04 2021_05 2021_06 2021_07 2021_08 2021_09 2021_10
## 1        0       0       0       0       0       0      12      21      47
## 2        0       0       0       0       0       0       0       0       0
## 3        0       0       0      37      44      70      79      91      93
## 4     2502    2637    2725    2835    2859    2898    2898    2915    2894
## 5     2193    2255    2258    2404    2443    2473    2519    2551    2572
## 6    15422   15936   16385   16899   17201   17575   17976   18238   18555
## 7        0       0      11      12      12      12      12      12      12
## 8     2868    2838    2812    2810    2817    2856    2867    2873    2897
## 9       18      16      16      31      65      74      77     103     118
## 10       0       0       0       0       0       0       0       0       0
## 11     115     114     113     112     109     109     110     110     110
## 12      11      14      41      43      47      49      50      47      55
## 13     153     276     366     431     475     478     526     565     595
##    2021_11 2021_12 2022_01 2022_02 2022_03 2022_04 2022_05 2022_06 2022_07
## 1       56      87      86      92     101     130     173     196     232
## 2        0       0       0       0       0       0       0       0       0
## 3      101     117     118     150     152     159     159     161     157
## 4     2871    2807    2798    2774    2757    2734    2705    2722    2730
## 5     2577    2571    2549    2592    2595    2657    2715    2756    2796
## 6    18664   18750   18955   19452   19625   19887   20292   20771   21157
## 7       12      12      13      12      12      12      13      14      14
## 8     2883    2845    2848    2797    2781    2759    2789    2802    2796
## 9      139     152     157     160     164     171     186     273     325
## 10       0       0       0       0       0       0       0       0       0
## 11     112     108     104     100      99     104     109     118     123
## 12      55      57      55      58      68      72      72      80      80
## 13     637     635     746     865     950    1009    1040    1088    1107
##    2022_08 2022_09 2022_10 2022_11 2022_12 2023_01 2023_02
## 1      257     289     340     379     404     423     461
## 2        0       0      12      13      25      26      37
## 3      159     149     151     156     169     159     148
## 4     2678    2658    2638    2616    2577    2529    2540
## 5     2831    2859    2897    2906    2919    2895    2934
## 6    21311   21370   21445   21585   21721   21907   22263
## 7       15      17      29      30      33      35      38
## 8     2824    2823    2817    2823    2784    2886    2903
## 9      349     393     415     419     409     396     428
## 10       0       0       0       0       0       0       0
## 11     128     145     139     136     132     136     139
## 12      93      99     104     112     122     130     147
## 13    1126    1155    1176    1181    1152    1186    1256
nyc_metro_map
##                                               plan_name  h_number 2021_01
## 1                  Hamaspik Medicare Choice (HMO D-SNP) H0034-002       0
## 2                       MetroPlus UltraCare (HMO D-SNP) H0423-007       0
## 3      Empire MediBlue HealthPlus Dual Plus (HMO D-SNP) H1732-001       0
## 4   VillageCareMAX Medicare Total Advantage (HMO D-SNP) H2168-002    2512
## 5             Elderplan Plus Long Term Care (HMO D-SNP) H3347-007    2282
## 6                  Healthfirst CompleteCare (HMO D-SNP) H3359-034   15166
## 7           AgeWell New York Advantage Plus (HMO D-SNP) H4922-010       0
## 8                          VNS Health Total (HMO D-SNP) H5549-003    3028
## 9                Wellcare Fidelis Dual Plus (HMO D-SNP) H5599-003      17
## 10               Wellcare Fidelis Dual Plus (HMO D-SNP) H5599-008       0
## 11      Senior Whole Health of New York NHC (HMO D-SNP) H5992-007     116
## 12                          RiverSpring MAP (HMO D-SNP) H6776-002       0
## 13 Centers Plan for Medicaid Advantage Plus (HMO D-SNP) H6988-004      50
##    2021_02 2021_03 2021_04 2021_05 2021_06 2021_07 2021_08 2021_09 2021_10
## 1        0       0       0       0       0       0      12      33      61
## 2        0       0       0       0       0       0       0       0       0
## 3        0       0       0      37      44      70      79      91      93
## 4     2502    2637    2725    2835    2859    2898    2898    2915    2894
## 5     2369    2434    2438    2588    2631    2663    2713    2748    2767
## 6    15885   16417   16880   17415   17743   18140   18556   18837   19165
## 7        0      11      27      31      32      36      35      37      39
## 8     2988    2963    2941    2948    2955    3002    3016    3022    3049
## 9       18      16      16      31      65      74      77     103     118
## 10       0       0       0       0       0       0       0       0       0
## 11     115     114     113     112     109     109     110     110     110
## 12      11      14      41      43      47      49      50      47      55
## 13     153     276     378     443     494     507     558     602     633
##    2021_11 2021_12 2022_01 2022_02 2022_03 2022_04 2022_05 2022_06 2022_07
## 1       80     111     101     110     115     145     190     214     262
## 2        0       0       0       0       0       0       0       0       0
## 3      101     117     131     166     165     172     174     175     174
## 4     2871    2807    2798    2774    2757    2734    2705    2722    2730
## 5     2772    2768    2738    2783    2784    2848    2904    2943    2982
## 6    19284   19376   19597   20102   20288   20560   20990   21476   21882
## 7       39      40      42      44      44      44      46      48      47
## 8     3033    2991    2997    2948    2932    2909    2944    2954    2953
## 9      139     152     157     160     164     171     186     273     325
## 10       0       0       0       0       0       0       0       0       0
## 11     112     108     104     100      99     104     109     118     123
## 12      55      57      55      58      68      72      72      80      80
## 13     678     675     794     920    1009    1066    1099    1148    1173
##    2022_08 2022_09 2022_10 2022_11 2022_12 2023_01 2023_02
## 1      301     342     408     452     480     504     543
## 2        0       0      12      13      25      26      37
## 3      177     168     169     174     188     176     165
## 4     2678    2658    2638    2616    2577    2529    2540
## 5     3017    3042    3084    3090    3105    3080    3120
## 6    22045   22138   22218   22380   22544   22732   23114
## 7       53      56      74      74      77      84      90
## 8     2986    2988    2986    2994    2954    3075    3100
## 9      349     408     428     432     422     410     442
## 10       0       0       0       0       0       0       0
## 11     128     145     139     136     132     136     139
## 12      93      99     104     112     122     130     147
## 13    1192    1226    1251    1257    1229    1264    1336
nys_map
##                                               plan_name  h_number 2021_01
## 1                  Hamaspik Medicare Choice (HMO D-SNP) H0034-002       0
## 2                       MetroPlus UltraCare (HMO D-SNP) H0423-007       0
## 3      Empire MediBlue HealthPlus Dual Plus (HMO D-SNP) H1732-001       0
## 4   VillageCareMAX Medicare Total Advantage (HMO D-SNP) H2168-002    2512
## 5             Elderplan Plus Long Term Care (HMO D-SNP) H3347-007    2282
## 6                  Healthfirst CompleteCare (HMO D-SNP) H3359-034   15166
## 7           AgeWell New York Advantage Plus (HMO D-SNP) H4922-010       0
## 8                          VNS Health Total (HMO D-SNP) H5549-003    3028
## 9                Wellcare Fidelis Dual Plus (HMO D-SNP) H5599-003      17
## 10               Wellcare Fidelis Dual Plus (HMO D-SNP) H5599-008      16
## 11      Senior Whole Health of New York NHC (HMO D-SNP) H5992-007     116
## 12                          RiverSpring MAP (HMO D-SNP) H6776-002       0
## 13 Centers Plan for Medicaid Advantage Plus (HMO D-SNP) H6988-004      50
##    2021_02 2021_03 2021_04 2021_05 2021_06 2021_07 2021_08 2021_09 2021_10
## 1        0       0       0       0       0       0      12      60      87
## 2        0       0       0       0       0       0       0       0       0
## 3        0       0       0      37      44      70      79      91      93
## 4     2502    2637    2725    2835    2859    2898    2898    2915    2894
## 5     2369    2434    2438    2588    2631    2663    2713    2748    2767
## 6    15885   16417   16880   17415   17743   18140   18556   18849   19178
## 7        0      11      27      31      32      36      35      37      39
## 8     2988    2963    2941    2948    2955    3002    3016    3022    3049
## 9       18      16      16      31      65      74      77     103     118
## 10      16      16      16      16      16      16      16      16      15
## 11     115     114     113     112     109     109     110     110     110
## 12      11      14      41      43      47      49      50      47      55
## 13     153     276     378     443     494     507     558     602     633
##    2021_11 2021_12 2022_01 2022_02 2022_03 2022_04 2022_05 2022_06 2022_07
## 1      107     139     129     137     143     176     221     246     292
## 2        0       0       0       0       0       0       0       0       0
## 3      101     117     131     166     165     172     174     175     174
## 4     2871    2807    2798    2774    2757    2734    2705    2722    2730
## 5     2772    2768    2738    2783    2784    2848    2904    2943    2982
## 6    19301   19393   19614   20121   20313   20584   21014   21500   21905
## 7       39      40      42      44      44      44      46      48      47
## 8     3033    2991    2997    2948    2932    2909    2944    2954    2953
## 9      139     152     157     160     164     171     186     273     325
## 10      17      16      14      15      15      14      29      42      52
## 11     112     108     104     100      99     104     109     118     123
## 12      55      57      55      58      68      72      72      80      80
## 13     678     675     794     920    1009    1066    1099    1148    1173
##    2022_08 2022_09 2022_10 2022_11 2022_12 2023_01 2023_02
## 1      333     374     440     485     514     539     579
## 2        0       0      12      13      25      26      37
## 3      177     168     169     174     188     176     165
## 4     2678    2658    2638    2616    2577    2529    2540
## 5     3017    3042    3095    3101    3116    3091    3132
## 6    22067   22159   22237   22400   22564   22749   23133
## 7       53      56      74      74      77      84      90
## 8     2986    2988    2986    2994    2954    3075    3100
## 9      349     408     428     432     422     410     442
## 10      70     111     125     115     101      93     110
## 11     128     145     139     136     132     136     139
## 12      93      99     104     112     122     130     147
## 13    1192    1226    1251    1257    1229    1264    1336
```

Add plan names to all MA D-SNP dataframes

``` r
madsnp_plan_names <- madsnp_2023_02 %>%
  select(plan_name, h_number)

nyc_madsnp <- inner_join(nyc_madsnp, madsnp_plan_names, by = "h_number") %>%
  select(plan_name, everything())

nyc_metro_madsnp <- inner_join(nyc_metro_madsnp, madsnp_plan_names, by = "h_number") %>%
  select(plan_name, everything())

nys_madsnp <- inner_join(nys_madsnp, madsnp_plan_names, by = "h_number") %>%
  select(plan_name, everything())

nyc_madsnp
##                                                    plan_name  h_number 2021_01
## 1                       Hamaspik Medicare Select (HMO D-SNP) H0034-001       0
## 2          UnitedHealthcare Dual Complete Choice (PPO D-SNP) H0271-060      NA
## 3                       MetroPlus Advantage Plan (HMO D-SNP) H0423-001    7760
## 4        Empire MediBlue HealthPlus Dual Connect (HMO D-SNP) H1732-003    1403
## 5       VillageCareMAX Medicare Health Advantage (HMO D-SNP) H2168-001     245
## 6  VillageCareMAX Medicare Health Advantage FLEX (HMO D-SNP) H2168-003      NA
## 7                     Aetna Medicare Assure Plan (HMO D-SNP) H3312-069     906
## 8                          EmblemHealth VIP Dual (HMO D-SNP) H3330-042   21043
## 9           Elderplan For Medicaid Beneficiaries (HMO D-SNP) H3347-002    4020
## 10             Healthfirst Life Improvement Plan (HMO D-SNP) H3359-021  114731
## 11     UnitedHealthcare Dual Complete Plan 1 (HMO-POS D-SNP) H3387-014      NA
## 12     UnitedHealthcare Dual Complete Plan 2 (HMO-POS D-SNP) H3387-015      NA
## 13             Humana Gold Plus SNP-DE H3533-034 (HMO D-SNP) H3533-034      NA
## 14                     AgeWell New York FeelWell (HMO D-SNP) H4922-003      35
## 15                      VNS Health EasyCare Plus (HMO D-SNP) H5549-011      NA
## 16                  Wellcare Fidelis Dual Access (HMO D-SNP) H5599-001   18283
## 17                 HumanaChoice SNP-DE H5970-026 (PPO D-SNP) H5970-026      NA
## 18                 EmblemHealth VIP Dual Reserve (HMO D-SNP) H5991-010     337
## 19    Senior Whole Health Medicare Complete Care (HMO D-SNP) H5992-008      NA
## 20           Centers Plan for Dual Coverage Care (HMO D-SNP) H6988-002     720
## 21                Empire MediBlue Dual Advantage (HMO D-SNP) H8432-007    4180
## 22         Empire MediBlue Dual Advantage Select (HMO D-SNP) H8432-028    5532
##    2021_02 2021_03 2021_04 2021_05 2021_06 2021_07 2021_08 2021_09 2021_10
## 1        0       0       0      18      36      33      54      50      42
## 2       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 3     7907    8008    8153    8250    8379    8509    8609    8718    8735
## 4     2076    2664    3127    3580    4044    4602    5050    5363    5790
## 5      263     276     306     339     389     443     511     566     589
## 6       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 7     1152    1293    1475    1719    1911    2104    2318    2474    2681
## 8    21114   21129   21198   21368   21510   21649   21852   21950   22034
## 9     4041    4070    4123    4080    4113    4234    4310    4344    4370
## 10  115784  116418  116304  117556  118340  119089  120630  121625  122450
## 11      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 12      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 13      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 14      33      33      33      30      29      29      30      30      27
## 15      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 16   18002   18251   18328   18302   18251   18268   18242   18166   18086
## 17      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 18     653     842     952    1047    1066    1077    1119    1155    1158
## 19      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 20     650     675     704     790     846     877     910     891     886
## 21    4103    4039    3961    3847    3743    3681    3615    3538    3472
## 22    5163    4808    4664    4594    4539    4486    4376    4336    4267
##    2021_11 2021_12 2022_01 2022_02 2022_03 2022_04 2022_05 2022_06 2022_07
## 1       43      41      88     104     101      98      98      97      97
## 2       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 3     8780    8929    8704    8719    8726    8751    8827    8948    9096
## 4     5980    6037    7660    8193    8543    9045    9465    9916   10288
## 5      585     551     475     440     464     448     452     454     449
## 6       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 7     2813    2789    3515    3639    3727    3794    3956    4027    4138
## 8    22131   21896   19837   18862   18440   18096   17784   17602   17394
## 9     4431    4499    4377    4370    4382    4399    4406    4399    4372
## 10  123359  124309  126428  128248  129303  130395  131826  132835  133987
## 11      NA      NA   60808   62796   63177   63569   64059   64458   64827
## 12      NA      NA    4047    3872    3509    3371    3224    3175    3118
## 13      NA      NA    6709    6807    6916    6986    7068    7090    7188
## 14      27      25      25      25      27      26      29      28      25
## 15      NA      NA     623     850     952    1063    1186    1276    1360
## 16   17981   17868   18588   18092   18113   18169   18140   18188   18232
## 17      NA      NA     411     496     577     637     712     772     819
## 18    1184    1149    1071    1036     995     979     978     959     942
## 19      NA      NA       0       0       0       0       0       0       0
## 20     863     845     919     933     902     923     912     894     867
## 21    3424    3380    3252    3181    3101    3045    2972    2917    2862
## 22    4237    4199    4065    3902    3741    3623    3500    3403    3349
##    2022_08 2022_09 2022_10 2022_11 2022_12 2023_01 2023_02
## 1       96      85      91     137     149     167     162
## 2       NA      NA      NA      NA      NA    1650    1976
## 3     9113    9233    9265    9331    9438    9292    9318
## 4    10470   10606   10990   11234   11433   12013   12279
## 5      474     493     488     485     467     468     462
## 6       NA      NA      NA      NA      NA      24      47
## 7     4238    4303    4416    4418    4454    5642    5923
## 8    17072   16817   16540   16286   16171   15396   15208
## 9     4342    4283    4210    4187    4251    4063    3962
## 10  135133  136554  137752  138461  139617  139916  140656
## 11   65280   65746   66298   66820   67126   69405   70751
## 12    3062    3016    2977    2972    2962    2842    2827
## 13    7265    7285    7356    7339    7225    7580    7549
## 14      35      34      33      20      18      19      16
## 15    1444    1523    1685    1729    1710    2009    2153
## 16   18267   18355   18352   18315   18411   17957   18059
## 17     878     910     955     981     988    1244    1297
## 18     919     894     877     860     850    1038    1085
## 19       0       0       0       0       0       0       0
## 20     850     804     788     767     745     794     750
## 21    2792    2753    2687    2652    2625    2554    2503
## 22    3251    3184    3141    3115    3080    2958    2915
nyc_metro_madsnp
##                                                    plan_name  h_number 2021_01
## 1                       Hamaspik Medicare Select (HMO D-SNP) H0034-001       0
## 2          UnitedHealthcare Dual Complete Choice (PPO D-SNP) H0271-060      NA
## 3                       MetroPlus Advantage Plan (HMO D-SNP) H0423-001    7760
## 4        Empire MediBlue HealthPlus Dual Connect (HMO D-SNP) H1732-003    1675
## 5       VillageCareMAX Medicare Health Advantage (HMO D-SNP) H2168-001     245
## 6  VillageCareMAX Medicare Health Advantage FLEX (HMO D-SNP) H2168-003      NA
## 7                     Aetna Medicare Assure Plan (HMO D-SNP) H3312-069    1337
## 8                          EmblemHealth VIP Dual (HMO D-SNP) H3330-042   26506
## 9           Elderplan For Medicaid Beneficiaries (HMO D-SNP) H3347-002    4156
## 10             Healthfirst Life Improvement Plan (HMO D-SNP) H3359-021  119282
## 11     UnitedHealthcare Dual Complete Plan 1 (HMO-POS D-SNP) H3387-014      NA
## 12     UnitedHealthcare Dual Complete Plan 2 (HMO-POS D-SNP) H3387-015      NA
## 13             Humana Gold Plus SNP-DE H3533-034 (HMO D-SNP) H3533-034      NA
## 14                     AgeWell New York FeelWell (HMO D-SNP) H4922-003     124
## 15                      VNS Health EasyCare Plus (HMO D-SNP) H5549-011      NA
## 16                  Wellcare Fidelis Dual Access (HMO D-SNP) H5599-001   24652
## 17                 HumanaChoice SNP-DE H5970-026 (PPO D-SNP) H5970-026      NA
## 18                 EmblemHealth VIP Dual Reserve (HMO D-SNP) H5991-010     337
## 19    Senior Whole Health Medicare Complete Care (HMO D-SNP) H5992-008      NA
## 20           Centers Plan for Dual Coverage Care (HMO D-SNP) H6988-002     720
## 21                Empire MediBlue Dual Advantage (HMO D-SNP) H8432-007    4717
## 22         Empire MediBlue Dual Advantage Select (HMO D-SNP) H8432-028    6684
##    2021_02 2021_03 2021_04 2021_05 2021_06 2021_07 2021_08 2021_09 2021_10
## 1        0      12      19      42      59      54      75      81      75
## 2       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 3     7907    8008    8153    8250    8379    8509    8609    8718    8735
## 4     2505    3181    3731    4260    4773    5442    5976    6400    6942
## 5      263     276     306     339     389     443     511     566     589
## 6       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 7     1753    1988    2292    2688    2974    3271    3575    3778    4104
## 8    26548   26613   26619   26879   27069   27262   27555   27688   27813
## 9     4182    4211    4269    4222    4255    4372    4449    4480    4505
## 10  120422  121183  121069  122489  123336  124171  125829  126888  127847
## 11      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 12      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 13      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 14     124     117     118     113     109     106     105     105     100
## 15      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 16   24281   24688   24824   24868   24855   24878   24911   24890   24808
## 17      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 18     653     842     952    1047    1066    1077    1119    1155    1158
## 19      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 20     650     675     704     790     846     877     910     891     886
## 21    4635    4563    4474    4351    4235    4162    4085    3999    3941
## 22    6300    5907    5745    5686    5637    5596    5477    5427    5367
##    2021_11 2021_12 2022_01 2022_02 2022_03 2022_04 2022_05 2022_06 2022_07
## 1       74      76     145     198     203     234     253     254     257
## 2       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 3     8780    8929    8704    8719    8726    8751    8827    8948    9096
## 4     7216    7301    9367   10081   10541   11168   11715   12300   12790
## 5      585     551     475     440     464     448     452     454     449
## 6       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 7     4301    4294    5444    5703    5904    6031    6348    6535    6694
## 8    27945   27706   25209   24062   23544   23174   22851   22629   22416
## 9     4569    4635    4507    4502    4517    4532    4539    4532    4506
## 10  128802  129864  132358  134462  135672  136908  138560  139730  141030
## 11      NA      NA   69805   72127   72661   73150   73834   74418   74935
## 12      NA      NA    5024    4845    4438    4277    4122    4079    4002
## 13      NA      NA    9680    9908   10088   10222   10394   10482   10623
## 14      99      96      91      90      94      91      91      88      81
## 15      NA      NA     635     880     997    1108    1242    1341    1429
## 16   24715   24612   25778   25042   25126   25288   25374   25500   25653
## 17      NA      NA     411     496     577     637     712     772     819
## 18    1184    1149    1071    1036     995     979     978     959     942
## 19      NA      NA       0       0       0       0       0       0       0
## 20     863     845     946     959     927     950     944     924     897
## 21    3886    3840    3697    3627    3546    3489    3412    3355    3295
## 22    5304    5250    5111    4919    4729    4585    4443    4333    4273
##    2022_08 2022_09 2022_10 2022_11 2022_12 2023_01 2023_02
## 1      265     254     258     315     330     358     357
## 2       NA      NA      NA      NA      NA    1998    2451
## 3     9113    9233    9265    9331    9438    9292    9318
## 4    13065   13283   13752   14068   14321   15025   15407
## 5      474     493     488     485     467     468     462
## 6       NA      NA      NA      NA      NA      24      47
## 7     6894    6994    7172    7240    7300    9049    9505
## 8    22052   21792   21514   21223   21093   20165   20075
## 9     4480    4418    4348    4319    4379    4185    4083
## 10  142313  143904  145292  146136  147436  147895  148776
## 11   75525   76178   76933   77600   78015   80905   82556
## 12    3932    3885    3844    3841    3827    3689    3672
## 13   10784   10821   10919   10937   10827   11277   11377
## 14      88      85      77      65      63      62      56
## 15    1517    1602    1774    1819    1798    2118    2266
## 16   25728   25954   26032   26089   26305   25692   25936
## 17     878     910     955     981     988    1244    1297
## 18     919     894     877     860     850    1038    1085
## 19       0       0       0       0       0       0       0
## 20     873     826     809     789     767     811     766
## 21    3213    3162    3091    3050    3022    2946    2880
## 22    4161    4077    4041    4010    3969    3811    3752
nys_madsnp
##                                                    plan_name  h_number 2021_01
## 1                       Hamaspik Medicare Select (HMO D-SNP) H0034-001       0
## 2          UnitedHealthcare Dual Complete Choice (PPO D-SNP) H0271-060      NA
## 3                       MetroPlus Advantage Plan (HMO D-SNP) H0423-001    7760
## 4        Empire MediBlue HealthPlus Dual Connect (HMO D-SNP) H1732-003    1694
## 5       VillageCareMAX Medicare Health Advantage (HMO D-SNP) H2168-001     245
## 6  VillageCareMAX Medicare Health Advantage FLEX (HMO D-SNP) H2168-003      NA
## 7                     Aetna Medicare Assure Plan (HMO D-SNP) H3312-069    1337
## 8                          EmblemHealth VIP Dual (HMO D-SNP) H3330-042   27293
## 9           Elderplan For Medicaid Beneficiaries (HMO D-SNP) H3347-002    4156
## 10             Healthfirst Life Improvement Plan (HMO D-SNP) H3359-021  119526
## 11     UnitedHealthcare Dual Complete Plan 1 (HMO-POS D-SNP) H3387-014      NA
## 12     UnitedHealthcare Dual Complete Plan 2 (HMO-POS D-SNP) H3387-015      NA
## 13             Humana Gold Plus SNP-DE H3533-034 (HMO D-SNP) H3533-034      NA
## 14                     AgeWell New York FeelWell (HMO D-SNP) H4922-003     124
## 15                      VNS Health EasyCare Plus (HMO D-SNP) H5549-011      NA
## 16                  Wellcare Fidelis Dual Access (HMO D-SNP) H5599-001   41880
## 17                 HumanaChoice SNP-DE H5970-026 (PPO D-SNP) H5970-026      NA
## 18                 EmblemHealth VIP Dual Reserve (HMO D-SNP) H5991-010     337
## 19    Senior Whole Health Medicare Complete Care (HMO D-SNP) H5992-008      NA
## 20           Centers Plan for Dual Coverage Care (HMO D-SNP) H6988-002     720
## 21                Empire MediBlue Dual Advantage (HMO D-SNP) H8432-007    4781
## 22         Empire MediBlue Dual Advantage Select (HMO D-SNP) H8432-028    7062
##    2021_02 2021_03 2021_04 2021_05 2021_06 2021_07 2021_08 2021_09 2021_10
## 1        0      68     106     156     185     187     206     215     229
## 2       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 3     7907    8008    8153    8250    8379    8509    8609    8718    8735
## 4     2535    3224    3782    4317    4840    5515    6064    6491    7046
## 5      263     276     306     339     389     443     511     566     589
## 6       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 7     1753    1988    2292    2688    2974    3271    3575    3778    4104
## 8    27383   27445   27437   27723   27905   28105   28418   28552   28718
## 9     4182    4211    4269    4222    4255    4372    4449    4480    4505
## 10  120681  121470  121364  122802  123660  124494  126158  127221  128182
## 11      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 12      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 13      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 14     124     117     118     113     109     106     105     105     100
## 15      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 16   41259   41921   42081   42234   42237   42320   42396   42400   42305
## 17      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 18     653     842     952    1047    1066    1077    1119    1155    1158
## 19      NA      NA      NA      NA      NA      NA      NA      NA      NA
## 20     650     675     704     790     846     877     910     891     886
## 21    4700    4630    4539    4413    4294    4219    4139    4052    3990
## 22    6681    6320    6167    6116    6070    6032    5915    5862    5797
##    2021_11 2021_12 2022_01 2022_02 2022_03 2022_04 2022_05 2022_06 2022_07
## 1      227     231     380     475     486     524     557     560     568
## 2       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 3     8780    8929    8704    8719    8726    8751    8827    8948    9096
## 4     7334    7422    9536   10258   10723   11362   11924   12524   13022
## 5      585     551     475     440     464     448     452     454     449
## 6       NA      NA      NA      NA      NA      NA      NA      NA      NA
## 7     4301    4294    5444    5703    5904    6031    6348    6535    6694
## 8    28862   28606   25990   24792   24248   23857   23510   23285   23070
## 9     4569    4635    4507    4502    4517    4532    4539    4532    4506
## 10  129150  130233  132729  134861  136080  137329  138991  140166  141479
## 11      NA      NA  101985  105193  106358  107542  109171  110408  111505
## 12      NA      NA    9368    9255    8797    8604    8437    8392    8314
## 13      NA      NA    9680    9908   10088   10222   10394   10482   10623
## 14      99      96      91      90      94      91      91      88      81
## 15      NA      NA     635     880     997    1108    1242    1341    1429
## 16   42221   42156   45054   43868   43934   44229   44429   44703   45010
## 17      NA      NA     411     496     577     637     712     772     819
## 18    1184    1149    1071    1036     995     979     978     959     942
## 19      NA      NA       0       0       0       0       0       0       0
## 20     863     845     946     959     927     950     944     924     897
## 21    3936    3890    3741    3674    3594    3533    3455    3397    3335
## 22    5730    5677    5513    5318    5111    4951    4809    4693    4630
##    2022_08 2022_09 2022_10 2022_11 2022_12 2023_01 2023_02
## 1      582     569     579     648     708     724     736
## 2       NA      NA      NA      NA      NA    4302    5213
## 3     9113    9233    9265    9331    9438    9292    9318
## 4    13291   13506   13986   14302   14558   15257   15649
## 5      474     493     488     485     467     468     462
## 6       NA      NA      NA      NA      NA      24      47
## 7     6894    6994    7172    7240    7300    9049    9505
## 8    22690   22426   22133   21831   21691   20743   20644
## 9     4480    4418    4348    4319    4379    4185    4083
## 10  142790  144395  145787  146634  147937  148319  149171
## 11  112788  114131  115697  117007  117775  121761  124158
## 12    8229    8188    8128    8125    8122    7910    7874
## 13   10784   10821   10919   10937   10827   11277   11377
## 14      88      85      77      65      63      62      56
## 15    1517    1602    1774    1819    1798    2118    2266
## 16   44998   45428   45562   45665   46119   44808   45182
## 17     878     910     955     981     988    1244    1297
## 18     919     894     877     860     850    1038    1085
## 19       0       0       0       0       0       0       0
## 20     873     826     809     789     767     811     766
## 21    3253    3199    3128    3087    3059    2986    2919
## 22    4500    4407    4359    4322    4281    4094    4030
```

Save datasets

``` r
write.csv(nyc_map, "data/output_data/map/nyc_map.csv", row.names = TRUE)
write.csv(nyc_metro_map, "data/output_data/map/nyc_map.csv", row.names = TRUE)
write.csv(nys_map, "data/output_data/map/nys_map.csv", row.names = TRUE)

write.csv(nyc_madsnp, "data/output_data/madsnp/nyc_madsnp.csv", row.names = TRUE)
write.csv(nyc_metro_madsnp, "data/output_data/madsnp/nyc_madsnp.csv", row.names = TRUE)
write.csv(nys_madsnp, "data/output_data/madsnp/nys_madsnp.csv", row.names = TRUE)
```

# Create Plots

``` r
nyc_map_long <-
  nyc_map %>%
   pivot_longer(
    "2021_01":"2023_02",
    names_to = "month",
    values_to = "enrollment",
    names_prefix = "month") 

nyc_map_long %>%  
  ggplot(aes(x = month, y = enrollment, group = plan_name, color = plan_name)) + 
  geom_point() + 
  geom_path() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom", legend.box = "horizontal")
```

![](cms_enrollment_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
  # labs(
    # title = "Proportion of Homicides That are Unsolved in 50 U.S. Cities",
    # x = "City",
    # y = "Proportion")
```
