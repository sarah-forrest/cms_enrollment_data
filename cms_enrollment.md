CMS Monthly Enrollment Data Extraction - All months
================
Sarah Forrest

``` r
library(tidyverse)
library(stringr)
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


df_name_map = str_c(yyyy_mm, "_map")
df_name_madsnp = str_c(yyyy_mm, "_madsnp")

assign(x = df_name_map, value = map_blank, envir = globalenv())
assign(x = df_name_madsnp, value = madsnp_blank, envir = globalenv())

save_path_map = str_c("data/output_data/map/", df_name_map, ".csv")
save_path_madsnp = str_c("data/output_data/madsnp/", df_name_madsnp, ".csv")

write.csv(map_blank ,save_path_map, row.names = TRUE)
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
