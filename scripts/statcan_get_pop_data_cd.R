# script to 
# - call Census District population data from Statistics Canada
# - filter for Vancouver Island

### --- SETUP

# package setup
library(tidyverse)

# cansim
# reference page: https://mountainmath.github.io/cansim/index.html
library(cansim) 

# create list of Vancouver Island CDs numbers
vi_cd_list <- c("17", "19", "21", "23", "24", "26", "27", "43", "45")

### --- DATA CALL

pop_cd <- get_cansim("17-10-0139-01")

# filter for Vancouver Island CDs 
pop_cd_vi <- pop_cd %>%
  # filter for BC, total population
  filter(str_detect(GeoUID, "^59")) %>%
  filter(Sex == "Both sexes" &
           `Age group` == "All ages") %>% 
  # filter for Vancouver Island
  # - create separate variable with just the CD number
  mutate(geo_cd = as.character(str_sub(GeoUID, start = 3, end = 4)))  %>% 
  filter(geo_cd %in% vi_cd_list)

# split GEO into regional district and province
pop_cd_vi <- pop_cd_vi %>% 
  tidyr::separate_wider_delim(GEO, delim = ", ", names = c("regional_district", "province"))

# remove unnecessary columns, reshape into wide format
pop_cd_vi_wide <- pop_cd_vi %>% 
  select(
    REF_DATE, 
    regional_district, 
    GeoUID,
    VALUE
  ) %>% 
  pivot_wider(names_from = REF_DATE, values_from = VALUE)


