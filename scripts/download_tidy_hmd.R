rm(list = ls())

pacman::p_load(
  tidyverse,
  stringr
)

# Download file from 

# http://www.mortality.org/cgi-bin/hmd/hmd_download.php

# Bottom link - 'All countries for the HMD'

# unzip file 

# dir.create("data/hmd/unzipped")
# unzip(
#   "data/hmd/zipped/hmd_countries.zip",
#   exdir="data/hmd/unzipped"
#   )


# Files to extract are 
# Exposures_1x1.txt
# Deaths_1x1.txt
# Population.txt

# Each time in [COUNTRY]/STATS

# Where [COUNTRY] is the directory name 


outer_dir_loc <- "data/hmd/unzipped"

#country codes 

# To get the list of directory names 

hmd_codes <- dir(outer_dir_loc)

exposure_loc <- file.path(outer_dir_loc, hmd_codes, "STATS", "Exposures_1x1.txt")
deaths_loc <- file.path(outer_dir_loc, hmd_codes, "STATS", "Deaths_1x1.txt")
population_loc <- file.path(outer_dir_loc, hmd_codes, "STATS", "Population.txt")

load_and_tidy <- function(x){
  dta <- read_table(x, skip = 2)
  
  dta %>% 
    gather(key = "sex", value = "count", Female:Total) %>% 
    mutate(age = Age %>% str_replace("\\+", "") %>% as.integer) %>% 
    mutate(sex = tolower(sex)) %>% 
    select(year = Year, age, sex, count)
}

hmd_loc_df <- data_frame(
  code = hmd_codes, exposure_loc, deaths_loc, population_loc
) %>% 
  mutate(
    deaths = map(deaths_loc, load_and_tidy),
    population = map(population_loc, load_and_tidy),
    exposure = map(exposure_loc, load_and_tidy)
  ) 

# reduce using full_join

# start with list of exposure, deaths, population from first country 

# let's try to join two together first 

hmd_loc_df %>% 
  mutate(
    dp_joined = map2(
      deaths, population, 
      function(x, y){
        x %>% 
          rename(death_count = count) %>% 
          full_join(
            y %>% rename(population_count = count)
          )
      }),
    all_joined = map2(
      dp_joined, exposure, 
      function(x, y){
        x %>% 
          full_join(
            y %>% rename(exposure = count)
          )
      }
      )
  ) %>% 
  select(code, all_joined) %>% 
  unnest() -> hmd_tidied

#dir.create("data/tidied/")

hmd_tidied %>% write_csv("data/tidied/hmd_tidied.csv")



# Extract full names  -----------------------------------------------------

# For each country code 
# Select exposure_1x1.txt file 
# Read in first line
# keep content before first comma

df_names <- data_frame(
  code = hmd_codes,
  exposure_loc = exposure_loc
) %>% 
  mutate(
    first_line = map_chr(exposure_loc, readLines, n = 1)
  ) %>% 
  mutate(
    population_name = str_extract(first_line, "^[^,]{1,}")
  ) %>% 
  select(code, population_name)

# Save this 

df_names %>% write_csv("data/tidied/code_lookup.csv")

