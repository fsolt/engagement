library(tidyverse)
library(haven)
library(stringr)

eb_datasets <- read_csv("data-raw/eb_datasets.csv")

data_dir <- "../Data/eb_files/"

files_to_get <- eb_datasets %>% 
    filter(year > 2002) %>% 
    mutate(gesis_id = str_extract(load_cmd, "(?<=ZA)\\d+")) %>% 
    select(gesis_id) %>% 
    unlist()

if (!dir.exists(data_dir)) dir.create(data_dir)
s <- login(username = getOption("gesis_user"),
           password = getOption("gesis_pass"))
walk(files_to_get, function(id) {
    id_dir <- str_c(data_dir, "ZA", id)
    if (!dir.exists(id_dir)) dir.create(id_dir)
    download_dataset(s, id, path = id_dir)
    try(download_codebook(id, path = id_dir))
})

id <- "3521"
id_dir <- str_c(data_dir, "ZA", id)
if (!dir.exists(id_dir)) dir.create(id_dir)
download_dataset(s, id, path = id_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)

