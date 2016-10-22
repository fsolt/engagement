library(tidyverse)
library(haven)
library(stringr)

eb_datasets <- read_csv("data-raw/eb_datasets.csv")

data_dir <- "../Data/eb_files/"

# files_to_get <- eb_datasets %>% 
#     filter(year > 2002) %>% 
#     mutate(gesis_id = str_extract(load_cmd, "(?<=ZA)\\d+")) %>% 
#     select(gesis_id) %>% 
#     unlist()

# Discussion: 3904, 3938, 4056, 4229, 4411, 4414, 4506, 4526, 4530,
# 4565, 4744, 4819
# Interest: 4744, 4973, 5481, 5567, 5612, 5685

files_to_get <- c("3904", "3938", "4056", "4229", "4411", "4414", "4506", "4526",
                  "4530", "4565", "4744", "4819", "4973", "5481", "5567", "5612",
                  "5685")

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


q_to_get <- c("5686", "5689", "5852", "5875", "5876", "5877", "5878", "5913",
            "5913", "5914", "5928","5929", "5930", "5931", "5932", "5933",
            "5964", "5965", "5998", "6595", "6596", "6642", "6643", "6644")

download_bq <- function (doi, path = ".", quiet = FALSE) {
    for (d in doi) {
        url <- paste0("https://dbk.gesis.org/dbksearch/SDesc2.asp?db=E&no=", 
                      d)
        page <- read_html(url)
        node <- html_nodes(page, xpath = "//a[contains(text(), '_bq')]")
        node <- paste0("https://dbk.gesis.org/dbksearch/", html_attr(node, 
                                                                     "href"))
        resp <- GET(node)
        if (!quiet) 
            message("Downloading questionnaire for DOI: ", d)
        filename <- gsub("^.*?\"|\"", "", resp$headers$`content-disposition`)
        filename <- file.path(path, filename)
        writeBin(content(resp, "raw"), filename)
    }
}

library(rvest)
library(httr)
library(xml2)
walk(q_to_get, function(id) {
    id_dir <- str_c(data_dir, "ZA", id)
    try(download_bq(id, path = id_dir))
})
