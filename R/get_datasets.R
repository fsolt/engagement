library(tidyverse)
library(haven)
library(stringr)
library(gesis)

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

id <- "3521" # 1970-2002 Trend File
id_dir <- str_c(data_dir, "ZA", id)
if (!dir.exists(id_dir)) dir.create(id_dir)
download_dataset(s, id, path = id_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)

