library(tidyverse)
library(haven)
library(stringr)
library(gesis)

# Eurobarometer--------

# Discussion: 3904, 3938, 4056, 4229, 4411, 4414, 4506, 4526, 4530,
# 4565, 4744, 4819
# Interest: 4744, 4973, 5481, 5567, 5612, 5685

files_to_get <- c("3904", "3938", "4056", "4229", "4411", "4414", "4506", "4526",
                  "4530", "4565", "4744", "4819", "4973", "5481", "5567", "5612",
                  "5685")

data_dir <- "~/Documents/Projects/Data/"

if (!dir.exists(data_dir)) dir.create(data_dir, 
                                      showWarnings = FALSE, 
                                      recursive = TRUE)
s <- login(username = getOption("gesis_user"),
           password = getOption("gesis_pass"))
walk(files_to_get, function(id) {
    id_dir <- str_c(data_dir, "gesis/eb_files/ZA", id)
    if (!dir.exists(id_dir)) dir.create(id_dir)
    download_dataset(s, id, path = id_dir)
    try(download_codebook(id, path = id_dir))
})

id <- "3521" # 1970-2002 Trend File
id_dir <- str_c(data_dir, "gesis/eb_files/ZA", id)
if (!dir.exists(id_dir)) dir.create(id_dir, showWarnings = FALSE)
download_dataset(s, id, path = id_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)

# WVS------
# Download WVS_Longitudinal_1981-2014_stata_dta_v_2015_04_18 
# from http://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp
# to "~/Documents/Projects/Data/wvs_files/"

# EVS------
id <- "4804" # Longitudinal Data File 1981-2008
id_dir <- str_c(data_dir, "gesis/evs_files/ZA", id)
if (!dir.exists(id_dir)) dir.create(id_dir, 
                                    showWarnings = FALSE, 
                                    recursive = TRUE)
download_dataset(s, id, path = id_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)
try(download_codebook(id, path = id_dir))

# ESS------
# set up directory
id_dir <- str_c(data_dir, "ess_files/")
if (!dir.exists(id_dir)) dir.create(id_dir, 
                                    showWarnings = FALSE, 
                                    recursive = TRUE)

# log in
s <- html_session("http://www.europeansocialsurvey.org/user/login")
form <- html_form(s)[[2]] %>% 
    set_values(u = getOption("ess_email"))
suppressMessages(output <- submit_form(s, form) %>%
                     stop_for_status() %>% 
                     jump_to(ess7) %>% 
                     submit_form(form))

# ess7
ess7 <- "http://www.europeansocialsurvey.org/file/download?f=ESS7e02.stata.zip&c=&y=2014"
output <- logged_in %>% 
    
    file_dir <- file.path(id_dir, "ESS7e02.stata.zip")
writeBin(httr::content(ess7, "raw"), file_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)

s <- html_session(ess7)
form <- html_form(s)[[2]] %>% 
    set_values(u = getOption("ess_email"))


file_dir <- file.path(id_dir, "ESS7e02.stata.zip"))

# ess cumulative
ess1_6 <- "http://www.europeansocialsurvey.org/downloadfile/esscumulative1-6e01_1.zip"

s <- html_session("http://www.europeansocialsurvey.org/user/login")
form <- html_form(s)[[2]] %>% 
    set_values(u = getOption("ess_email"))
suppressMessages(output <- submit_form(s, form) %>%
                     stop_for_status() %>% 
                     jump_to(ess1_6))

file_dir <- str_c(id_dir, "esscumulative1-6e01_1.zip")
writeBin(httr::content(output$response, "raw"), file_dir)
writeBin(ess1_6, str_c(id_dir, "esscumulative1-6e01_1.zip"))
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(str_c(id_dir, "esscumulative1-6e01_1.zip"), exdir = id_dir)
