library(tidyverse)
library(haven)
library(stringr)
library(gesis)

# Eurobarometer--------

# Discussion: 3904, 3938, 4056, 4229, 4411, 4414, 4506, 4526, 4530,
# 4565, 4744, 4819
# Interest: 4744, 4973, 5481, 5567, 5612, 5685 <- Actually, these are not consistent with polint4 :(

files_to_get <- c("3904", "3938", "4056", "4229", "4411", "4414", "4506", "4526",
                  "4530", "4565", "4744", "4819", "4973", "5481", "5567", "5612",
                  "5685")

data_dir <- "~/Documents/Projects/Data/dcpo_surveys/"

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
# Download WVS_Longitudinal_1981-2014_rdata_v_2015_04_18 
# from http://www.worldvaluessurvey.org/WVSDocumentationWVL.jsp
# to "~/Documents/Projects/Data/wvs_files/", unzip, load,
# and then *resave* with save(); over 30x smaller and *much* faster

# EVS------
id <- "4804" # Longitudinal Data File 1981-2008
id_dir <- str_c(data_dir, "gesis_files/evs_files/ZA", id)
if (!dir.exists(id_dir)) dir.create(id_dir, 
                                    showWarnings = FALSE, 
                                    recursive = TRUE)
download_dataset(s, id, path = id_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)
try(download_codebook(id, path = id_dir))

# ISSP-----
files_to_get <- c(3950, 6670) # Citizenship I & II
s <- login(username = getOption("gesis_user"),
           password = getOption("gesis_pass"))
walk(files_to_get, function(id) {
    id_dir <- str_c(data_dir, "gesis_files/issp_files/ZA", id)
    if (!dir.exists(id_dir)) dir.create(id_dir, 
                                        showWarnings = FALSE, 
                                        recursive = TRUE)
    download_dataset(s, id, path = id_dir)
    try(download_codebook(id, path = id_dir))
})

# ESS------
# none of the following works (yet)
# set up directory
id_dir <- str_c(data_dir, "ess_files/")
if (!dir.exists(id_dir)) dir.create(id_dir, 
                                    showWarnings = FALSE, 
                                    recursive = TRUE)



# ess_combo
ess1_6 <- "http://www.europeansocialsurvey.org/downloadfile/esscumulative1-6e01_1.zip"

s <- html_session("http://www.europeansocialsurvey.org/user/login")
form <- html_form(s)[[2]] %>% 
    set_values(u = getOption("ess_email"))
suppressMessages(output <- submit_form(s, form) %>%
                     stop_for_status() %>% 
                     jump_to(ess1_6) %>% 
                     submit_form(form))
file_dir <- str_c(id_dir, "esscumulative1-6e01_1.zip")
writeBin(httr::content(output$response, "raw"), file_dir)
writeBin(ess1_6, str_c(id_dir, "esscumulative1-6e01_1.zip"))

# ess7
ess7 <- "http://www.europeansocialsurvey.org/file/download?f=ESS7e02.stata.zip&c=&y=2014"
output <- logged_in %>% 

    # log in
    s <- html_session("http://www.europeansocialsurvey.org/user/login")
form <- html_form(s)[[2]] %>% 
    set_values(u = getOption("ess_email"))
suppressMessages(output <- submit_form(s, form) %>%
                     stop_for_status() %>% 
                     jump_to(ess7) %>% 
                     submit_form(form))

file_dir <- file.path(id_dir, "ESS7e02.stata.zip")
writeBin(httr::content(ess7, "raw"), file_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)

s <- html_session(ess7)
form <- html_form(s)[[2]] %>% 
    set_values(u = getOption("ess_email"))

file_dir <- file.path(id_dir, "ESS7e02.stata.zip"))

# EES ----
files_to_get <- c(5055, 5160) # EES 2009 & 2014
s <- login(username = getOption("GESIS_USER"),
           password = getOption("GESIS_PASS"))
walk(files_to_get, function(id) {
    id_dir <- str_c(data_dir, "gesis_files/ees_files/ZA", id)
    if (!dir.exists(id_dir)) dir.create(id_dir, 
                                        showWarnings = FALSE, 
                                        recursive = TRUE)
    download_dataset(s, id, path = id_dir)
    try(download_codebook(id, path = id_dir))
})

# ees trend
ees_trend <- "http://www.tcd.ie/Political_Science/staff/michael_marsh/trend_ees_stata.zip"

id <- "ees_trend" # 1989, 1994, 1999, 2004
id_dir <- file.path(data_dir, "misc_files", "ees_files", id)
file_dir <- file.path(id_dir, "trend_ees_stata.zip")
if (!dir.exists(id_dir)) dir.create(id_dir,
                                    showWarnings = FALSE,
                                    recursive = TRUE)
download.file(ees_trend, file_dir)
zip_name <- list.files(id_dir, pattern = "zip$")[[1]]
unzip(file.path(id_dir, zip_name), exdir = id_dir)

download.file("http://www.tcd.ie/Political_Science/staff/michael_marsh/codebook.pdf", file.path(id_dir, "trend_ees_codebook.pdf"))
