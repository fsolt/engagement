library(tidyverse)
library(stringr)
library(haven)
library(beepr)
library(rvest)

short_country_names <- function(cname) {
    countrycode::countrycode(cname, "country.name", "country.name") %>%
    str_replace(", .*", "") %>% 
    str_replace("^Republic of ", "") %>% 
    str_replace(" of [GA].*", "")
}


# Load data
ds1 <- read_csv("~/Documents/Projects/engagement/data-raw/datasets_table1.csv") %>% 
    select(-disc3) %>% 
    filter(!int4=="NA_real_")

l1_data_all <- mlm_setup(datasets_table = ds1,
                         dep_var = "int4",
                         chime = TRUE)


# Define universe: democracies without substantial vote-buying or coercion
# EU + OECD + Taiwan - Turkey - Mexico

oecd_members <- read_html("https://en.wikipedia.org/wiki/Organisation_for_Economic_Co-operation_and_Development") %>% 
    html_table(fill=TRUE) %>%   # generates a list
    nth(6) %>%                  # get sixth element of the list
    as_tibble() %>%             # make it a tibble (data_frame)
    select(Country) %>% 
    unlist() %>% 
    str_trim()

eu_members <- read_html("https://en.wikipedia.org/wiki/Member_state_of_the_European_Union") %>%
    html_table(fill=TRUE) %>%   # generates a list
    nth(2) %>%                  # get second element of the list
    as_tibble() %>%             # make it a tibble (data_frame)
    select(`Country name`) %>% 
    unlist() %>% 
    str_trim() %>% 
    str_replace("\\[.*\\]", "")

c_universe <- c(eu_members, oecd_members, "Taiwan") %>% 
    unique() %>%                    
    short_country_names() %>% 
    setdiff(., c("Mexico", "Turkey"))   # exclude for vote-buying/coercion; 40 countries left

l1_data <- l1_data_all %>% 
    filter(c_mlm %in% c_universe)   # all 40 are represented (but not equally)

l1_data %>%
    group_by(c_mlm) %>% 
    count(y_mlm) %>% 
    ungroup() %>% 
    count(c_mlm) %>% 
    ungroup() %>% 
    arrange(nn) %>%
    mutate(c_mlm = str_replace(c_mlm, ",.*", "")) %>% 
    mutate(c_mlm = factor(c_mlm, levels = c_mlm[order(nn, decreasing = TRUE)])) %>%
    ggplot(aes(x = c_mlm, y = nn)) +
        geom_bar(fill = "#011993", stat = "identity") +
        labs(x = NULL, y=NULL) +
        theme_bw() +
        theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
        ggtitle("Count of Country-Years by Country")


# Get contextual data

dir.create("data-raw/context/desaw/", recursive = TRUE, showWarnings = FALSE)
desaw <- "http://mattgolder.com/files/research/es_data-v3.zip"
zip_path <- "data-raw/context/desaw/es_data-v3.zip"
download.file(desaw, zip_path)
unzip(file.path(zip_path), exdir = str_extract(zip_path, ".*/"))

desaw <- read_csv("data-raw/context/desaw/es_data-v3.csv") %>% 
    mutate(c_mlm = countrycode(country, "country.name", "country.name"),
           y_mlm = year) %>% 
    filter(c_mlm %in% c_universe)

l2_data <- desaw %>%
    filter(y_mlm >= min(l1_data$y_mlm) - 5) %>% 
    mutate(election_year = 1) %>% 
    select(c_mlm, y_mlm, legislative_type, elecrule, enep1, election_year, date) %>% 
    mutate(prop_rep = as.numeric(legislative_type==2 | elecrule==11),
           enep = enep1) %>% 
    complete(c_mlm, y_mlm) %>%
    mutate(election_year = recode(election_year, .missing = 0)) %>%
    select(c_mlm, y_mlm, prop_rep, enep, election_year) %>% 
    fill(prop_rep, enep) %>% 
    distinct()

l3_data <- read_csv("data-raw/context/l3_data.csv")


l123_data <- l1_data %>% 
    left_join(l2_data, by = c("c_mlm", "y_mlm")) %>%
    left_join(l3_data, by = "c_mlm")


# Impute missing values

mdf <- suppressMessages(
    p2007_cnty %>% select(rej_merit, gini_cnty, income, 
                          educ, age, male, noncitizen, 
                          latino, black, asian, other_min,
                          partyid_rep, ideo_con, attend,
                          income_cnty, black_cnty, perc_bush04,
                          pop_cnty, fips) %>% 
        missing_data.frame()
)
mdf <- change(mdf, y = "fips", what = "type", to = "irrelevant")
mdf <- change(mdf, y = c("income", "educ", "attend"), what = "type", to = "ordered-categorical")
mdf <- change(mdf, y = "age", what = "type", to = "bounded-continuous", lower=18, upper=97)

mdf_mi <- mi(mdf, seed = 324) 
mdf_mi_list <- complete(mdf_mi, m=10)
mdf_mi_list <- lapply(mdf_mi_list, function(df) 
    sapply(df, function(v) 
        if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>%
        data.frame) # get rid of %&*(&^ factors
mdf_mi_list <- imputationList(mdf_mi_list)