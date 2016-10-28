#' Prepare Data for MLM
#'
#' \code{mlm_setup} prepares survey data for use in multilevel modeling
#'
#' @param vars a data frame (or, optionally, a csv file) of surveys and item recodes
#' @param datapath path to the directory that houses raw survey datasets
#' @param chime play chime when complete?
#
#' @details \code{mlm_setup}, when passed a data frame of surveys with instructions
#' for recoding variables, recodes and formats the data for multilevel modeling
#'
#' @return a data frame
#'
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom haven read_dta read_sav
#' @importFrom beepr beep
#'
#' @export

mlm_setup <- function(datasets_table,
                      dep_var,
                      chime = TRUE) {
    # Get datasets table
    if ("data.frame" %in% class(datasets_table)) {
        ds <- datasets_table
    } else {
        ds <- read_csv(datasets_table)
    }
    
    l1_vars <- names(ds)[!names(ds) %in%
                             c("survey", "cy_data", "filepath", "id", "wt")]
    
    file_rows <- seq(nrow(ds))

    all_sets <- map_df(file_rows, function(i) {
        cat(i, " ")

        # Get dataset
        fname <- list.files(ds$filepath[i], 
                               pattern = str_c(ds$id[i], ".*(dta|sav|rdata)$"))
        fpath <- str_c(ds$filepath[i], fname)
        if (str_detect(fpath, "dta$")) {
            t_data <- read_dta(fpath)
        } else if (str_detect(fpath, "sav$")) {
            t_data <- read_sav(fpath)
        } else {
            existing_obj <- ls()
            load(fpath)
            new_obj <- ls() 
            new_obj <- new_obj[!new_obj %in% existing_obj]
            t_data <- get(new_obj)
        }
        
        # Fix column names (sometimes necessary)
        valid_column_names <- make.names(names=names(t_data), unique=TRUE, allow_ = TRUE)
        names(t_data) <- valid_column_names
            
        # Get country-years
        cc <- eval(parse(text = ds$cy_data[i]))
        t_data <- suppressMessages(left_join(t_data, cc))
            
        # Get weights
        if (!is.na(ds$wt[i])) {
            if (length(unlist(strsplit(ds$wt[i], split = " "))) == 1) {
                wt <- with(t_data, get(ds$wt[i]))
            } else {
                eval(parse(text = ds$wt[i]))
            }
            t_data$wt_mlm <- wt
            rm(wt)
        } else t_data$wt_mlm <- 1
        
        # Generate level-1 variables
        for (v in l1_vars) {
            t_data[[v]] <- with(t_data, eval(parse(text = ds[i, v])))
        }

        # Drop country-years without DVs
        t_data1 <- t_data %>%
            group_by(c_mlm, y_mlm) %>% 
            mutate_at(dep_var, funs(mean_cy = mean(., na.rm=T))) %>%
            {if (length(dep_var)==1) { 
                filter(., !is.na(mean_cy)) %>% 
                    ungroup() %>% 
                    select(c_mlm, y_mlm, wt_mlm, one_of(l1_vars))
            } else {
                mutate_at(., str_c(dep_var, "_mean_cy"), funs(coalesce(., 0))) %>% 
                    ungroup() %>% 
                    mutate(sum_dv_mlm = rowSums(.[str_c(dep_var, "_mean_cy")])) %>% 
                    filter(sum_dv_mlm > 0) %>% 
                    select(c_mlm, y_mlm, wt_mlm, one_of(l1_vars))
            }}
    })
    
    # Chime
    if(chime) {
        beepr::beep()
    }
    
    return(all_sets)        
}
