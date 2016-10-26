#' Prepare Data for MLM
#'
#' \code{mlm_setup} prepares survey data for use in multilevel modeling
#'
#' @param vars a data frame (or, optionally, a csv file) of survey items
#' @param datapath path to the directory that houses raw survey datasets
#' @param chime play chime when complete?
#
#' @details \code{mlm_setup}, when passed a data frame of survey items, collects the
#' responses and formats them for use with the \code{dcpo} function.
#'
#' @return a data frame
#'
#' @import foreign
#' @import haven
#' @import readr
#' @import reshape2
#' @import dplyr
#' @import beepr
#' @import Hmisc::spss.get
#'
#' @export

mlm_setup <- function(datasets_table,
                      dep_var,
                      chime = TRUE) {
    # Get datasets table
    if ("data.frame" %in% class(vars)) {
        ds <- datasets_table
    } else {
        ds <- read_csv(datasets_table)
    }
    
    l1_vars <- names(ds)[!names(ds) %in%
                             c("survey", "cy_data", "filepath", "id", "wt")]
    
    # file_rows <- seq(nrows(ds))
    file_rows <- 1
    
    # for (i in seq(nrows(ds))) {
    all_sets <- map_df(file_rows, function(i) {
        cat(i, " ")
        # v <- vars_table[i, ]
        # ds <- datasets_table[datasets_table$survey==v$survey, ]
        
        # Get dataset
        fname <- list.files(ds$filepath[i], 
                               pattern = str_c(ds$id, ".*(dta|sav)$"))
        fpath <- str_c(ds$filepath[i], fname)
        if (str_detect(fpath, "dta$")) {
            t_data <- read_dta(fpath)
        } else {
            t_data <- read_sav(fpath)
        }
        
        # Fix column names (sometimes necessary)
        valid_column_names <- make.names(names=names(t_data), unique=TRUE, allow_ = TRUE)
        names(t_data) <- valid_column_names
            
        # Get country-years
        cc <- eval(parse(text = ds$cy_data[i]))
        t_data <- left_join(t_data, cc)
            
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
            mutate_at(str_c(dep_var, "_mean_cy"), funs(coalesce(., 0))) %>%
            ungroup() %>% 
            mutate(sum_dv_mlm = rowSums(.[str_c(dep_var, "_mean_cy")])) %>% 
            filter(sum_dv_mlm > 0) %>% 
            select(c_mlm, y_mlm, wt_mlm, one_of(l1_vars))

    })
 
    # gen income5 from income
    

    rm(list = c("t_data", "cc", "ds", "v"))
    
    for (i in seq(length(all_sets))) {
        add <- melt(all_sets[i], id.vars = c("country", "year", "survey", "n",
                                             "cutpoint", "variance"), na.rm=T)
        if (i == 1) all_data <- add else all_data <- rbind(all_data, add)
    }
    rm(add)
    all_data$y_r = with(all_data, as.integer(round(n * value))) # number of 'yes' response equivalents, given data weights
    
    all_data2 <- all_data %>% select(-value, -L1, -survey) %>%
        group_by(country, year, variable, cutpoint, variance) %>%
        summarize(y_r = sum(y_r),     # When two surveys ask the same question in
                  n = sum(n)) %>%     # the same country-year, add samples together
        ungroup() %>%
        group_by(country) %>%
        mutate(cc_rank = n(),         # number of country-year-item-cuts (data-richness)
               firstyr = first(year, order_by = year),
               lastyr = last(year, order_by = year)) %>%
        ungroup() %>%
        arrange(desc(cc_rank), country, year) %>% # order by data-richness
        # Generate numeric codes for countries, years, questions, and question-cuts
        mutate(variable_cp = paste(variable, cutpoint, sep="_gt"),
               ccode = as.numeric(factor(country, levels = unique(country))),
               tcode = as.integer(year - min(year) + 1),
               qcode = as.numeric(factor(variable, levels = unique(variable))),
               rcode = as.numeric(factor(variable_cp, levels = unique(variable_cp)))) %>%
        arrange(ccode, tcode, qcode, rcode)
    
    # Chime
    if(chime) {
        beep()
    }
    
    return(all_data2)
}
