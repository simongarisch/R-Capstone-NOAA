#library(readr)
#library(magrittr)
#library(dplyr)
#library(stringr)

# https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
# devtools::document()
# webpage: https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
# download entire significant earthquake data file in tab-delimited format:
# https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt
# you can view all of the data from the NOAA website
# notice that many earthquakes do not have all of the date fields (some are blank)

# Instructions:
# cleans the raw data collected from NOAA...
# 1) A date column created by uniting the year, month, day and converting it to the Date class
# 2) LATITUDE and LONGITUDE columns converted to numeric class
# 3) In addition, write a function eq_location_clean() that cleans the LOCATION_NAME column

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#' @title Load the raw NOAA data
#'
#' @description This will load the raw tab delimited data previously downloaded from the NOAA website
#' which is currently sitting in the data folder.
#'
#' @param path A character string file path to your downloaded data source. Default is "data/signif.txt"
#'
#' @return Returns a dataframe result after calling readr's read_delim
#'
#' @importFrom readr read_delim
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data()
#'   print(class(df))
#'   head(df)
#' }
#'
load_data <- function(path=file.path("data_raw", "signif.txt")){
  df <- readr::read_delim(path, delim="\t")
}

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#' @title Create a date from NOAA data given DAY, MONTH, YEAR
#'
#' @description The NOAA data has columns called DAY, MONTH, YEAR. Part of the requirements for
#' cleaning this data is to create a date column. All of the years are provided, but some of
#' the months and days are NA. We'll replace NA days with 1st and NA months with Jan.
#' Also, note that as.Date does not play nice with negative years, so these have been removed.
#' Vectors days, months and years must be of the same length.
#'
#' @param days Day of the year as integer vector
#' @param months Month of the year as integer vector
#' @param years An integer vector of years
#'
#' @return A vector of the date objects returned from passing date strings to as.Date
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data() %>% dplyr::filter(YEAR >= 0) %>%
#'        dplyr::mutate(date = get_date(DAY, MONTH, YEAR))
#'   print(class(df))
#'   head(df)
#' }
#'
get_date <- function(days, months, years){
  #sum(is.na(df$YEAR))  # [1] 0
  #sum(is.na(df$DAY))   # [1] 557
  #sum(is.na(df$MONTH)) # [1] 405
  n <- length(days)
  dates <- seq(as.Date(Sys.Date()), by=0, len=n)
  for(i in 1:n){
    day <- days[i]
    month <- months[i]
    year <- years[i]
    if(is.na(day)){day <- 1}
    if(is.na(month)){month <- 1}
    date_str = paste(year, month, day, sep="-")
    dates[i] <- as.Date(date_str)
  }
  return(dates)
}

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#' @title Clean the raw NOAA earthquake data
#'
#' @description This function takes the raw NOAA data and cleans it as per the requirements from
#' Coursera: 1) Creates a date column by uniting the year, month, day and converting it
#' to the Date class. 2)  The LATITUDE and LONGITUDE columns are converted to a numeric class.
#' 3) A function eq_location_clean() is applied that cleans the LOCATION_NAME column.
#' We have also cleared any years that are negative as these create issues with as.Date.
#'
#' @param df_raw A dataframe of raw NOAA data
#'
#' @return Returns a cleaned dataframe
#'
#' @importFrom dplyr filter, mutate
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data() %>% dplyr::filter(YEAR >= 0) %>%
#'        dplyr::mutate(date = get_date(DAY, MONTH, YEAR))
#'   print(class(df))
#'   head(df)
#' }
#'
eq_clean_data <- function(df_raw){
  df <- df_raw
  df <- df %>% dplyr::filter(YEAR >= 0) %>%
    dplyr::mutate(date = get_date(DAY, MONTH, YEAR)) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
    eq_location_clean()
  return(df)
}

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#' @title Cleans the LOCATION_NAME column
#'
#' @description As per the instructions on Coursera: write a function eq_location_clean() that
#' cleans the LOCATION_NAME column by stripping out the country name (including the colon) and
#' converts names to title case (as opposed to all caps). This will be needed later for
#' annotating visualizations.
#'
#' @param df a dataframe containing a column LOCATION_NAME that will be cleaned
#'
#' @return Returns a cleaned dataframe as described
#'
#' @importFrom dplyr filter, mutate
#' @importFrom stringr str_trim, str_to_title
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data() %>% eq_location_clean()
#'   print(class(df))
#'   head(df)
#' }
#'
eq_location_clean <- function(df){
  df <- df %>% dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_trim(gsub(".*:","", LOCATION_NAME))) %>%
    dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_to_title(CLEAN_LOCATION_NAME))
  return(df)
}

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#if(!dir.exists("data_raw")){
#  setwd('..') # go back to package directory
#}

#df_earthquakes <- load_data() %>% eq_clean_data()
#if(!file.exists(file.path("data", "df_earthquakes.rda"))){
#  # package this cleaned data as part of the project
#  library(devtools)
#  devtools::use_data(df_earthquakes)
#}

#print(names(df))
#print(head(df[c("YEAR", "MONTH", "DAY", "date")], 20)) # checking the date cleaning
#print(tail(df[c("YEAR", "MONTH", "DAY", "date")], 20))
#print("****************")
#print(head(df[c("LOCATION_NAME", "CLEAN_LOCATION_NAME")], 20)) # and for the location name
#print(tail(df[c("LOCATION_NAME", "CLEAN_LOCATION_NAME")], 20))
