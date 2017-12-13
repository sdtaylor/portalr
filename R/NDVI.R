#Functions for retrieving and processing NDVI data for Portal.
#Currently only GIMMS is supported, with years 1981-2013 available.
# This code is modified from get_ndvi_data.R script in the Weecology repo: BBS-forecasting
#(https://github.com/weecology/bbs-forecasting/blob/master/R/get_ndvi_data.R)
#
#GIMMS: Call get_gimms_ndvi() to retrieve a dataframe of (year, month, ndvi).
#-It will take a while to download the 14gb of data and extract values the 1st time around. Results are stored in an sqlite db for quick access.
#-To redo it just delete the database file.
#-na values are due to missing periods from filtering out unwanted quality flags.

#################################################
#This assumes we want all gimms files that are available. It queries for files
#that are available for download and compares against files already in the gimms
#data directory.
#Returns a list of files to download, which may be length 0.
##################################################
get_gimms_download_list=function(gimms_folder = './GIMMS'){
  available_files_download_path=gimms::updateInventory(version=0)
  available_files_name=basename(available_files_download_path)

  files_present=list.files(gimms_folder)
  #hdr files are created from some of the gimms processing that we don't want to
  #use here.
  files_present=files_present[!grepl('hdr', files_present)]

  to_download=available_files_download_path[! available_files_name %in% files_present]

  return(to_download)
}

################################################
#Extract values from a single gimms file given a set of coordinates.
#Excludes values which don't meet NDVI quality flags.
#From the GIMMS readme:
#FLAG = 7 (missing data)
#FLAG = 6 (NDVI retrieved from average seasonal profile, possibly snow)
#FLAG = 5 (NDVI retrieved from average seasonal profile)
#FLAG = 4 (NDVI retrieved from spline interpolation, possibly snow)
#FLAG = 3 (NDVI retrieved from spline interpolation)
#FLAG = 2 (Good value)
#FLAG = 1 (Good value)
################################################
#' @importFrom gimms rasterizeGimms
extract_gimms_data=function(gimms_file_path, site){
  gimmsRaster=rasterizeGimms(gimms_file_path, keep=c(1,2,3))
  ndvi=raster::extract(gimmsRaster, site, buffer=4000)
  ndvi=as.numeric(lapply(ndvi, mean, na.rm=TRUE))

  year=as.numeric(substr(basename(gimms_file_path), 4,5))
  month=substr(basename(gimms_file_path), 6,8)
  day=substr(basename(gimms_file_path), 11,11)

  #Convert the a b to the 1st and 15th
  day=ifelse(day=='a',1,15)

  #Convert 2 digit year to 4 digit year
  year=ifelse(year>50, year+1900, year+2000)

  return(data.frame(year=year, month=month, day=day, ndvi=ndvi, stringsAsFactors = FALSE))
}

################################################
#Extract the NDVI time series for all bbs routes
#from all years of gimms data
################################################
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom sp coordinates<-
process_gimms_ndvi=function(gimms_folder = './GIMMS'){
  long = -109.08029
  lat = 31.937769
  site <- data.frame(long,lat)
  coordinates(site) <- c("long", "lat")

  gimms_files=list.files(gimms_folder, full.names = TRUE)
  #hdr files are created from some of the gimms processing that we don't want to
  #use here.
  gimms_files=gimms_files[!grepl('hdr', gimms_files)]

  gimms_ndvi=data.frame()
  for(file_path in gimms_files){
    gimms_ndvi=extract_gimms_data(file_path, site) %>%
      bind_rows(gimms_ndvi)
  }

  #Get a single value per site/month/year. NA values
  #are kept. These are from where the quality flag was not met.
  gimms_ndvi = gimms_ndvi %>%
    group_by(year, month) %>%
    summarize(ndvi=mean(ndvi, na.rm=TRUE)) %>%
    ungroup()

  return(gimms_ndvi)
}

#################################################
#Get the GIMMS AVHRR ndvi bi-monthly time series for every bbs site.
#Pulling from the sqlite DB or extracting it from raw gimms data if needed.
#################################################
#' @importFrom dplyr sql src_sqlite src_tbls tbl copy_to collect
#' @importFrom gimms downloadGimms
get_gimms_ndvi = function(gimms_folder = './GIMMS'){
  dir.create(gimms_folder, showWarnings = FALSE, recursive = TRUE)

  if (db_engine(action='check', table_to_check = 'gimms_ndvi')){
    return(db_engine(action='read', sql_query='SELECT * from gimms_ndvi'))
  } else {
    print('Gimms NDVI data not found, processing from scratch')

    files_to_download=get_gimms_download_list(gimms_folder = gimms_folder)
    if(length(files_to_download)>0){
      print('Downloading GIMMS data')
      downloadGimms(x=files_to_download, dsn=gimms_folder)
    }

    gimms_ndvi_data=process_gimms_ndvi(gimms_folder = gimms_folder)

    db_engine(action='write', df=gimms_ndvi_data, new_table_name = 'gimms_ndvi')

    return(gimms_ndvi_data)
  }
}

#' Single wrapper for all database actions
#'
#' We require only a few simple sql methods. They are 1. Writing an entire dataframe
#' directly to a database as it's own table, 2. Reading the same tables as dataframes,
#' possibly with some modification using SQL statements, 3. Checking to see if a
#' table exists. If a particular table does it exists it's assumed it has all data
#' required.
#'
#' read returns a dataframe
#' write returns nothing
#' check returns boolean
#'
#' @param action Action to perform in db call. Either read, write, or check
#' @param db name of database. A file if using sqlite
#' @param sql_query SQL statement if action is read
#' @param df Dataframe of data if action is write. Will copy the dataframe verbatim to it's own table with name new_table_name
#' @param new_table_name Table name for new data being written
#' @param table_to_check Table name to check if it exists for when action is check
#' @importFrom dplyr copy_to src_sqlite src_tbls collect tbl
db_engine=function(action, db='./GIMMS/gimms_ndvi.sqlite', sql_query=NULL,
                   df=NULL, new_table_name=NULL, table_to_check=NULL){

  if(!dir.exists("GIMMS")){dir.create("GIMMS")}

  con <- src_sqlite(db, create=TRUE)

  if(action=='read'){
    to_return=collect(tbl(con, sql(sql_query)), n=Inf)

  } else if(action=='write') {
    copy_to(con, df, name=new_table_name, temporary = FALSE)
    to_return=NA

  } else if(action=='check') {
    #Only works with sqlite for now.
    to_return=tolower(table_to_check) %in% tolower(src_tbls(con))

  } else {
    stop(paste0('DB action: ',action,' not found'))
  }

  #Close the connection before returning results.
  rm(con)
  return(to_return)
}

###############################
#MASTER CODE FOR CALLING THE FUNCTIONS
###############################
library(dplyr)
library(gimms)
library(sp)
library(rgdal)
library(dbplyr)
library(RSQLite)

test = get_gimms_ndvi(gimms_folder = '/home/shawn/data/gimms/')
