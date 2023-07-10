# Functions related to generating, reading and writing queries

#' Move a query file.
#'
#' @description A wrapper for file.copy.
#'
#' @param from_file Full file path to the file to be moved.
#' @param to_dir Full path to the desired destination directory.
#' @param delete_original TRUE/FALSE to remove the original file.
#'
#' @noRd
#'
move_query <- function(from_file,
                       to_dir,
                       delete_original = TRUE){
  # Test for presence of slash at end of directory
  if(substr(to_dir, nchar(to_dir)-1+1, nchar(to_dir)) != "/"){
    to_dir <- paste0(to_dir,"/")
  }
  # Trim the filename
  file_short <- trim_path(filename = from_file)
  # Add it to the to_dir
  to_file = paste0(to_dir,file_short)
  # Copy!
  file.copy(from = from_file,
            to = to_file)
  # Delete, if asked.
  if(isTRUE(delete_original)){
    file.remove(from_file)
  }
}

#' Convert a parsed query into a request for ecmwfr.
#'
#' @description Takes an output from parse_met_query and convert it into the format used by ecmwfr to fetch ERA-5 data.
#'
#' @param query an output from parse_met_query.
#'
#' @noRd
#'
query_to_CDSrequest <- function(query){
  query_request <- list(
    "dataset_short_name" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "dataset_short_name")],
    "product_type" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "product_type")],
    "variable" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "variable")],
    "pressure_level" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "pressure_level")],
    "year" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "year")],
    "month" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "month")],
    "day" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "day")],
    "time" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "time")],
    "area" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "area")],
    "format" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "format")],
    "target" = query[[1]]$details$suffix[which(query[[1]]$details$prefix == "target")]
  )
  return(query_request)
}

#' Check a folder for met queries
#'
#' @description Check a folder for query files.
#'
#' @param query_folder full file path to the folder containing queries.
#' @param target_string character vector of a string used to grep the file names. Use to specify or isolate from other files.
#'
#' @noRd
#'
check_for_queries <- function(query_folder = paste0(getwd(),"/incoming-queries/"),
                              target_string = 'query'){
  # query_folder = paste0(proj_test_dir,"incoming-queries/")
  # target_string = 'query'
  # Are there files in the folder?
  query_files <- list.files(query_folder, full.names = T)
  if(is_empty(query_files)){
    return(FALSE)
  } else {
    # Check for string query
    if(!is.null(target_string)){
      queries_short <- list.files(query_folder, full.names = F)
      matches <- grep(target_string,queries_short)
      queries <- query_files[c(matches)]
      if(is_empty(queries)){
        return(FALSE) # There are no entries that match the string query.
      } else {
        return(TRUE) # There are entries that match the string query.
      }
    } else {
      # There is no match string, and there are files.
      return(TRUE)
    }
  }
}

#' Create a query table
#'
#' @description create a table that stores all information needed for a met query
#'
#' @param output_dir Full filepath to the directory where the queries will be sent
#' @param pc_name Character vector of the PC Name sending the query
#' @param dataset_short_name Character vector; request var for ecmwfr
#' @param product_type Character vector; request var for ecmwfr
#' @param variable A vector containing one or more variables; request var for ecmwfr
#' @param year A vector containing one or more years; request var for ecmwfr
#' @param month  A vector containing one or more months; request var for ecmwfr
#' @param day A vector containing one or more days; request var for ecmwfr
#' @param time A vector containing one or more times in the format "HH:MM" e.g. "03:00"; request var for ecmwfr
#' @param area An area definition for the met. Format "NORTH/WEST/SOUTH/EAST" e.g. "0/-180/-90/180" for the entire SH.
#' @param format What format? One of 'grib' or 'netcdf'
#'
create_met_query <- function(pc_name,
                             dataset_short_name = "reanalysis-era5-pressure-levels",
                             product_type = "reanalysis",
                             variable = all_vars,
                             pressure_level = all_levels,
                             year = "2000",
                             month = "04",
                             day = "04",
                             time = times_3_hours,
                             area = "0/-180/-90/180",
                             format = "grib",
                             target = "target"){
  if(format == "grib"){
    if(isFALSE(grepl(paste0(".",format),target))){
      target = paste0(target,".",format)
    }
  } else if(format == "netcdf"){
    if(isFALSE(grepl(".nc",target))){
      target = paste0(target,".nc")
    }
  }
  # create table with relevant info.
  # adopting the approach of the SampleQueue log file creation method.
  ## Create blank query
  blank_query <- data.frame(matrix(NA,nrow = 19, ncol = 2)) %>% 'colnames<-'(c('prefix','suffix'))
  blank_query[1,1] <- c("MET_QUERY")
  blank_query[3,1] <- c("metfetchR_package")
  # BREAKS
  blank_query[c(2,4,7),1] <- c("-")
  # PCNAME
  blank_query[5,c(1,2)] <- c("from",pc_name)
  # DATETIME
  dt <- Sys.Date() %>% date_compact()
  tm <- Sys.time() %>% format(format = "%H:%M:%S") %>% str_replace_all(., "[:]","-")
  datetime <- paste0(dt,"-",tm)
  blank_query[6,c(1,2)] <- c("timestamp",datetime)
  # DETAILS
  blank_query[8,1] <- c("DETAILS")
  # QUERY INFO
  vars <- c("dataset_short_name","product_type","variable","pressure_level","year","month","day","time", "area",'format','target')
  blank_query[seq(9,19,1),1] <- vars
  # Assign inputs using explicit name indexing
  blank_query[which(blank_query[,1] == "dataset_short_name"),2] <- dataset_short_name
  blank_query[which(blank_query[,1] == "product_type"),2] <- product_type
  blank_query[which(blank_query[,1] == "variable"),2] <- paste0(variable, collapse = ",")
  blank_query[which(blank_query[,1] == "pressure_level"),2] <- paste0(pressure_level, collapse = ",")
  blank_query[which(blank_query[,1] == "year"),2] <- paste0(year, collapse = ",")
  blank_query[which(blank_query[,1] == "month"),2] <- paste0(month, collapse = ",")
  blank_query[which(blank_query[,1] == "day"),2] <- paste0(day, collapse = ",")
  blank_query[which(blank_query[,1] == "time"),2] <- paste0(time, collapse = ",")
  blank_query[which(blank_query[,1] == "area"),2] <- area
  blank_query[which(blank_query[,1] == "format"),2] <- format
  blank_query[which(blank_query[,1] == "target"),2] <- target
  return(blank_query)
}

#' Save a query as a text file
#'
#' @description Save a generated query table to a specified location, with optional date-time append.
#'
#' @param query_table A simple table to be coerced into a text file
#' @param destination full filepath to the desired destination folder
#' @param name name of the file to be saved.
#' @param append_datetime logical TRUE/FALSE to add date and time in format YYYY-MM-DD-HH-MM-SS
#'
#' @export
#'
save_query <- function(query_table,
                       destination,
                       name,
                       append_datetime = TRUE){
  # Remove extension
  if(grepl('.txt',name)){
    name <- str_remove(name,'.txt')
  }
  # add datetime
  if(isTRUE(append_datetime)){
    dt <- Sys.Date() %>% date_compact()
    tm <- Sys.time() %>% format(format = "%H:%M:%S") %>% str_replace_all(., "[:]","-")
    datetime <- paste0(dt,"-",tm)
    name <- paste0(name,"_",datetime)
  }
  # introduce or re-introduce extension
  if(!grepl('.txt',name)){
    name <- paste0(name,'.txt')
  }
  # write
  write.table(query_table, paste0(destination, name),
              col.names = FALSE, quote = FALSE, row.names = FALSE, na = "")
}

#' Read a met query file into R
#'
#' @description takes the specified directory and reads all query files within it.
#'
#' @param query_dir full path to the location where incoming queries are located
#' @param which_element numeric vector of length >=1 or 'all', used to index the query files. By default only the first is selected.
#' @param target_string character vector of a string used to grep the file names. Use to specify or isolate from other files.
#'
#' @importFrom rlang is_empty
#' @importFrom magrittr %>%
#'
#' @noRd
#'
read_met_query <- function(query_dir = paste0(getwd(),"/incoming-queries/"),
                           which_element = 1,
                           target_string = 'query'){
  # test vars
  # query_dir = paste0(proj_test_dir,"incoming-queries/")
  # which_element = 1
  # target_string = 'query'
  # Get queries
  queries <- list.files(query_dir, full.names = T)
  # Restrict to target string, if specified.
  if(!is.null(target_string)){
    queries_short <- list.files(query_dir, full.names = F)
    matches <- grep(target_string,queries_short)
    queries <- queries[c(matches)]
  }
  if(is_empty(queries)){
    return(FALSE)
  } else {
    query_flist <- queries %>% as.list()
  }
  # Get specified item
  if(which_element != 'all'){
    if(!is.numeric(which_element)){
      stop("read_met_query error. Please provide which_element as a numeric element or 'all'")
    }
    query_flist <- query_flist[c(which_element)]
    query <- lapply(query_flist, function(f){
      f2 <- parse_met_query(f)
      f2
    })
    # query <- list()
  } else if(which_element == 'all'){
    query <- lapply(query_flist, function(f){
      f2 <- parse_met_query(f)
      f2
    })
  } else {
    stop("read_met_query error: which_element must be a numeric vector of length >=1 or 'all'.")
  }
  return(query)
}


#' Parse a met query file into R
#'
#' @description Wrapped inside read_met_query(). Reads a query file in using readLines and parses the output.
#'
#' @param query_file full filepath to a query file (.txt)
#' @param warn logical; TRUE/FALSE; to display warnings during column separation at whitespace.
#'
#' @noRd
#'
parse_met_query <- function(query_file,
                            warn = FALSE){
  # Create results list
  res_list <- vector('list', length = 4) %>%
    'names<-'(c('file','from','created','details'))
  res_list$file <- query_file
  # fl <- read.table(file = query_file, sep = '\t')
  if(!isTRUE(warn)){
    fl <- readLines(con = query_file) %>%
      data.frame() %>% 'colnames<-'(c('full')) %>%
      {suppressWarnings(separate(., full, into = c("prefix", "suffix"), sep = " (?=[^ ]+$)"))} %>%
      mutate(prefix = stri_trim_right(prefix))
  } else if(isTRUE(warn)) {
    fl <- readLines(con = query_file) %>%
      data.frame() %>% 'colnames<-'(c('full')) %>%
      separate(., full, into = c("prefix", "suffix"), sep = " (?=[^ ]+$)") %>%
      mutate(prefix = stri_trim_right(prefix))
  } else {
    stop("parse_met_query error: warn must be logical.")
  }
  # Assign outputs to res_list
  res_list$from <- fl$suffix[which(fl$prefix == "from")]
  res_list$created <- fl$suffix[which(fl$prefix == "timestamp")]
  res_list$details <- fl %>% slice((which(fl$prefix == "DETAILS")+1):nrow(.))
  return(res_list)
}
