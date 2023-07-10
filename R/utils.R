# Various utilities

#' A setup function used to create directories on the fetching PC.
#'
#' @description Create the directories used by the fetching PC to receive queries and store/convert met data.
#'
#' @param parent_directory Full filepath to the folder where the directory tree is to be created.
#'
#' @export
#'
create_fetch_dirs <- function(parent_directory){
  # Specify names in folder hierarchy
  names_l1 <- c("incoming-queries","met-raw","met-converted","archive")
  names_l2 <- c("met-raw","met-converted-archive","queries")
  # Create folder 'lists'
  folders <- vector('list', length = 4) %>%
    'names<-'(names_l1)
  folders[[4]] <- vector('list', length = 3) %>%
    'names<-'(names_l2)
  # Create folders
  for(l1 in seq_along(folders)){
    dir_path_l1 = paste0(parent_directory, names(folders[l1]),
                         "/")
    dir.create(path = dir_path_l1, showWarnings = FALSE)
  }
  for(l2 in seq_along(folders[['archive']])){
    dir_path_l2 = paste0(parent_directory,"archive/", names(folders[['archive']][l2]),
                         "/")
    dir.create(path = dir_path_l2, showWarnings = FALSE)
  }
}

#' Check if a date is valid, and return a compact version.
#'
#' @description Perform an is_date() check, and, if the date is valid under the prescribed format, return in format ddmmyy. From SampleQueue package.
#'
#' @param date the date to convert to DDMMYY
#' @param date.format the format of the imported date.
#'
#' @noRd
#'
date_compact <- function(date, date.format = "%Y-%m-%d"){
  if(isTRUE(is_date(mydate = date, date.format))){
    date <- as.Date(date, format = date.format)
    date_of_run_compact <- format(date, "%Y-%m-%d")
  } else if(isTRUE(is_date(mydate = date, date.format = "%d%m%y"))){
    date_of_run_compact <- date
  } else {
    stop("please enter a valid date of format YYYY-MM-DD")
  }
  return(date_of_run_compact)
}

#' Check if a date is valid.
#'
#' @description Check if the date is valid in the format dd/mm/yy. Adapted from https://stackoverflow.com/questions/48542804/r-date-format-check
#'
#' @param mydate the input date.
#' @param date.format the format to check against.
#'
#' @noRd
#'
is_date <- function(mydate, date.format = "%d/%m/%y"){
  tryCatch(!is.na(as.Date(mydate, date.format)),
           error = function(err) {FALSE})
}

#' Remove the file path from a given file or folder name.
#'
#' @description Removes the file or folder path from a given file/folder name by splitting the path at every "/", and getting rid of all but the last set of characters. If given a short filename (no path), the same string will just be spat out the other end unchanged.
#'
#' @param filnames a string containing the full file path.
#'
#' @noRd
#'
trim_path <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim.")
  }
}
