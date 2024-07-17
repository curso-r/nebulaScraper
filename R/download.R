#' Download Nebula Award Page for a Given Year
#'
#' This function downloads the Nebula Award page for a specified year and saves it as an HTML file in a given directory.
#'
#' @param yr A numeric value or string representing the year for which the Nebula Award page is to be downloaded.
#' @param path A string specifying the directory path where the HTML file should be saved.
#' 
#' @return A string containing the path to the downloaded HTML file.
#' 
#' @details
#' The function first creates the specified directory if it does not already exist. It then constructs the file name and checks if the file already exists in the directory. If the file does not exist, the function constructs the URL for the Nebula Award page for the specified year and downloads the page, saving it as an HTML file in the specified directory. If the file already exists, it simply returns the path to the existing file.
#' 
#' @export
nebula_download_year <- function(yr, path) {
  fs::dir_create(path)
  f <- paste0(file.path(path, yr), ".html")
  if (!file.exists(f)) {
    u <- paste0("https://nebulas.sfwa.org/award-year/", yr)
    httr::GET(u, httr::write_disk(f))
  }
  f
}