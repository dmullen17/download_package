#' downloadPackage.R
#'
#' @title Utility function for downloading DataOne packages.
#'
#' @description This function downloads all of the data objects in a DataOne package when a "Download All" button is not available.
#' Setting check_download_size = T is recommended if you are uncertain of the total download size.
#' This function will also download any data objects it finds in any children of the input data package.  If you would only like to download data from
#' one package set download_child_packages = F, or use the function on a package without children.
#'
#' @param mn (MNode) The MNode instance to be changed.
#' @param resource_map_pid (chraracter) The PID of the resource map for the package to download.
#' @param download_directory (character) The path of the directory to download the package to.
#' @param check_download_size (logical) Optional.  Whether to check the total download size before continuing.  Setting this to FALSE speeds up the function, especially when the package has many elements.
#' @param download_child_packages (logical) Optional.  Whether to download data from child packages of the selected package.
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as aruments exist on the MN before continuing. Checks that objects exist and are of the right format type. Setting this to FALSE speeds up the function, especially when the package has many elements.
#'
#' @example
#'\dontrun{
#' cn <- CNode('PROD')
#' mnReal <- getMNode(cn,'urn:node:ARCTIC')
#' wd <- getwd()
#' downloadPackage(mn = mnReal, resource_map_pid = "resource_map_urn:uuid:2b4e4174-4e4b-4a46-8ab0-cc032eda8269", download_directory = wd)
#' }
#'
#' @import dataone
#' @import pbapply
#'
#' @export
downloadPackage <- function(mn,
                            resource_map_pid,
                            download_directory,
                            check_download_size = TRUE,
                            download_child_packages = TRUE,
                            check_first = TRUE) {
  ## Possible Changes:
  #1 Make mn argument a character of "ADC", "KNB", etc. with switch statements based on node? -- probably overkill
  #2 How many child levels should it support? -- 3 or 4 max
  #3 Should non-authenticated user warning messages for get_package and getSystemMetdata be suppressed? -- looks like Bryce removed these
  #4 resource_map_pid argument accepts metadata pids - could change to not accept metadata pids
  ## Comments:
  #1 would be ideal not to have to call getSystemMetadata once for each file name.  Could remove check_download_size from arguments (not optional) - this would reduce
  # getSystemMetadata calls in half, assuming they are necessary to return the fileNames

  # Check that input arguments are in the correct format
  stopifnot(is.character(resource_map_pid))
  stopifnot(is.character(download_directory))
  stopifnot(file.exists(download_directory))
  stopifnot(is.logical(check_download_size))
  stopifnot(is.logical(download_child_packages))
  stopifnot(is.logical(check_first))

  # Get package pids
  package <- arcticdatautils::get_package(mn, resource_map_pid)

  # Get child package pids
  if (download_child_packages) {

    # Check that child packages exist
    if (length(package$child_packages) != 0) {
      child_packages <- list()
      n <- length(package$child_packages)
      progressBar <- txtProgressBar(min = 0, max = n, style = 3)
      message("\nDownloading identifiers from child packages")

      # Loop through child packages and extract pids using get_package()
      for (i in 1:n) {
        child_packages[[i]] <- arcticdatautils::get_package(mn, package$child_packages[i])
        setTxtProgressBar(progressBar, i)
      }

      rm(n)
      rm(progressBar)
    }
  }

  # Initialize data pids vector
  data_pids <- c()

  # Select data pids from initial package, if they exist
  if (length(package$data) != 0) {
    data_pids <- package$data
  }

  # Select data pids from child packages, if they exist
  if (exists("child_packages")) {
    for (i in 1:length(child_packages)) {
      # Check if child package contains data
      if (length(child_packages[[i]]$data != 0)) {
        data_pids <- c(data_pids, child_packages[[i]]$data)
      }
    }
    rm(child_packages)
  }

  # Check that data exists
  if (length(data_pids) == 0) {
    stop("No data selected.  Double check the package you entered contains data files")
  }

  # Check total download size
  if (check_download_size) {
    message("\nDownloading file sizes from system metadata.  This could take a significant amount of time if the number of data objects is large")
    fileSizes <- pbsapply(data_pids, function(pid) { dataone::getSystemMetadata(mn, pid)@size })
    downloadSize <- sum(fileSizes, na.rm = T)
    rm(fileSizes)

    # Simplify downloadSize to readable format
    if (downloadSize >= 1e+12) {
      downloadSize <- round(downloadSize/(1e+12), digits = 2)
      unit = " terabytes"
    } else if (1e+12 > downloadSize & downloadSize >= 1e+9) {
      downloadSize <- round(downloadSize/(1e+9), digits = 2)
      unit = " gigabytes"
    } else if (1e+9 > downloadSize & downloadSize >= 1e+6) {
      downloadSize <- round(downloadSize/(1e+6), digits = 2)
      unit = " megabytes"
    } else if (1e+6 > downloadSize) {
      downloadSize = round(downloadSize/1000, digits = 2)
      unit = " kilobytes"
    }

    # Prompt user if they wish to continue based on total download size
    message(paste0("\nYour download is approximately ", downloadSize, unit, "\n"))
    continue <- readline(prompt = paste0("Proceed with the download (", downloadSize, unit, ")? Input yes/no: "))
    while (!(continue %in% c("yes", "no"))) {
      message("Type yes or no without quotation marks or capitals\n")
      continue <- readline(prompt = "Proceed with the download? Input yes/no: ")
    }

    # Cancel download if "no" was entered
    if (continue == "no") {
      stop("Download cancelled by user")
    }

    rm(downloadSize)
    rm(unit)
    rm(continue)

  }

  # Download data pids to selected directory
  message("\nDownloading file names from system metadata.  This could take a significant amount of time if the number of data objects is large")
  fileNames <- pbsapply(data_pids, function(pid) { dataone::getSystemMetadata(mn, pid)@fileName })
  n <- length(data_pids)
  progressBar <- txtProgressBar(min = 0, max = n, style = 3)
  message(paste0("\nDownloading data objects to ", download_directory))
  for (i in 1:n) {
    dataObj <- dataone::getObject(mn, data_pids[i], check = check_first)
    writeBin(dataObj, paste(download_directory, fileNames[i], sep='/'))
    setTxtProgressBar(progressBar, i)
  }

}
