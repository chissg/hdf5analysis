# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' read_h5ls
#'
#' Read the file 'filename', expected to be the output of "h5ls -vr <file>".
#' Return a tibble with one row per dataset found in the file.
#'
#' @param filename Name of the h5ls output file to be read.
#'
#' @return A tibble
#' @export
#' @importFrom  magrittr "%>%"
#'
read_h5ls <- function(filename)
{
  txt <- readLines(filename)
  # Get names and lengths of datasets
  dsetlines <- txt[stringr::str_detect(txt, "Dataset")]
  dsets <- stringr::str_match(dsetlines, "/([^/]+)/(\\S+)\\s+Dataset \\{(\\d+)/Inf,\\s+(\\d+)/")
  # Get logical and allocated sizes in bytes
  storagelines <- txt[stringr::str_detect(txt, "Storage:")]
  sizes <- stringr::str_match(storagelines, "([0-9]+) logical bytes, ([0-9]+) allocated bytes")
  # Assemble result
  tibble::tibble( table = dsets[,2]
                , name = dsets[,3]
                , nrec = as.integer(dsets[,4])
                , asize = as.integer(dsets[,5])
                , logical = as.integer(sizes[,2])
                , alloc = as.integer(sizes[,3])
        ) %>%
    tidyr::unite("fullname", c("table", "name"), sep = "/", remove = FALSE)
}

