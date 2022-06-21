#' Find Phase III deployment workbook sheets id
#'
#' @param deployid Deployment id e.g. "bw180905-53"
#'
#' @return A `sheets_id` object
find_phaseiii <- function(deployid) {
  phaseiii_drid <- "1SjiVVzHdOiLK5UnwQt0RA184_TN4-XNE"
  phaseiii_dr <- googledrive::drive_get(id = phaseiii_drid)
  gsid <- googledrive::drive_ls(phaseiii_dr, pattern = deployid)[["id"]]
  if (length(gsid) == 0) {
    stop("No Phase III workbooks found for deployid ", deployid)
  } else if (length(gsid) == 1) {
    googlesheets4::as_sheets_id(gsid)
  } else {
    stop("Multiple Phase III workbooks found for deployid ", deployid)
  }
}

#' Read a Phase III deployment workbook
#'
#' @param deployid Deployment id e.g. "bw180905-53"
#'
#' @return A `tibble` with columns `deployid`, `motionlessid`,
#' `motionlessstart`, `motionlessend`, and `duration_s`
read_phaseiii <- function(deployid) {
  gsid <- find_phaseiii(deployid)
  googlesheets4::with_gs4_quiet(
    googlesheets4::read_sheet(gsid)
  )
}
