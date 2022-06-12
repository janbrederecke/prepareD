#' @title project_structure
#'
#' @description Creates a project structure in the current working directory.
#'
#' @param .path A character vector containing the path to the project.

#'
#' @return A character vector with the numeric variables.
#' @examples -
#' @export

project_structure <- function(.path = ""
){
  # Check the path
  if (.path == "") {
    .path <- getwd()
  }

  # Copy the template files to the current working directory
  all_files <- list.files(system.file("templates/basic",
                                      package = "prepareD"),
                          full.names = TRUE)
  file.copy(from = all_files, to = .path, overwrite = TRUE,
            recursive = TRUE)

  # Create the empty folders that cannot be included in the template
  dir.create(paste0(.path, "/data"))
  dir.create(paste0(.path, "/data/raw"))
  dir.create(paste0(.path, "/data/clean"))
}
