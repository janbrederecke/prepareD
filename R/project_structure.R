project_structure <- function(path = "") {
    if (path == "") {
        path <- getwd()
    }

    # Create the project directory structure
        all_files <- list.files(system.file("inst/templates/basic",
                                            package = "prepareD"),
                                full.names = TRUE)

        file.copy(
            from = all_files,
            to = path,
            overwrite = TRUE,
            recursive = TRUE)

        dir.create(paste0(path, "data"))
        dir.create(paste0(path, "data/raw"))
        dir.create(paste0(path, "data/clean"))
}
