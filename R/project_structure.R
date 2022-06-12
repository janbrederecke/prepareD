project_template_init <- function(path = "") {
    if (path == "") {
        path <- getwd()
    }   

    
        all_files <- list.files(system.file("inst/templates/basic", package = "prepareD"), full.names = TRUE)

        file.copy(  
            from = all_files,
            to = path,
            overwrite = TRUE,
            recursive = TRUE)
}
