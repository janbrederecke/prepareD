# Package startup message
.onAttach <- function(libname
                      , pkgname
){

  # Print the package startup message
  msg <- paste0(
    "\n",
    "== Thank you for using prepareD =========================================",
    "\nIf you encounter any bugs feel free to file an issue at:",
    "\nhttps://github.com/janbrederecke/prepareD",
    "\n",
    "\nYou might want to check my other packages at:",
    "\nhttps://github.com/janbrederecke"
  )
  packageStartupMessage(msg)
}