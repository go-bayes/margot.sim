# package announcement
.onAttach <- function(libname, pkgname) {
  # respect quiet option
  if (isTRUE(getOption("margot.sim.quiet"))) {
    return(invisible())
  }

  version <- utils::packageDescription(pkgname, fields = "Version")
  packageStartupMessage("margot.sim ", version)
}

# global variables to avoid R CMD check notes
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "..density..",
    "apply_post_treatment_selection",
    "bias",
    "estimate", 
    "id",
    "ipw",
    "n_effective",
    "rep_id"
  ))
}