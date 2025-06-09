# package announcement
.onAttach <- function(libname, pkgname) {
  # respect quiet option
  if (isTRUE(getOption("margot.sim.quiet"))) {
    return(invisible())
  }

  version <- utils::packageDescription(pkgname, fields = "Version")
  packageStartupMessage("margot.sim ", version)
}