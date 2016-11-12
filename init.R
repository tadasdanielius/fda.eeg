if (!exists('proj.env')) {
  source('env.R')
}

proj.env$load_library = function(lib) {
  # First, check if package is installed, if not install, otherwise just load
  if (is.null(lib)) {
    return (FALSE)
  }
  message('Loading ', lib)
  if (!"fda.usc" %in% installed.packages()) {
    message('Package ', lib, ' is not installed fda.usc. Installing')
    install.packages(lib)
  } else {
    library(package = lib,
            character.only = TRUE,
            quietly = TRUE)
  }
  return (TRUE)
}


proj.env$libs = array(c('fda.usc', 'lattice'))
apply(proj.env$libs, 1, proj.env$load_library)
