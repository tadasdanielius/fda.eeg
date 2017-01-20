if (!exists('proj.env')) {
  source('env.R')
}

# default settings
proj.env$set.config = function(envir = proj.env) {
  config = new.env(parent=emptyenv())
  
  config$type.basis = 'fourier'
  config$nbasis = 39
  config$hz = 250
  config$trial_duration = 7 # In seconds
  config$ncl = 4
  config$num_of_channels = 60
  config$channels = 1:config$num_of_channels
  
  config$reload.data = FALSE
  config$verbose = TRUE
  
  config$raw.data.path = 'data/eeg.raw.bin'
  config$event.matrix.path = 'data/event.matrix.bin'
  
  # Some of the trials are rejected 
  # should we ignore them or use for our analysis
  config$ignore_rejected_trials = TRUE
  
  # event codes
  config$codes = list()
  config$codes$rejected = 0x3ff
  
  config$codes$left_hand = 0x301
  config$codes$right_hand = 0x302
  config$codes$tongue = 0x304
  config$codes$foot_towards_right = 0x303
  
  assign('config', config, envir = envir)
}

proj.env$set.config()
