if (!exists('proj.env')) {
  source('env.R')
}
# Just in case config is not yet sourced
if (!exists('config', envir = proj.env)) {
  source('config.R')
}

if(!exists('eeg_data', envir = proj.env)) {
  proj.env$eeg_data = new.env(parent=emptyenv())  
}

# To event matrix add Index, Second, Position and valid flag
# convert to data.frame
proj.env$eeg_data$clean_events = function(events.matrix) {
  eeg_data_env = proj.env$eeg_data
  env = eeg_data_env$EEG
  config = proj.env$config
  event.matrix = env$event.matrix
  trial = config$trial_duration * config$hz
  
  event.matrix = as.data.frame(event.matrix)
  
  # bind seconds to row
  event.matrix = cbind(event.matrix[,1] / config$hz, event.matrix)
  
  # bind Row number to matrix
  event.matrix = cbind(1:length(event.matrix[,1]), event.matrix)
  
  # bind accepted flag to matrix
  event.matrix = cbind(event.matrix, TRUE)

  
  # Some of the trials are rejected and may affect our analysis. 
  # therefore we need to get rid of them. 
  if (config$ignore_rejected_trials) {
    rejected = event.matrix[ event.matrix[,4] == config$codes$rejected, 3 ]
    for (pos in rejected) {
      event.matrix[ event.matrix[,3] >= pos & event.matrix[,3] <= (pos+trial), 5 ] = FALSE
    }
  }
  
  colnames(event.matrix) = c('INDEX','SECOND', 'POS', 'TYPE', 'VALID')
  
  return(event.matrix)
}

proj.env$eeg_data$load = function() {
  eeg_data = proj.env$eeg_data
  config = proj.env$config
  env = eeg_data$EEG
  
  if (!exists("RAW", envir=env)) {
    message ('Loading EEG data...', appendLF = FALSE)
    load(config$raw.data.path, envir = env)
    message ('Done')
  } else {
    if (config$verbose) {
      message('EEG data already loaded. Skipping.')
    }
    
  }
  
  if (!exists("event.matrix", envir=env)) {
    message ('Loading event matrix...', appendLF = FALSE)
    load(config$event.matrix.path, envir = env)
    message('Done.')
    
    message('Preprocessing event matrix')
    env$event.matrix = eeg_data$clean_events()
    
  }
  
}

proj.env$eeg_data$setup = function() {
  config = proj.env$config
  eeg_data = proj.env$eeg_data

  eeg_data$ready = FALSE
  
  # Force to load data everytime this file is sourced
  if (config$reload.data == TRUE) {
    if (config$verbose)
      message('Reinitializing data, since reload.data flag is set to true.')
    
    if (exists('EEG', envir = eeg_data)) {
      rm('EEG', envir = eeg_data)
      if (config$verbose)
        message('Cached data removed.')
    }
  }
  
  if (!exists('EEG', envir = eeg_data) || is.null(eeg_data$EEG)) {
    eeg_data$EEG = new.env(parent = emptyenv())
    if (config$verbose) {
      message('EEG Environment created.')
    }
  }
  
  eeg_data$load()
  eeg_data$ready = TRUE
}

# Get raw data from sec. to sec.
proj.env$eeg_data$sample = function(from=2, duration=2, channels = proj.env$config$channels, preproc = NULL) {
  eeg_data = proj.env$eeg_data
  raw = eeg_data$EEG$RAW
  config = proj.env$config
  
  if (!eeg_data$ready) {
    # Shouldn't happen... just in case somewhere messed up data environment
    warning('Data is not yet ready... unexpected.')
    proj.env$eeg_data$setup()
  }
  
  # Multiply by hz to get actual range 1 second = config$hz
  sample.from = from*config$hz
  sample.to = sample.from + duration*config$hz

  sample = raw[channels, sample.from:sample.to]
  
  if (!is.null(preproc)) {
    return(preproc(sample))
  }
  
  return(sample)
}

proj.env$eeg_data$event = function(event_id, duration = 1, include_rejected = FALSE, channels = proj.env$config$channels) {
  eeg_data = proj.env$eeg_data
  event.matrix = eeg_data$EEG$event.matrix
  
  events = event.matrix [ event.matrix[,2] %in% event_id, ]
  
  return(events)
}

proj.env$eeg_data$setup()
