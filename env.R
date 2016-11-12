# Create new environment if it doesn't exists
if (!exists('proj.env') || !is.environment(proj.env)) {
  message('Creating new environment.')
  #proj.env = new.env()
  # at the moment use global environment
  proj.env = globalenv()
}

