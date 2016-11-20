if (!exists('proj.env')) {
  source('env.R')
}
if (!exists('config', envir = proj.env)) {
  source('config.R')
}

proj.env$fda = new.env(parent=emptyenv())

proj.env$fda$smooth = function(raw, type.basis = proj.env$config$type.basis, nbasis=proj.env$config$nbasis) {
  eeg = raw
  if (class(eeg) != 'fdata') {
    eeg = fdata(eeg)
  }
  return(fdata2fd( eeg, type.basis=type.basis, nbasis=nbasis))
}

proj.env$fda$cluster = function(data, ncl = proj.env$config$ncl, method = 'Exact', draw=FALSE, max.iter=100) {
  eeg = data
  if (class(eeg) != 'fd') {
    eeg = proj.env$fda$smooth(eeg)
  }
  #clusters = kmeans.fd(eeg, ncl = ncl, method=method, draw=FALSE)
  
  clusters = kmeans.fd(eeg,ncl=ncl,dfunc=func.trim.RPD, method=method,
           max.iter=70,par.dfunc=list(dfunc="depth.FM",deriv=c(0,1,2)))
  return (clusters)
}