source('include.R')

event.matrix = proj.env$eeg_data$EEG$event.matrix
eeg.sample = proj.env$eeg_data$sample
fda.smooth = proj.env$fda$smooth

# left hand 0x301
# tongue move 0x304

leg.loc = event.matrix[event.matrix$TYPE == config$codes$left_hand &
                         event.matrix$VALID == TRUE,]
tongue.loc = event.matrix[event.matrix$TYPE == config$codes$tongue &
                            event.matrix$VALID == TRUE,]


right.hand.loc = event.matrix[event.matrix$TYPE == config$codes$foot_towards_right &
                            event.matrix$VALID == TRUE,]


tongue.loc = tongue.loc[1:36,]
dim(tongue.loc)
dim(right.hand.loc)
right.hand.loc = right.hand.loc[1:36,]


get.sample.mean = function(from,
                           duration = 1,
                           nbasis = 5,
                           raw = TRUE) {
  sample.data = eeg.sample(from - 2, duration = 4)
  sample.smooth = proj.env$fda$smooth(sample.data, nbasis = 5)
  sample.mean = func.mean(sample.data)
  if (!raw) {
    return(sample.mean)
  }
  return(as.vector(sample.mean$data[, ]))
}

x_minus = 4
x_plus = 6

all_chan_classif = function () {
  all.data = NULL

  categories = NULL
  
  for (x in leg.loc[,2]) {
    sample.data = eeg.sample(x - x_minus, duration = x_plus)
    if (is.null(all.data)) {
      all.data = sample.data
      categories = rep(0, 60)
    } else {
      all.data = rbind(sample.data, all.data)
      categories = c(rep(0, 60), categories)
    }
  }

  for (x in tongue.loc[,2]) {
    sample.data = eeg.sample(x - x_minus, duration = x_plus)
    all.data = rbind(sample.data, all.data)
    categories = c(rep(1, 60), categories)
  }
  
  #all = t(all.data)
  #rownames(all) <- NULL
  all = all.data
  all.smooth = fda.smooth(all, nbasis = 15, type.basis = 'fourier')
  all.d1 = fdata.deriv(all.smooth, nderiv = 1)
  
  resp = as.data.frame(categories)
  colnames(resp) = c('RESPONSE')
  
  ldata = list("df" = resp,
               "x" = fdata(all.smooth),
               "x.d1" = all.d1)
  
  res.bin = fregre.glm(RESPONSE ~ x, ldata, family = binomial())
  #res.gsam <- classif.glm(RESPONSE ~ x, data = ldata)
  res.gsam <- classif.gsam(RESPONSE~s(x),data=ldata)
  summary(res.gsam)
  
}

#all_chan_classif()
mean_classif = function() {
  
  
  
  leg.mvt = matrix(1:250)
  all_data = NULL
  
  for (x in leg.loc[, 2]) {
    sample.mean = get.sample.mean(x)
    leg.mvt = cbind(leg.mvt, sample.mean)
    
    if (is.null(all_data)) {
      all_data = sample.mean
    } else {
      all_data = cbind(sample.mean, all_data)
    }
  }
  leg.mvt = leg.mvt[, -1]
  leg.smooth = fda.smooth(leg.mvt)
  
  tongue.mvt = matrix(1:250)
  for (x in tongue.loc[, 2]) {
    sample.mean = get.sample.mean(x)
    tongue.mvt = cbind(tongue.mvt, sample.mean)
    all_data = cbind(sample.mean, all_data)
  }
  tongue.mvt = tongue.mvt[, -1]

  right.hand.mvt = matrix(1:250)
  for (x in right.hand.loc[, 2]) {
    sample.mean = get.sample.mean(x)
    right.hand.mvt = cbind(tongue.mvt, sample.mean)
    all_data = cbind(sample.mean, all_data)
  }
  right.hand.mvt = right.hand.mvt[,-1]
  
  
    
  tongue.smooth = fda.smooth(tongue.mvt)

  #all.smooth = fda.smooth(all_data)
  #all.d1 = fdata.deriv(all_data)
  all = cbind(tongue.mvt, leg.mvt) #, right.hand.mvt)
  all = t(all)
  rownames(all) <- NULL
  all.smooth = fda.smooth(all, nbasis = 15, type.basis = 'fourier')
  all.d1 = fdata.deriv(all, nderiv = 1)
  
  len = dim(tongue.loc)[1]
  #y = c(rep(0, len), rep(1, len), rep(2,len))
  y = c(rep(0, len), rep(1, len))
  resp = as.data.frame(y)
  colnames(resp) = c('RESPONSE')
  
  ldata = list("df" = resp,
               "x" = fdata(all.smooth),
               "x.d1" = all.d1)
  
  #res.bin = fregre.glm(RESPONSE ~ x, ldata, family = binomial())
  #res.gsam <- classif.glm(RESPONSE ~ x, data = ldata)
  res.gsam <- classif.gsam(RESPONSE~s(x),data=ldata)
  summary(res.gsam)
  ycat = ldata$df$RESPONSE
  res.DD<-classif.DD(ycat,all.d1,classif="gam",depth="mode")
  res.DD$prob.classification
}



smpl = eeg.sample(from=12.7)
