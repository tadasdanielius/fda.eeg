source('include.R')
set.seed(1)

event.matrix = proj.env$eeg_data$EEG$event.matrix
eeg.sample = proj.env$eeg_data$sample
fda.smooth = proj.env$fda$smooth
config = proj.env$config

x_minus = 0
x_plus = 2

nbasis = 15
nbasis.type = 'fourier'

settings.classif = 'lda'
settings.depth = 'RP'
settings.nderiv = 1


get.sample.mean = function(from, duration = 1) {
  sample.data = eeg.sample(from, duration)
  sample.mean = func.mean(sample.data)
  return(as.vector(sample.mean$data[, ]))
}


generate.sample.mean.matrix = function(event.type) {
  event.loc = event.matrix[event.matrix$TYPE == event.type &
                             event.matrix$VALID == TRUE, ]
  mvt = NULL
  for (x in sample(event.loc[,2])) {
    sample.mean = get.sample.mean(x-x_minus, x_plus) 
    if (is.null(mvt)) {
      mvt = sample.mean
    } else {
      mvt = cbind(mvt, sample.mean)
    }
  }
  ret = list()
  ret$mvt = mvt
  ret$events = event.loc
  return(ret)
}

#class_1 = generate.sample.mean.matrix(config$codes$tongue)
#class_2 = generate.sample.mean.matrix(config$codes$foot_towards_right)

class_1 = generate.sample.mean.matrix(config$codes$left_hand)
class_2 = generate.sample.mean.matrix(config$codes$right_hand)


class_3 = generate.sample.mean.matrix(config$codes$left_hand)


min_val = min(length(class_1$events[,2]), length(class_2$events[,2])) #,length(class_3$events[,2]))
test_from = 25

test_1 = class_1$mvt[,test_from:min_val]
test_2 = class_2$mvt[,test_from:min_val]
#test_3 = class_3$mvt[,test_from:min_val]
  
min_val = min_val - test_from
class_1$mvt = class_1$mvt[,1:min_val]
class_2$mvt = class_2$mvt[,1:min_val]
#class_3$mvt = class_3$mvt[,1:min_val]

all = cbind(class_1$mvt, class_2$mvt) #, class_3$mvt)
all = t(all)
rownames(all) <- NULL
all.smooth = fda.smooth(all, nbasis = nbasis, type.basis = nbasis.type)
all.d1 = fdata.deriv(all.smooth, nderiv = settings.nderiv)



y = c(rep(1, min_val), rep(2, min_val)) #, rep(3,min_val))

resp = as.data.frame(y)
colnames(resp) = c('RESPONSE')

ldata = list("df" = resp,
             "x" = fdata(all.smooth),
             "x.d1" = all.d1)

res.DD<-classif.DD(y,all.d1,classif=settings.classif,depth=settings.depth)
res.DD$prob.classification

real = c(rep(1, min_val+1), rep(2, min_val+1)) #, rep(3,min_val+1))
all.test = cbind(test_1, test_2, test_3)
all.test = all.test[,14]
all.test = t(all.test)
rownames(all.test) <- NULL

all.test.smooth = fda.smooth(all.test, nbasis = nbasis, type.basis = nbasis.type)
all.test.d1 = fdata.deriv(all.test.smooth, nderiv = settings.nderiv)
tdata = list('x'=fdata(all.test.smooth), 'x.d1'=all.test.d1)
pred = predict.classif.DD(res.DD, all.test.d1)
pred

vect = c()
for (x in 1:(min_val*2+2)) {
  all.test = cbind(test_1, test_2, test_3)
  all.test = all.test[,x]
  all.test = t(all.test)
  all.test.smooth = fda.smooth(all.test, nbasis = nbasis, type.basis = nbasis.type)
  all.test.d1 = fdata.deriv(all.test.smooth, nderiv = settings.nderiv)
  pred = predict.classif.DD(res.DD, all.test.d1)
  vect = c(vect, as.vector(pred))
  message(x, ' ', pred)
  
}

table(as.vector(vect),real)

