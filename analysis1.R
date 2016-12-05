source('data init.R')

data.smooth = eeg.event.data.smooth

tt = 1:251
data.eval = eval.fd(tt, data.smooth)

pc = pca.fd(data.smooth, nharm = 5, centerfns = FALSE)
pc.eval = eval.fd(1:300,pc$harmonics)

plot(data.eval[,1], type='l')
lines(pc.eval[,1]*251, col='red')

fpc = fdata2pc(fdata(data.smooth), ncomp=3)
summary(fpc)


eval.fdata.comp = function(obj) {
  rotations = 
}