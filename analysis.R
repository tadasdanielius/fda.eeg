source('data init.R')

eeg.plot = proj.env$fda.plot


message('nbasis=',proj.env$config$nbasis, ' nbasis type=',proj.env$config$type.basis)
# First look. Plot smoothed data
par(mfrow = c(1,1))
plot(eeg.event.data.smooth, main='Smoothed Data')

# Variance 
par(mfrow = c(1,1))
eeg.var = func.var(eeg.event.data.smooth)
plot(eeg.var, main='Variance')

# Mean
par(mfrow = c(1,1))
eeg.event.data.mean = func.mean(eeg.event.data.smooth)
plot(eeg.event.data.mean, main='Mean')



# Derivatives
par(mfrow = c(1,1))
eeg.deriv = fdata.deriv(eeg.event.data.smooth, nderiv = 1)
plot(eeg.deriv, main='Derivatives')


# variance of the deriv
par(mfrow = c(1,1))
eeg.deriv.var = func.var(eeg.deriv)
plot(eeg.deriv.var, main='Variance of derivatives')

# Mean of derivatives
par(mfrow = c(1,1))
eeg.deriv.mean = func.mean(eeg.deriv)
plot(eeg.deriv.mean, main='mean of derivatives')

#clustering
set.seed(0)
eeg.clusters = fda.cluster(eeg.event.data.smooth)
eeg.plot$plot.clusters(eeg.clusters)
eeg.plot$plot.clusters.charts(eeg.event.data.smooth, eeg.clusters$cluster)

set.seed(0)
eeg.clusters = fda.cluster(eeg.event.data.smooth, ncl = 3)
eeg.plot$plot.clusters(eeg.clusters)
eeg.plot$plot.clusters.charts(eeg.event.data.smooth, eeg.clusters$cluster)

#clustering derivative
set.seed(0)
eeg.deriv.clusters = fda.cluster(eeg.deriv, ncl = 3)
eeg.plot$plot.clusters(eeg.deriv.clusters)
eeg.plot$plot.clusters.charts(eeg.deriv, eeg.deriv.clusters$cluster)

# FPCA
eeg.event.data.smooth.fd = fdata(eeg.event.data.smooth)
eeg.event.data.pca3 = fdata2pc(eeg.event.data.smooth.fd,ncomp=3,l=5)
summary(eeg.event.data.pca3)

# FPCA deriv
eeg.deriv.pca3 = fdata2pc(eeg.deriv,ncomp=3,l=5)
summary(eeg.deriv.pca3)
