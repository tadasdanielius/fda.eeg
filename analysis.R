source('data init.R')

eeg.plot = proj.env$fda.plot

message('nbasis=',proj.env$config$nbasis, ' nbasis type=',proj.env$config$type.basis)

# First look. Plot smoothed data
par(mfrow = c(1,1))
plot(eeg.event.data.smooth, main='Smoothed Data')

# Smoothed data
par(mfrow = c(2,2))
plot(eeg.event.data.fdata[1,], main="Channel 1", col='gray')
lines(eeg.event.data.smooth[1,],col='red')

plot(eeg.event.data.fdata[25,], main="Channel 25", col='gray')
lines(eeg.event.data.smooth[25,],col='red')

plot(eeg.event.data.fdata[50,], main="Channel 50", col='gray')
lines(eeg.event.data.smooth[50,],col='red')

plot(eeg.event.data.fdata[60,], main="Channel 60", col='gray')
lines(eeg.event.data.smooth[60,],col='red')


bs.smoothed = fdata2fd(eeg.event.data.fdata,'bspline', 39)

par(mfrow = c(2,2))
plot(eeg.event.data.fdata[1,], main="Channel 1", col='gray')
lines(bs.smoothed[1,],col='red')
lines(eeg.event.data.smooth[1,],col='blue')


plot(eeg.event.data.fdata[25,], main="Channel 25", col='gray')
lines(bs.smoothed[25,],col='red')
lines(eeg.event.data.smooth[25,],col='blue')

plot(eeg.event.data.fdata[50,], main="Channel 50", col='gray')
lines(bs.smoothed[50,],col='red')
lines(eeg.event.data.smooth[50,],col='blue')

plot(eeg.event.data.fdata[60,], main="Channel 60", col='gray')
lines(bs.smoothed[60,],col='red')
lines(eeg.event.data.smooth[60,],col='blue')

ft_mean = mean.fd(eeg.event.data.smooth)
bs_mean = mean.fd(bs.smoothed)

plot(ft_mean,col='red', main='Average')
lines(bs_mean,col='blue')


#Assessing fit
eeg.data.mat = eval.fd(seq(1,251), eeg.event.data.smooth)
eeg.data.res = t(eeg.event.data) - eeg.data.mat
eeg.data.var1 = apply(eeg.data.res^2, 1, sum)/60
eeg.data.var2 = apply(eeg.data.res^2, 2, sum)/(250-25)

plot(sqrt(eeg.data.var2), xlab='Channel',ylab='Standard Deviation accross channel')
plot(sqrt(eeg.data.var1), xlab='Time',ylab='Standard Deviation accross time interval')


eeg.data.stddev.fd = fdata2fd(fdata(log(eeg.data.var1))/2, type.basis = 'bspline', nbasis = 50)
eeg.data.stddev.var = exp(eval.fd(seq(1,251), eeg.data.stddev.fd))
lines(eeg.data.stddev.var, col='red')

resvar1 = apply(res^2, 1, sum)/60
resvar2 = apply(res^2, 2, sum)/250

# PCA
eeg.pca = pca.fd(eeg.event.data.smooth,nharm=5)

plot_scatter(eeg.pca,1,2)
plot_scatter(eeg.pca,1,3)
plot_scatter(eeg.pca,1,4)
plot_scatter(eeg.pca,1,5)

plot_scatter(eeg.pca,2,3)
plot_scatter(eeg.pca,2,4)
plot_scatter(eeg.pca,2,5)

plot_scatter(eeg.pca,3,4)
plot_scatter(eeg.pca,2,5)

plot_scatter(eeg.pca,4,5)


#Reconstruct with pca
cumsum(eeg.pca$varprop)

PCs = eeg.pca$harmonics
scores = eeg.pca$scores
pca_1 = eeg.pca$meanfd + scores[1,1]* PCs[1]
pca_2 = pca_1 + scores[1,2] * PCs[2]
pca_3 = pca_2 + scores[1,3] * PCs[3]
pca_4 = pca_3 + scores[1,4] * PCs[4]
pca_5 = pca_4 + scores[1,5] * PCs[5]

plot(eeg.event.data.fdata[1,])
lines(pca_5, col='blue', type='l', lty=2)
lines((eeg.event.data.smooth[1,] - pca_5), col='red')



# Residuals PCA
par(mfrow=c(1,1))
plot(fdata(res))
res.pca = fdata2pc(fdata(res))
summary(res.pca)

# Clustering
eeg.clusters = fda.cluster(eeg.event.data.smooth,ncl=3)
eeg.plot$plot.clusters(eeg.clusters)
eeg.plot$plot.clusters.charts(eeg.event.data.smooth, eeg.clusters$cluster)

eeg.clusters = kmeans.fd(eeg.event.data.smooth, ncl=3,draw = F)
eeg.plot$plot.clusters(eeg.clusters)

# cognitive events
plot(mean.fd(get_event_data(proj.env$config$codes$left_hand, 2, 9, -1, T)[17:45,]), main='Left hand movement event')
abline(-250,1, col='blue', lwd=4)
abline(-2000,1, col='blue', lwd=4)

plot(deriv.fd(deriv.fd(deriv.fd(mean.fd(get_event_data(proj.env$config$codes$left_hand, 2, 9, -1, T)[17:45,])))), main="Left hand movement event. Third level derivative")
abline(-250,1, col='blue', lwd=4)
abline(-2000,1, col='blue', lwd=4)

