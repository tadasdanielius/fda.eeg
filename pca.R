source('main.R')
set.seed(1)


event.matrix = proj.env$eeg_data$EEG$event.matrix
eeg.sample = proj.env$eeg_data$sample
fda.smooth = proj.env$fda$smooth
fda.cluster = proj.env$fda$cluster
eeg.plot = proj.env$fda.plot



tongue.loc = event.matrix[event.matrix$TYPE == config$codes$tongue & event.matrix$VALID == TRUE,]

evt = tongue.loc[1,]
smpl = eeg.sample(evt$SECOND,1)
fd_smpl = fdata(smpl)
pc = fdata2pc(fd_smpl,l=10, ncomp=4)
summary(pc)

fd_smoothed = fda.smooth(smpl)
plot(fd_smoothed)


clstr = fda.cluster(fd_smoothed, ncl=2)
eeg.plot$plot.clusters(clstr)
eeg.plot$plot.clusters.charts(fd_smoothed, clstr$cluster)


fds = fdata(fd_smoothed)
pc2 = fdata2pc(fds,ncomp=2,l=10)
summary(pc2)


clstr = fda.cluster(fd_smoothed, ncl=4)
eeg.plot$plot.clusters.charts(fd_smoothed, clstr$cluster)
eeg.plot$plot.clusters(clstr)

clstr = fda.cluster(fd_smoothed, ncl=2)
eeg.plot$plot.clusters.charts(fd_smoothed, clstr$cluster)
eeg.plot$plot.clusters(clstr)
