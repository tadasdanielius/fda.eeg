source('data init.R')

smpl = eeg.event.data

smpl.fdata = fdata(smpl[,2:ncol(smpl)])
smpl.fd = fdata2fd(smpl.fdata, nbasis = 20)

smpl.pca = pca.fd(smpl.fd,nharm=5)
cumsum(smpl.pca$varprop)
scores = smpl.pca$scores
PCs = smpl.pca$harmonics

# Reconstruction by adding each pca
pca_1 = smpl.pca$meanfd + scores[1,1]* PCs[1]
pca_2 = pca_1 + scores[1,2] * PCs[2]
pca_3 = pca_2 + scores[1,3] * PCs[3]
pca_4 = pca_3 + scores[1,4] * PCs[4]
pca_5 = pca_4 + scores[1,5] * PCs[5]
#pca_6 = pca_5 + scores[1,6] * PCs[6]

plot(smpl.fd[1,])
lines(pca_1, col='red', type='l', lty=2)
lines(pca_2, col=3, type='l', lty=2)
lines(pca_3, col=4, type='l', lty=2)
lines(pca_4, col=5, type='l', lty=2)
lines(pca_5, col=6, type='l', lty=2)
#lines(pca_6, col=9, type='l', lty=2)

plot(smpl.fdata[1,])
lines(pca_5, col='blue', type='l', lty=2)
lines((smpl.fdata[1,] - fdata(pca_5)), col='red')

s.pca = fdata2pc(smpl.fdata, 5, norm=F)
s.scores = s.pca$u
s.pcs = s.pca$rotation

s.p1 = s.scores[1,1]*s.pcs[1]
s.p2 = s.p1 + s.scores[1,2]*s.pcs[2]
s.p3 = s.p2 + s.scores[1,3]*s.pcs[3]
s.p4 = s.p3 + s.scores[1,4]*s.pcs[4]
s.p5 = s.p4 + s.scores[1,5]*s.pcs[5]

plot(smpl.fdata[1,])
lines(s.p1, col='red', type='l', lty=2)
lines(s.p2, col=3, type='l', lty=2)
lines(s.p3, col=4, type='l', lty=2)
lines(s.p4, col=5, type='l', lty=2)
lines(s.p5, col=6, type='l', lty=2)

