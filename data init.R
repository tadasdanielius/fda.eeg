source('include.R')

event.matrix = proj.env$eeg_data$EEG$event.matrix
eeg.sample = proj.env$eeg_data$sample
fda.smooth = proj.env$fda$smooth
fda.cluster = proj.env$fda$cluster

events.left_hand = event.matrix[event.matrix$TYPE == config$codes$left_hand & event.matrix$VALID == TRUE,]
eeg.event = events.left_hand[1,]

eeg.event.data = eeg.sample(eeg.event$SECOND,1)

eeg.event.data.fdata = fdata(eeg.event.data)
eeg.event.data.smooth = fda.smooth(eeg.event.data.fdata)

