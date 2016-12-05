source('include.R')
eeg.plot = proj.env$fda.plot


get_event_data = function(evt, idx, duration=1) {
  evt.list = event.matrix[event.matrix$TYPE == evt & event.matrix$VALID == TRUE,]
  eeg.evt = evt.list[idx,]
  evt.data = eeg.sample(eeg.evt$SECOND, duration+3)
  evt.fdata = fdata(evt.data)
  eeg.smooth = fda.smooth(evt.fdata)
  return(eeg.smooth)
}

events.left_hand = event.matrix[event.matrix$TYPE == config$codes$left_hand & event.matrix$VALID == TRUE,]
# events.foot_towards_right = event.matrix[event.matrix$TYPE == config$codes$foot_towards_right & event.matrix$VALID == TRUE,]
# 
# eeg.event = events.left_hand[1,]
# eeg.event_foot_towards_right = events.foot_towards_right[1,]
# 
# eeg.event.data = eeg.sample(eeg.event$SECOND,1)
# eeg.event.data.foot_towards_right = eeg.sample(eeg.event_foot_towards_right$SECOND,1)
# 
# 
# eeg.event.data.fdata = fdata(eeg.event.data)
# eeg.event.data.fdata_foot_towards_right = fdata(eeg.event.data.foot_towards_right)
# 
# eeg.event.data.smooth = fda.smooth(eeg.event.data.fdata)
# eeg.event.data.smooth_foot_towards_right= fda.smooth(eeg.event.data.fdata_foot_towards_right)
# 
# 
# plot(eeg.event.data.smooth)
# plot(eeg.event.data.smooth_foot_towards_right)
# 
# tperm.fd(eeg.event.data.smooth, eeg.event.data.smooth_foot_towards_right)

foot1 = get_event_data(config$codes$foot_towards_right, 1)
foot2 = get_event_data(config$codes$foot_towards_right, 2)
foot3 = get_event_data(config$codes$foot_towards_right, 3)
foot4 = get_event_data(config$codes$foot_towards_right, 4)
foot5 = get_event_data(config$codes$foot_towards_right, 5)

# foot1.d = fdata.deriv(foot1)
# foot2.d = fdata.deriv(foot2)
# foot3.d = fdata.deriv(foot3)
# foot4.d = fdata.deriv(foot4)


foot12.d = deriv.fd(foot1,2)
foot22.d = deriv.fd(foot2,2)
foot32.d = deriv.fd(foot3,2)
foot42.d = deriv.fd(foot4,2)
foot52.d = deriv.fd(foot5,2)

foot1.d = deriv.fd(foot1)
foot2.d = deriv.fd(foot2)
foot3.d = deriv.fd(foot3)
foot4.d = deriv.fd(foot4)
foot5.d = deriv.fd(foot5)



hand1 = get_event_data(config$codes$left_hand,1)
hand2 = get_event_data(config$codes$left_hand,2)
hand3 = get_event_data(config$codes$left_hand,3)
hand4 = get_event_data(config$codes$left_hand,4)

hand1.d = deriv.fd(hand1)
hand2.d = deriv.fd(hand2)
hand3.d = deriv.fd(hand3)
hand4.d = deriv.fd(hand4)

hand12.d = deriv.fd(hand1,2)
hand22.d = deriv.fd(hand2,2)
hand32.d = deriv.fd(hand3,2)
hand42.d = deriv.fd(hand4,2)


plot(var.fd(foot2, foot5))

plot(var.fd(hand2.d, tongue1.d))


# hand1.d = fdata.deriv(left_hand1)
# hand2.d = fdata.deriv(left_hand2)
# hand3.d = fdata.deriv(left_hand3)
# hand4.d = fdata.deriv(left_hand4)


plot(var.fd(foot2, hand2))

tongue1 = get_event_data(config$codes$tongue, 1)
tongue2 = get_event_data(config$codes$tongue, 2)
tongue3 = get_event_data(config$codes$tongue, 3)
tongue4 = get_event_data(config$codes$tongue, 4)
tongue5 = get_event_data(config$codes$tongue, 5)
  

tongue1.d = deriv.fd(tongue1)
tongue2.d = deriv.fd(tongue2)
tongue3.d = deriv.fd(tongue3)
tongue4.d = deriv.fd(tongue4)

tongue12.d = deriv.fd(tongue1,2)
tongue22.d = deriv.fd(tongue2,2)
tongue32.d = deriv.fd(tongue3,2)
tongue42.d = deriv.fd(tongue4,2)

tperm.fd(foot22.d, hand12.d)



set.seed(2)
eeg.clusters = fda.cluster(tongue4,ncl = 2)
eeg.plot$plot.clusters(eeg.clusters)
eeg.plot$plot.clusters.charts(foot1, eeg.clusters$cluster)


clusters = eeg.clusters$cluster
which(clusters %in% 1)

pdf(file="C:\\temp\\plots\\tperm_foot_foot_d1_c1c2.pdf")  
tperm.fd(foot12.d[which(clusters %in% 1)],    foot12.d[which(clusters %in% 2)])
tperm.fd(foot12.d[which(clusters %in% 1)],    foot22.d[which(clusters %in% 2)])
tperm.fd(foot12.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 2)])
tperm.fd(foot12.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 2)])
tperm.fd(foot22.d[which(clusters %in% 1)],    foot22.d[which(clusters %in% 2)])
tperm.fd(foot22.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 2)])
tperm.fd(foot22.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 2)])
tperm.fd(foot32.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 2)])
tperm.fd(foot32.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 2)])
tperm.fd(foot42.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 2)])
dev.off()




tperm.fd(foot22.d[which(clusters %in% 1)],    foot22.d[which(clusters %in% 2)], nperm=500)

pdf(file="C:\\temp\\plots\\tperm_footc1_footc1_d2.pdf")  
tperm.fd(foot12.d[which(clusters %in% 1)],    foot22.d[which(clusters %in% 1)])
tperm.fd(foot12.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 1)])
tperm.fd(foot12.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 1)])
tperm.fd(foot22.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 1)])
tperm.fd(foot22.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 1)])
tperm.fd(foot32.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 1)])
dev.off()

pdf(file="C:\\temp\\plots\\tperm_footc1_footc1_d1.pdf")  
tperm.fd(foot1.d[which(clusters %in% 1)],    foot2.d[which(clusters %in% 1)])
tperm.fd(foot1.d[which(clusters %in% 1)],    foot3.d[which(clusters %in% 1)])
tperm.fd(foot1.d[which(clusters %in% 1)],    foot4.d[which(clusters %in% 1)])
tperm.fd(foot2.d[which(clusters %in% 1)],    foot3.d[which(clusters %in% 1)])
tperm.fd(foot2.d[which(clusters %in% 1)],    foot4.d[which(clusters %in% 1)])
tperm.fd(foot3.d[which(clusters %in% 1)],    foot4.d[which(clusters %in% 1)])
dev.off()



pdf(file="C:\\temp\\plots\\tperm_footc1_tonguec2_d2.pdf")  
tperm.fd(foot12.d[which(clusters %in% 1)],    tongue22.d[which(clusters %in% 2)])
tperm.fd(foot12.d[which(clusters %in% 1)],    tongue32.d[which(clusters %in% 2)])
tperm.fd(foot12.d[which(clusters %in% 1)],    tongue42.d[which(clusters %in% 2)])
tperm.fd(foot22.d[which(clusters %in% 1)],    tongue32.d[which(clusters %in% 2)])
tperm.fd(foot22.d[which(clusters %in% 1)],    tongue42.d[which(clusters %in% 2)])
tperm.fd(foot32.d[which(clusters %in% 1)],    tongue42.d[which(clusters %in% 2)])
dev.off()

pdf(file="C:\\temp\\plots\\tperm_footc1_tonguec1_d2.pdf")  
tperm.fd(foot12.d[which(clusters %in% 1)],    tongue22.d[which(clusters %in% 1)])
tperm.fd(foot12.d[which(clusters %in% 1)],    tongue32.d[which(clusters %in% 1)])
tperm.fd(foot12.d[which(clusters %in% 1)],    tongue42.d[which(clusters %in% 1)])
tperm.fd(foot22.d[which(clusters %in% 1)],    tongue32.d[which(clusters %in% 1)])
tperm.fd(foot22.d[which(clusters %in% 1)],    tongue42.d[which(clusters %in% 1)])
tperm.fd(foot32.d[which(clusters %in% 1)],    tongue42.d[which(clusters %in% 1)])
dev.off()

pdf(file="C:\\temp\\plots\\tperm_footc1_tonguec1_d1.pdf")  
tperm.fd(foot1.d[which(clusters %in% 1)],    tongue2.d[which(clusters %in% 1)])
tperm.fd(foot1.d[which(clusters %in% 1)],    tongue3.d[which(clusters %in% 1)])
tperm.fd(foot1.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)])
tperm.fd(foot2.d[which(clusters %in% 1)],    tongue3.d[which(clusters %in% 1)])
tperm.fd(foot2.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)])
tperm.fd(foot3.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)])
dev.off()


pdf(file="C:\\temp\\plots\corr_foot_foot_d1_c2.pdf")  
plot(var.fd(foot1.d[which(clusters %in% 1)],    foot2.d[which(clusters %in% 2)]))
plot(var.fd(foot1.d[which(clusters %in% 1)],    foot3.d[which(clusters %in% 2)]))
plot(var.fd(foot1.d[which(clusters %in% 1)],    foot4.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    foot3.d[which(clusters %in% 2)]))
plot(var.fd(foot3.d[which(clusters %in% 1)],    foot4.d[which(clusters %in% 2)]))
dev.off()



plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)]))


par(mfrow = c(2,1))
plot(var.fd(foot1.d[which(clusters %in% 2)],    foot4.d[which(clusters %in% 2)]))


plot(cca.fd(foot3.d[which(clusters %in% 2)],    hand3.d[which(clusters %in% 2)]))
plot(cca.fd(foot1, foot2, ncan = 2))


somePDFPath = "C:\\temp\\corr_foot_foot_d1_c2.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot1.d[which(clusters %in% 2)],    foot2.d[which(clusters %in% 2)]))
plot(var.fd(foot1.d[which(clusters %in% 2)],    foot3.d[which(clusters %in% 2)]))
plot(var.fd(foot1.d[which(clusters %in% 2)],    foot4.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    foot3.d[which(clusters %in% 2)]))
plot(var.fd(foot3.d[which(clusters %in% 2)],    foot4.d[which(clusters %in% 2)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_d2_c2.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot12.d[which(clusters %in% 2)],    foot22.d[which(clusters %in% 2)]))
plot(var.fd(foot12.d[which(clusters %in% 2)],    foot32.d[which(clusters %in% 2)]))
plot(var.fd(foot12.d[which(clusters %in% 2)],    foot42.d[which(clusters %in% 2)]))
plot(var.fd(foot22.d[which(clusters %in% 2)],    foot32.d[which(clusters %in% 2)]))
plot(var.fd(foot32.d[which(clusters %in% 2)],    foot42.d[which(clusters %in% 2)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_hand_d1.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot1.d[which(clusters %in% 2)],    hand2.d[which(clusters %in% 2)]))
plot(var.fd(foot1.d[which(clusters %in% 2)],    hand3.d[which(clusters %in% 2)]))
plot(var.fd(foot1.d[which(clusters %in% 2)],    hand4.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    hand3.d[which(clusters %in% 2)]))
plot(var.fd(foot3.d[which(clusters %in% 2)],    hand4.d[which(clusters %in% 2)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_hand_d1_all.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot1.d,    hand2.d))
plot(var.fd(foot1.d,    hand3.d))
plot(var.fd(foot1.d,    hand4.d))
plot(var.fd(foot2.d,    hand3.d))
plot(var.fd(foot3.d,    hand4.d))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_hand_d2_all.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot12.d,    hand22.d))
plot(var.fd(foot12.d,    hand32.d))
plot(var.fd(foot12.d,    hand42.d))
plot(var.fd(foot22.d,    hand32.d))
plot(var.fd(foot32.d,    hand42.d))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_d2_all.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot1.d,    tongue2.d))
plot(var.fd(foot1.d,    tongue3.d))
plot(var.fd(foot1.d,    tongue4.d))
plot(var.fd(foot2.d,    tongue3.d))
plot(var.fd(foot3.d,    tongue4.d))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_hand_d2_c1.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot12.d[which(clusters %in% 1)],    hand22.d[which(clusters %in% 1)]))
plot(var.fd(foot12.d[which(clusters %in% 1)],    hand32.d[which(clusters %in% 1)]))
plot(var.fd(foot12.d[which(clusters %in% 1)],    hand42.d[which(clusters %in% 1)]))
plot(var.fd(foot22.d[which(clusters %in% 1)],    hand32.d[which(clusters %in% 1)]))
plot(var.fd(foot32.d[which(clusters %in% 1)],    hand42.d[which(clusters %in% 1)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_foot_d2_c1.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot12.d[which(clusters %in% 1)],    foot22.d[which(clusters %in% 1)]))
plot(var.fd(foot12.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 1)]))
plot(var.fd(foot12.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 1)]))
plot(var.fd(foot22.d[which(clusters %in% 1)],    foot32.d[which(clusters %in% 1)]))
plot(var.fd(foot32.d[which(clusters %in% 1)],    foot42.d[which(clusters %in% 1)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_tongue_d1_c1.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue1.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue2.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue3.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)]))
plot(var.fd(foot3.d[which(clusters %in% 1)],    tongue1.d[which(clusters %in% 1)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_tongue_d1_c1.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue1.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue2.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue3.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)]))
plot(var.fd(foot3.d[which(clusters %in% 1)],    tongue1.d[which(clusters %in% 1)]))
dev.off()

somePDFPath = "C:\\temp\\corr_foot_tongue_d1_c2.pdf"
pdf(file=somePDFPath)  
plot(var.fd(foot2.d[which(clusters %in% 2)],    tongue1.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    tongue2.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    tongue3.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    tongue4.d[which(clusters %in% 2)]))
plot(var.fd(foot3.d[which(clusters %in% 2)],    tongue1.d[which(clusters %in% 2)]))
dev.off()


plot(var.fd(foot2.d[which(clusters %in% 2)],    foot1.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    foot2.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    foot3.d[which(clusters %in% 2)]))
plot(var.fd(foot2.d[which(clusters %in% 2)],    foot4.d[which(clusters %in% 2)]))
plot(var.fd(foot3.d[which(clusters %in% 2)],    foot1.d[which(clusters %in% 2)]))

plot(var.fd(foot2.d[which(clusters %in% 1)],    foot1.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    foot2.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    foot3.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    foot4.d[which(clusters %in% 1)]))
plot(var.fd(foot3.d[which(clusters %in% 1)],    foot1.d[which(clusters %in% 1)]))

plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue1.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue2.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue3.d[which(clusters %in% 1)]))
plot(var.fd(foot2.d[which(clusters %in% 1)],    tongue4.d[which(clusters %in% 1)]))
plot(var.fd(foot3.d[which(clusters %in% 1)],    tongue1.d[which(clusters %in% 1)]))

plot(var.fd(foot2[which(clusters %in% 1)],    foot1[which(clusters %in% 1)]))
plot(var.fd(foot2[which(clusters %in% 1)],    foot2[which(clusters %in% 1)]))
plot(var.fd(foot2[which(clusters %in% 1)],    foot3[which(clusters %in% 1)]))
plot(var.fd(foot2[which(clusters %in% 1)],    foot4[which(clusters %in% 1)]))
plot(var.fd(foot3[which(clusters %in% 1)],    foot1[which(clusters %in% 1)]))

plot(var.fd(foot2[which(clusters %in% 1)],    tongue1[which(clusters %in% 1)]))
plot(var.fd(foot2[which(clusters %in% 1)],    tongue2[which(clusters %in% 1)]))
plot(var.fd(foot2[which(clusters %in% 1)],    tongue3[which(clusters %in% 1)]))
plot(var.fd(foot2[which(clusters %in% 1)],    tongue4[which(clusters %in% 1)]))
plot(var.fd(foot3[which(clusters %in% 1)],    tongue1[which(clusters %in% 1)]))
