if (!exists('proj.env')) {
  source('env.R')
}

proj.env$fda.plot = new.env(parent=emptyenv())

# plot helpers
proj.env$fda.plot$plot.circle = function (x, y, radius, col) {
  theta <- seq(0, 2 * pi, length = 200)
  lines(x = radius * cos(theta) + x,
        y = radius * sin(theta) + y,
        col = col)
}

proj.env$fda.plot$plot.clusters <- function(clstr, numbers = FALSE) {
  ncl <- length(unique(clstr$cluster))
  # Build head grid
  y <-
    c(
      1,
      rep(2, 3),
      rep(3, 5),
      rep(4, 7),
      rep(5, 9),
      rep(6, 8),
      rep(7, 9),
      rep(8, 7),
      rep(9, 5),
      rep(10, 3)
    )
  x <-
    c(6,
      5:7,
      4:8,
      3:9,
      2,
      3,
      4:10,
      1,
      2,
      4:5,
      7:8,
      10:11,
      2,
      3,
      4:10,
      3:9,
      4:8,
      5:7)
  
  par(mfrow = c(1, 1))
  if (numbers == FALSE) {
    plot(
      11 - y ~ x,
      col = clstr$cluster,
      pch = clstr$cluster + 64,
      cex = 2
    )
  } else {
    plot(
      11 - y ~ x,
      col = clstr$cluster,
      pch = clstr$cluster + 48,
      cex = 2
    )
  }
  abline(h = (seq(0.5, 11, 1)),
         col = "lightgray",
         lty = "dotted")
  abline(v = (seq(0.5, 11, 1)),
         col = "lightgray",
         lty = "dotted")
  
  proj.env$fda.plot$plot.circle(6, 5.5, 5, col = "gray")
}

proj.env$fda.plot$plot.clusters.charts <- function(data, clusters) {
  ncl <- length(unique(clusters))
  
  if (ncl > 2) {
    par(mfrow = c(round(ncl / 2), 2))  
  } else {
    par(mfrow = c(2,1))
  }
  
  for (x in 1:ncl) {
    plot(data[which(clusters %in% x)])
  }
}

plot_scatter = function(d, x, y) {
  scores = d$scores
  xval = scores[,x]
  yval = scores[,y]
  plot(xval,yval,col='gray', xlab=paste('Harmonics ',x), ylab=paste('Harmonics ', y))
  text(xval,yval, labels=seq(1,60))
}

get_event_data = function(evt, idx, duration=1, append=0, smooth=T) {
  evt.list = event.matrix[event.matrix$TYPE == evt & event.matrix$VALID == TRUE,]
  eeg.evt = evt.list[idx,]
  evt.data = eeg.sample(eeg.evt$SECOND+append, duration)
  if (smooth == F) {
    return(fdata(evt.data))
  }
  evt.fdata = fdata(evt.data)
  eeg.smooth = fda.smooth(evt.fdata)
  return(eeg.smooth)
}
