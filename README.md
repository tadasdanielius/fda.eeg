# fda.eeg
Coursework for Functional Data Analysis course.

## Introduction

In this assignment we are going to investigate a data-driven approach and apply functional data analysis toolbox on EEG brain waves
Using FDA apprach we will try to look into possible ways to detect cognitive events.
The data is taken from  "[BCI Competition III](http://www.bbci.de/competition/iii/)"

## Data

The recording was made with a 60 channel EEG amplifier from Neuroscan, using the left mastoid for reference and right mastoid as ground. The EEG was sampled with 250 Hz, it was
filtered between 1 and 50hz with Notchfilter on. 60 EEG channels were recorded according to this scheme.

![scheme](https://github.com/tadasdanielius/fda.eeg/blob/master/images/fig1.jpeg)

### Steps

* Fit the data using [Fourier basis system](https://en.wikipedia.org/wiki/Fourier_series)
* Assess the fit to the data
* Functional Principal Component analysis
* Clustering
* Cognitive events in EEG. Visually inspect [Mu waves](https://en.wikipedia.org/wiki/Mu_wave)
* Classification of cognitive events using functional data

Full description can be found [here](https://github.com/tadasdanielius/fda.eeg/blob/master/coursework.pdf)
