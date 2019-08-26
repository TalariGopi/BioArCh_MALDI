library("MALDIquant")
library("MALDIquantForeign")
library("MALDIrppa")

## get example directory
Directory1 <- "D:/MALDIQuant/Mass-up/Calf/00_Calf_150"
Directory2 <- "D:/MALDIQuant/Mass-up/Goat/00_Goat_150"
Directory3 <- "D:/MALDIQuant/Mass-up/Sheep/00_Sheep_150"

## import txt files
spectra1 <- import(Directory1)
spectra2 <- import(Directory2)
spectra3 <- import(Directory3)
# Backup <- spectra
# spectra <- Backup[1]
# import csv files

#spectra2<- spectra[100]

# Pre-processing parameter settings
thScale <- 2.5 # Smoothing
ite <- 100 # Baseline correction
SigNoi <- 6 # Peak extraction
hws <- 20 # Peak extraction
tol <- 0.003 # Peak binning
               
# sc.results <- screenSpectra(spectra)
# summary(sc.results)
#plot(sc.results, labels = TRUE)

#pdf(file = "FileName.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
plot(spectra1[[1]])
#dev.off()


#spectra <- sc.results$fspectra # Filtered list of mass spectra
#type <- sc.results$fmeta # Filtered metadata

#------------Transform intensity--------------

spectra1 <- transfIntensity(spectra1, fun = "sqrt")
spectra2 <- transfIntensity(spectra2, fun = "sqrt")
spectra3 <- transfIntensity(spectra3, fun = "sqrt")

plot(spectra1[[1]])

#------------------Smoothing-------------------

spectra1 <- smoothIntensity(spectra1, method="SavitzkyGolay", halfWindowSize=10)
spectra2 <- smoothIntensity(spectra2, method="SavitzkyGolay", halfWindowSize=10)
spectra3 <- smoothIntensity(spectra3, method="SavitzkyGolay", halfWindowSize=10)


plot(spectra1[[1]])

#---------------------Baseline-------------------

baseline <- estimateBaseline(spectra1[[1]], method="SNIP", iterations=100)
plot(spectra1[[1]])
lines(baseline, col="red", lwd=2)


spectra1 <- removeBaseline(spectra1, method = "SNIP", iterations = 100)
spectra2 <- removeBaseline(spectra2, method = "SNIP", iterations = 100)
spectra3 <- removeBaseline(spectra3, method = "SNIP", iterations = 100)

plot(spectra1[[1]])


#---------------Calibrate intensity-----------

spectra1 <- calibrateIntensity(spectra1, method = "TIC") #Total Ion current
pectra12 <- calibrateIntensity(spectra1, method = "TIC") #Total Ion current
pectra13 <- calibrateIntensity(spectra1, method = "TIC") #Total Ion current

plot(spectra1[[1]])

#----------------Estimate Noisy----------------------------------

noise <- estimateNoise(spectra1[[1]])
plot(spectra1[[1]], xlim=c(700, 3500), ylim=c(0, 0.002))
lines(noise, col="red")
lines(noise[,1], noise[, 2]*2, col="blue")


peaks1 <- detectPeaks(spectra1, SNR = 5, halfWindowSize = 10)
peaks2 <- detectPeaks(spectra2, SNR = 5, halfWindowSize = 10)
peaks3 <- detectPeaks(spectra3, SNR = 5, halfWindowSize = 10)


plot(peaks1[[1]])
plot(peaks2[[1]])
plot(peaks3[[1]])

points(peaks2[[1]], col="red", pch=4)


#-----------------Peak allignment---------------------------------

peaks1 <- alignPeaks(peaks1, minFreq = 0.8, tolerance = 0.003)
peaks2 <- alignPeaks(peaks2, minFreq = 0.8, tolerance = 0.003)
peaks3 <- alignPeaks(peaks3, minFreq = 0.8, tolerance = 0.003)


countPeaks(peaks3)
plot(spectra1[[1]])

#------------------Peak binning--------------------------------

peaks1 <- binPeaks(peaks1, tolerance=0.003)
peaks2 <- binPeaks(peaks2, tolerance=0.003)
peaks3 <- binPeaks(peaks3, tolerance=0.003)


plot(spectra[[1]])

#----------------Feature matrix------------------------------

peaks1 <- filterPeaks(peaks1, minFrequency=0.25)
peaks2 <- filterPeaks(peaks2, minFrequency=0.25)
peaks3 <- filterPeaks(peaks3, minFrequency=0.25)


plot(spectra[[1]])


featureMatrix1 <- intensityMatrix(peaks1,spectra1)
featureMatrix2 <- intensityMatrix(peaks2,spectra2)
featureMatrix3 <- intensityMatrix(peaks3,spectra3)

head(featureMatrix1 [,1:3]) # check matrix

#-------------------Export/Save peaks ------------------------------

export(peaks1, type="csv", path="D:/MALDIQuant/Mass-up/Mass_UP_Input/Calf", force=FALSE)
export(peaks2, type="csv", path="D:/MALDIQuant/Mass-up/Mass_UP_Input/Goat", force=FALSE)
export(peaks3, type="csv", path="D:/MALDIQuant/Mass-up/Mass_UP_Input/Sheep", force=FALSE)

write.csv(featureMatrix1, file = "D:/MALDIQuant/Mass-up/Calf_matrix.csv", col.names = T,row.names = T, sep = ",")
write.csv(featureMatrix2, file = "D:/MALDIQuant/Mass-up/Goat_matrix.csv", col.names = T,row.names = T, sep = ",")
write.csv(featureMatrix3, file = "D:/MALDIQuant/Mass-up/Sheep_matrix.csv", col.names = T,row.names = T, sep = ",")



#------------------------------------------------------------
# plot(cP, type = "n")
# text(cP, label = 1:length(cP))

#---------------------------------------

# pdf(file = "FileName.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
# peakPatterns(peaks)
# dev.off()
# 
# 
# out <- detectOutliers(peaks, type$Isolate, binary = TRUE)
# 
# peaks.clean <- peaks[out == FALSE] # Discard outlying peak profiles
# type.clean <- type[out == FALSE]  # and corresponding metadata
# 
# 
# 
# peaks.clean.f <- filterPeaks(peaks.clean, minFreq = 0.25)
# 
# 
# peakPatterns(peaks.clean.f)
# 
# 
# peaks.clean.fm <- mergeMassPeaks(peaks.clean.f, method = "median")
# 
# int.clean.fm <- intensityMatrix(peaks.clean.fm)
# 
