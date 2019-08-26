
# pep_deer_thermo contains theoretical  seq + nhyd nglut     mass1 seqpos missed.cleaves
#deer data contains one spot with 3 replicates

ms_fit(pep_deer_thermo,deer_data[[1]],doplot=T,force=F)

require(bacollite, quietly = T) 

lagg_plot(pep_deer_thermo,deer_data[[1]])



# cd1 contains mass and probability theoretical peptides
cd1 <- ms_iso(pep_deer_thermo$seq[1],ndeamidations=pep_deer_thermo$nglut[1],nhydroxylations=pep_deer_thermo$nhyd[1])
lags <- matrix(nrow=nrow(pep_deer_thermo),ncol=3)
lags[] <- 0
moff <- 1.5
#lbl and ubl for mass

lbl <- min(cd1$mass) - moff# + (peptides$nglut[i]*0.984015)+(peptides$nhyd[i]*16)
ubl <- max(cd1$mass) + moff# + (peptides$nglut[i]*0.984015)+(peptides$nhyd[i]*16)
subms1 <- ms_subrange(deer_data[[1]]$s1,lbl,ubl)
subms2 <- ms_subrange(deer_data[[1]]$s2,lbl,ubl)
subms3 <- ms_subrange(deer_data[[1]]$s3,lbl,ubl)

#combining both
myxlim = c(lbl,ubl)

#cd1 assigned to cdshift(mass , probability)
cdshift <-cd1

#align1,2,3, contains cor    lag 
align1 <- ms_align(cdshift,subms1,myxlim)
align2 <- ms_align(cdshift,subms2,myxlim)
align3 <- ms_align(cdshift,subms3,myxlim)


######filling the lags....

lags[1,1]<-align1$lag
lags[1,2]<-align2$lag
lags[1,3]<-align3$lag




#---------------------------------------------------------------------


myby <- 0.005 #125
#mylagmax gives the 'reverse scaling' of the stepsize - useful when comparing etc.
mylagmax <- 1/myby

#Xout generated values between two ranges with myby(0.005 difference)

xout = seq(from = myxlim[1], to = myxlim[2], by = myby)

#Resample against xout.

#yii ontains mass and intensity of experimental peptidesas  a length of generated xout
yii <- approx(x=subms1[,1], y=subms1[,2], xout=xout, method="linear", rule = 2)

#renormalise this segment, normalised intensities
yii$y = yii$y/max(yii$y)

#intensities from sample 1
subms1[,2]
                 

#yir contains mass and prob from theoretical peptides
yri <- approx(x=cd1$mass,y=cd1$prob,xout=xout, method="linear", rule = 2)
yri$y[] <-0 #prob =0


# for loop for replacing the 0 probabilities with 
for(i in 1:length(cd1$prob)){
    idx <- which.min(abs(yri$x-cd1$mass[i]))
    # idx is contains minimum values position of absolute values of both masses
    yri$y[idx] <- cd1$prob[i]
  }

## yri$x theroretical pep mass and yii$x experimental peptide mass, auto croos covariance
ccd1 <- ccf(yri$x,yii$x,ylim=c(-0.1,0.5),plot=doplot,axes=F, lag.max = mylagmax, main = "")
mass = ccd1$acf[,,1] # correlation
lag = ccd1$lag[,,1] # lag
res = data.frame(mass,lag*myby) # muliply by myby
##



res_max = res[which.max(res$cor),]

out <- data.frame(
    cor = res_max$cor,
    lag = res_max$lag * myby
  )

laglim = 0.3

lag_range = c(-laglim,laglim)
lagset = res[ (res$lag > -(laglim/myby)) & (res$lag < (laglim/myby)),]

res_lrmax = lagset[which.max(lagset$cor),]


plot(yii,type="l",col="red")

lines(yri)
par(mar=c(0.9,2.3,2.9,.3), mfrow = c(3,1), oma=c(5,0,2,0))

title(sprintf("Data resampled to resolution %0.2f Da",myby), line = "-2")




points(x=res_max$lag,y=res_max$cor,pch=19,col="red")
points(x=res_lrmax$lag,y=res_lrmax$cor,pch=10,col="green",cex = 3)


deer_list <- deer_data[[1]]
ts <- data.frame()
ts <- lagg_plot(pep_deer_thermo,deer_list)
