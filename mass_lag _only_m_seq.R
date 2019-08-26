# mass_lag function to crate lag values from ms allign
# theroretical_pep for the insilico digested peptides
# sample_data_data for the experimental sample data


mass_lag_seq<- function(theroretical_pep,sample_data_data, use_ms_iso=T){
#standards ->theroretical_pep
#calibrant_data[[i]]->sample_data_data
  count <- 0
  moff <- 1.5
  lags <- matrix(nrow=nrow(theroretical_pep),ncol=3)
  lags[] <- 0

  cd1<- list()
  for (i in 1:nrow(theroretical_pep)){
    #theroretical_pep <- theroretical_pep[theroretical_pep$seq==i,]
    cd1 <- ms_iso(theroretical_pep$seq[i])

    if((max(cd1$mass) > 800 && min(cd1$mass) < 3500)) {

      count = count +1

      lbl <- min(cd1$mass) - moff# + (theroretical_pep$nglut[i]*0.984015)+(theroretical_pep$nhyd[i]*16)
      ubl <- max(cd1$mass) + moff# + (theroretical_pep$nglut[i]*0.984015)+(theroretical_pep$nhyd[i]*16)

      subms1 <- ms_subrange(sample_data_data$s1,lbl,ubl)
      subms2 <- ms_subrange(sample_data_data$s2,lbl,ubl)
      subms3 <- ms_subrange(sample_data_data$s3,lbl,ubl)



      cdshift <-cd1

      myxlim = c(lbl,ubl)

      #from the ms_alling we are calculating the lag
      align1 <- ms_align(cdshift,subms1,myxlim)
      align2 <- ms_align(cdshift,subms2,myxlim)
      align3 <- ms_align(cdshift,subms3,myxlim)

      #storing the lag in matrix format for each sample
      lags[i,1]<-align1$lag
      lags[i,2]<-align2$lag
      lags[i,3]<-align3$lag


      }
  }

  #ts dataframe contains "mass","lag1","lag2","lag3"
  ts <- data.frame(mass=theroretical_pep$mass,lag=lags)
  colnames(ts)<- c("mass","lag1","lag2","lag3")
  return(ts)
  }




