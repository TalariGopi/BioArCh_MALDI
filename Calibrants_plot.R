

p <- myplot(ts,overlap = F)
print(p)

png("Myplot.png",res = 600,width = 12,units = "in",height = 9)
print(p)
dev.off()
# masss_lagg function is to gerate the plots for the lag vs mass
# ts as data frame contains ts$pm1 and ts$lags 1,2,3

myplot <-  function(ts,level=0.99,overlap=T){
  
  require(ggplot2)
  colnames(ts)[1] <- "pm1"
  len <- sum(grepl("lag",colnames(ts)))
  
  df1 <- reshape2::melt(ts, id = c("pm1","Label"))
  
  pi <- list()
  model <- list()
  
  for(i in 1:len){
    
    lag <- paste0("lag",i)
    model[[i]] <- lm(df1$value[df1$variable==lag] ~ poly(df1$pm1[df1$variable==lag],3))
    
    #Generate predicted points from the model1
    pi[[i]] <- predict(model[[i]],data.frame(x=df1$pm1[df1$variable==lag],interval='confidence',level=0.99))
    df1$pi[df1$variable==lag] <- pi[[i]] 
    
  }
  
  # model1 <- lm(df1$value[df1$variable=="lag1"] ~ poly(df1$pm1[df1$variable=="lag1"],3))
  # 
  # #Generate predicted points from the model1
  # pi1 <- predict(model1,data.frame(x=df1$pm1[df1$variable=="lag1"],interval='confidence',level=0.99))
  # 
  # model2 <- lm(df1$value[df1$variable=="lag2"] ~ poly(df1$pm1[df1$variable=="lag2"],3))
  # 
  # #Generate predicted points from the model2
  # pi2 <- predict(model2,data.frame(x=df1$pm1[df1$variable=="lag2"],interval='confidence',level=0.99))
  # 
  # model3 <- lm(df1$value[df1$variable=="lag3"] ~ poly(df1$pm1[df1$variable=="lag3"],3))
  # 
  # #Generate predicted points from the model3
  # pi3 <- predict(model3,data.frame(x=df1$pm1[df1$variable=="lag3"],interval='confidence',level=0.99))
  
  
  # df1$pi[df1$variable=="lag1"] <- pi1 
  # df1$pi[df1$variable=="lag2"] <- pi2
  # df1$pi[df1$variable=="lag3"] <- pi3
  
  #to genrate the single plot as overlap
  plot_overlap <-   
    ggplot(df1,aes(pm1,value,color=variable))+
    geom_point(aes(pm1,value,color = variable),alpha=0.70,size=3)+
    geom_line(aes(pm1,pi,color = variable))+
    theme_bw()+xlab("Mass") + ylab("Lag")+guides(color=guide_legend(title = "Lag"))+
    theme(legend.position = "bottom")
  
  #to generate sperate lags for each sample in serate window
  plot_stratify <- 
    ggplot(df1,aes(pm1,value,color=variable))+
    geom_point(aes(pm1,value,color = variable))+
    geom_line(aes(pm1,pi,color = variable))+
    theme_bw()+xlab("Mass") + ylab("Lag")+guides(color=guide_legend(title = "Lag"),ncol=1)+
    facet_grid(Label~variable)+theme(strip.text = element_text(size=8))+
    theme(legend.position = "bottom")
  
  if(overlap==T){
    return(plot_overlap)
  }else{
    return(plot_stratify)
  }
  
}




