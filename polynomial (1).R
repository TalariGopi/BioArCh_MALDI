
df <- ts
xx <- myplot(ts,overlap = T)

png("plot.png",res = 600,width = 12,units = "in",height = 9)
print(xx)
dev.off()

myplot <-  function(data_frame,level=0.99,overlap=T){
  
  df1 <- reshape2::melt(ts,id = "pm1")
  
  model1 <- lm(df1$value[df1$variable=="lag1"] ~ poly(df1$pm1[df1$variable=="lag1"],3))
  
  #Generate predicted points from the model1
  pi1 <- predict(model1,data.frame(x=df1$pm1[df1$variable=="lag1"],interval='confidence',level=0.99))
  
  model2 <- lm(df1$value[df1$variable=="lag2"] ~ poly(df1$pm1[df1$variable=="lag2"],3))
  
  #Generate predicted points from the model2
  pi2 <- predict(model2,data.frame(x=df1$pm1[df1$variable=="lag2"],interval='confidence',level=0.99))
  
  model3 <- lm(df1$value[df1$variable=="lag3"] ~ poly(df1$pm1[df1$variable=="lag3"],3))
  
  #Generate predicted points from the model3
  pi3 <- predict(model3,data.frame(x=df1$pm1[df1$variable=="lag3"],interval='confidence',level=0.99))
  
  df1$pi[df1$variable=="lag1"] <- pi1 
  df1$pi[df1$variable=="lag2"] <- pi2
  df1$pi[df1$variable=="lag3"] <- pi3
  
  plot_overlap <-   
    ggplot(df1,aes(pm1,value,color=variable))+
    geom_point(aes(pm1,value,color = variable),alpha=0.70,size=3)+
    geom_line(aes(pm1,pi,color = variable))+
    theme_bw()+xlab("Mass") + ylab("Lag")+guides(color=guide_legend(title = "Lag"))+
    theme(legend.position = "bottom")

  plot_stratify <- 
    ggplot(df1,aes(pm1,value,color=variable))+
    geom_point(aes(pm1,value,color = variable))+
    geom_line(aes(pm1,pi,color = variable))+
    theme_bw()+xlab("Mass") + ylab("Lag")+guides(color=guide_legend(title = "Lag"))+
    facet_grid(~variable)+
    theme(legend.position = "bottom")
  
  if(overlap==T){
    return(plot_overlap)
  }else{
      return(plot_stratify)
    }
  
}


#create a polynomial
###################

#set the random number seed for predictable results
set.seed(20)

message("Making model")

names(ts)[1] <- c("pm1")
#Sort the data by the order in pm1
ts <- ts[ order(ts$pm1), ]

#Set the polynomial level:
plevel <- 3

#Catch situations where there isn't enough data
if(nrow(ts)<(plevel + 2) ){
	message(sprintf("ts has %d points - less than polynomial degree!",nrow(ts)))
	}else{
	#Create the model from the data - x-axis is ts$pm1, y-axis is ts$lag
	model1 <- lm(df1$value[df1$variable=="lag1"] ~ poly(df1$pm1[df1$variable=="lag1"],3))
	
	#Generate predicted points from the model
	pi1 <- predict(model,data.frame(x=df1$pm1[df1$variable=="lag1"],interval='confidence',level=0.99))
	
	model2 <- lm(df1$value[df1$variable=="lag2"] ~ poly(df1$pm1[df1$variable=="lag2"],3))
	
	#Generate predicted points from the model
	pi2 <- predict(model,data.frame(x=df1$pm1[df1$variable=="lag2"],interval='confidence',level=0.99))
	
	model3 <- lm(df1$value[df1$variable=="lag3"] ~ poly(df1$pm1[df1$variable=="lag3"],3))
	
	#Generate predicted points from the model
	pi3 <- predict(model,data.frame(x=df1$pm1[df1$variable=="lag3"],interval='confidence',level=0.99))
	
	#plot the data points:
	plot(x=ts$pm1, y=ts$lag1)
		
	#plot the best fit line:	
	lines(ts$pm1,pi,col='orange')
	
	df1$pi[df1$variable=="lag1"] <- pi1 
	df1$pi[df1$variable=="lag2"] <- pi2
	df1$pi[df1$variable=="lag3"] <- pi3
	
	ggplot(df1,aes(pm1,value,color=variable))+
	  geom_point(aes(pm1,value,color = variable))+
	  geom_line(aes(pm1,pi,color = variable))+
	  theme_bw()+xlab("Mass") + ylab("Lag")
	
	ggplot(df1,aes(pm1,value,color=variable))+
	  geom_point(aes(pm1,value,color = variable))+
	  geom_line(aes(pm1,pi,color = variable))+
	  theme_bw()+xlab("Mass") + ylab("Lag")+
	  facet_grid(~variable)
	}
	
