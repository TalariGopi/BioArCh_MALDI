



#create a polynomial
###################

#set the random number seed for predictable results
set.seed(20)

message("Making model")

#Sort the data by the order in pm1
ts <- ts[ order(ts$mass), ]

#Set the polynomial level:
plevel <- 3

#Catch situations where there isn't enough data
if(nrow(ts)<(plevel + 2) ){
	message(sprintf("ts has %d points - less than polynomial degree!",nrow(ts)))
	}else{
	#Create the model from the data - x-axis is ts$pm1, y-axis is ts$lag
	model <- lm(ts$lag2 ~ poly(ts$mass,3))
	
	#Generate predicted points from the model
	pi <- predict(model,data.frame(x=ts$mass,interval='confidence',level=0.99))
	
	#plot the data points:
	
	
  pl <- ggplot(ts, aes(ts$mass,ts$lag1))+
    
    #geom_line(data=data$s2,aes(mass,intensity/max(data$s1$intensity)),color="green")+
    #geom_line(data=data$s3,aes(mass,intensity/max(data$s1$intensity)),color="blue")+
    #geom_point(data = bzzt,aes(mass1,0.5*bzzt$mass1/bzzt$mass1),color="orange")+
    xlab("Mass")+ylab("lag")+
    theme_bw()
  print(pl)

	#plot the best fit line:	
	
	}
	


lines(ts$mass,pi,col='orange')












#create a polynomial
###################

#set the random number seed for predictable results
set.seed(20)

message("Making model")

#Sort the data by the order in pm1
ts <- t[ order(t$pm1), ]

#Set the polynomial level:
plevel <- 3

#Catch situations where there isn't enough data
if(nrow(ts)<(plevel + 2) ){
	message(sprintf("ts has %d points - less than polynomial degree!",nrow(ts)))
	}
else{
	#Create the model from the data - x-axis is ts$pm1, y-axis is ts$lag
	model <- lm(ts$lag ~ poly(ts$pm1,3))
	
	#Generate predicted points from the model
	pi <- predict(model,data.frame(x=ts$pm1,interval='confidence',level=0.99))
	
	#plot the data points:
	plot(x=ts$pm1, y=ts$lag)
		
	#plot the best fit line:	
	lines(ts$pm1,pi,col='orange')
	}
	
