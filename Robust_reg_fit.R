
# masss_lagg function is to gerate the plots for the lag vs mass
# ts as data frame contains ts$pm1 and ts$lags 1,2,3

myplot_lm <-  function(ts,nm,level=0.99,overlap=T){

  df1 <- reshape2::melt(ts, id = "pm1")
  #df1 <- reshape2::melt(ts_both, id.vars = c("pm1", "id"))
  model1 <- lm(df1$value[df1$variable=="lag1"] ~ poly(df1$pm1[df1$variable=="lag1"],3))
  #lmrob(y ~ x, data = data.frame(x, y), setting = "KS2014");
  #Generate predicted points from the model1
  pi1 <- predict(model1,data.frame(x=df1$pm1[df1$variable=="lag1"], interval='confidence',level=0.99))

  model2 <- lm(df1$value[df1$variable=="lag2"] ~ poly(df1$pm1[df1$variable=="lag2"],3))

  #Generate predicted points from the model2
  pi2 <- predict(model2,data.frame(x=df1$pm1[df1$variable=="lag2"],interval='confidence',level=0.99))
  model2 <- lm(df1$value[df1$variable=="lag2"] ~ poly(df1$pm1[df1$variable=="lag2"],3))

  #Generate predicted points from the model2
  pi2 <- predict(model2,data.frame(x=df1$pm1[df1$variable=="lag2"],interval='confidence',level=0.99))

  model3 <- lm(df1$value[df1$variable=="lag3"] ~ poly(df1$pm1[df1$variable=="lag3"],3))

  #Generate predicted points from the model3
  pi3 <- predict(model3,data.frame(x=df1$pm1[df1$variable=="lag3"],interval='confidence',level=0.99))

  df1$pi[df1$variable=="lag1"] <- pi1
  df1$pi[df1$variable=="lag2"] <- pi2
  df1$pi[df1$variable=="lag3"] <- pi3
  #
  x1 <- paste(nm,paste("R2 = ",signif(summary(model1)$r.squared, 5),
              " P =",signif(summary(model1)$coef[2,4], 5)),sep = "       ")

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
    theme_bw()+xlab("Mass") + ylab("Lag")+guides(color=guide_legend(title = "Lag"))+
    facet_grid(~variable)+
    theme(legend.position = "bottom")+
    #ggtitle(nm)+ theme(plot.title = element_text(hjust = 0.5))+

    labs(title = x1)

  # plot_overlap <- ggplotly(plot_overlap)
  # plot_stratify <- ggplotly(plot_stratify)

  if(overlap==T){
    return(plot_overlap)
  }else{
    return(plot_stratify)
  }

}

ind1 <- unname(which(y > (predict(fit1) + err1) | y < (predict(fit1) - err1)))
x <- x[-ind1]
y <- y[-ind1]
## Fitting data and calculating error for recursive set
fit2 <- lm(y~x)
err2 <- summary(fit2)$sigma
## Plotting fit and error
plot(x,y, ylim = c(-1.5*err2, 11))
abline(fit2)
lines(x, (predict(fit2) + err2), col = 'red', lty = 2)
lines(x, (predict(fit2) - err2), col = 'red', lty = 2)



