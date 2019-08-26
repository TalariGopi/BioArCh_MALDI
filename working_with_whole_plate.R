# x <- list.files("C:/Users/GopaiahTalari/Desktop/25032018/drive-download-20180511T164510Z-001/CSV_20141028_SF_BZ06-16_HH15-36")
# y <- as.data.frame(x) 
# y<- "20180420_CSQ02_A1.txt"
# z <- rev(gregexpr("_", y)[[1]])[1]
# z
# substr(y,1,(z+1))
# y$name <- substr(y$x,1,(z))
# sample_spots<- unique(y$name)
library(openxlsx)


file_path<- "C:/Users/GopaiahTalari/Desktop/25032018/drive-download-20180511T164510Z-001/CSV_20141028_SF_BZ06-16_HH15-36/"
file_path2 <- "C:/Users/GopaiahTalari/Desktop/25032018/drive-download-20180511T164510Z-001/CSV_20141126_SF_BZ17-45_BZ89_IMI01-12rep/"
file_path3 <- "C:/Users/GopaiahTalari/Desktop/25032018/drive-download-20180511T164510Z-001/CSV_20141127_SF_BZ46-77_RESPOT/"
file_path4 <- "C:/Users/GopaiahTalari/Desktop/25032018/drive-download-20180511T164510Z-001/CSV_20141128_SF_BZ82-88_BBZ01-09_UMLB01-12/"

x <- list.files(file_path2,pattern = ".txt")


mynames <- do.call(rbind,strsplit(x,"\\_|\\."))

mynames2 <- data.frame(filename=x,sampleid=mynames[,ncol(mynames)-1])


mymap <- read.xlsx("PLATEMAP.xlsx")
rep_num <- do.call(rbind,strsplit(mymap$rep_num,"\\."))
mymap$num <- rep_num[,2]

myrep <- t(apply(mynames2,1,function(x){
  
  xx <- mymap$num[which((x[2]==mymap$Sample))]
  xx <- ifelse(is.null(xx),NA,xx)
  
  yy <- mymap$rep_num[which((x[2]==mymap$Sample))]
  yy <- ifelse(is.null(yy),NA,yy)
  
  c(xx,yy)
  
}))

mynames2 <- cbind(mynames2,myrep)
names(mynames2)[c(3,4)] <- c("Replicate","Sort_ID")

mynames2 <- mynames2[order(mynames2$Sort_ID,mynames2$Replicate),]
mynames2 <- mynames2[!is.na(mynames2$Sort_ID),]
mynames2$Sort_ID <- as.character(mynames2$Sort_ID)

mygroup <-  do.call(rbind,strsplit(mynames2$Sort_ID,"\\."))
mynames2 <- cbind(mynames2,mygroup=mygroup[,1])

replicates <- list()
l <- 1
mynames2$sampleid <- as.character(mynames2$sampleid)
mynames2$mygroup <- as.character(mynames2$mygroup)


for(i in unique(mynames2$mygroup)){
  replicates[[l]] <- mynames2$sampleid[mynames2$mygroup==i]
  l <- l+1
}
mynames2$filename <- as.character(mynames2$filename)
xx <- unlist(strsplit(mynames2$filename[1],"\\_|\\."))
samplelist <- paste0(paste(xx[1:(length(xx)-2)],collapse = "_"),"_")

#####-------------


sd <- ifelse(grepl("BZ",x), paste(x), NA) 
sd1 <- sd[!is.na(sd)]

y <- as.data.frame(sd1)

#y$name <- ifelse(rev(gregexpr("_", i)[[1]])[1],)
df2 <- data.frame()
for (i in sd1) {

z1 <- rev(gregexpr("_", i)[[1]])[1]
name<- substr(i, 1, (z1))
num <-  substr(i, z1+1, (z1+3))
df3 <- data.frame(name,num)
df2 <- rbind(df2,df3)
}

sample_list <- df2$name 


library(openxlsx)
path <- paste(file_path2,"PLATEMAP.xlsx", sep = "")
df <- read.xlsx(path,skipEmptyRows = T, colNames = FALSE)
df1<- df[grepl("BZ", df$X5),]


numb<- c()
a <- 1
for (i in 1:(length(df1$X5)/3)) {
  numb <-append(numb,a)
  a<- a+3
}
# aa <- c()

samples1_list<-list()
#ii <- 1
replicates<- list()
for(i in numb){
 # print(i)
  p <- df1$X4[i:(i+2)]
  print(p)
  replicates[[i]]<- p

non_null_names <- which(!sapply(replicates, is.null))
replicates <- replicates[non_null_names]
}

    for(ii in 1:length(replicates)){
    #message(sprintf("Opening %s%s%s.txt",file_path,elk_lots[ii],replicates[1]))
    froot <- sprintf("%s%s",file_path2,sample_list[ii])
    samples1_list[[ii]] <- load.sample(froot=froot,name=sprintf("%s",sample_list[ii]),replicates[[ii]], fext = ".txt")

}
#----------------------------------------

library(ggplot2)
#source("C:/Users/GopaiahTalari/Desktop/25032018/mass_lag.R")
#source("C:/Users/GopaiahTalari/Desktop/25032018/my_lag_plot.R")

pdf(file="CSV_20141126_SF_BZ17-45_BZ89_IMI01-12rep.pdf",w=12,h=8)

for(i in 1: length(samples1_list)){
  sample_list <- samples1_list[[i]]
  ts <- data.frame()
  ts <- mass_lag(fasta_mam,samples1_list[[i]])
  names(ts)[1] <- c("pm1")
  #Sort the masss data by the order in pm1
  ts <- ts[ order(ts$pm1), ]
  ts <- ts[abs(ts$lag1) < 0.5,]
  ts <- ts[abs(ts$lag2) < 0.5,]
  ts <- ts[abs(ts$lag3) < 0.5,]
  nm <-paste(samples1_list[[i]][["name"]],samples1_list[[i]][["spot"]])
  masss_lagg <- myplot(ts,nm,overlap = F)
  print(masss_lagg)
}
dev.off()











