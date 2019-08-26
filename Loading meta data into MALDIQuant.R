library(openxlsx)
library(dplyr)
library(tidyr)
  # df <- read.csv("E:/All_identifications.csv")
  # 
  # 
  # df$Sample.Number<- gsub(" ","", df$Sample.Number)
  # 
  # library("tibble")


  # x <- list.files()
  # 
  # xx<- gsub(".", "_", x, fixed = TRUE)
  # 
  # file.rename(from=x,to=xx)


#----------------
#adding .csv extensiom

oldNames<-list.files()
file.rename(oldNames,paste0(oldNames,".csv"))

#-----Converting to csv from space seprate file------------
  
setwd("D:/MALDIQuant/Complete_data_after_rename")

x <- list.files("D:\\MALDIQuant\\Complete_data_after_rename")
y <- as.data.frame(x)
  # y <- y[!(grepl("(|)", y$x)),]
  # y <- strsplit(y$x,".")
  # y1<-y %>%
  # separate(x, c("file_name", "extension","yy"), "\\.")

for (i in y$x){
  print(i)
  df <- read.csv(i, header = FALSE)
  p<-(length(names(df)))
  if (p==1){
    df1<- df %>%
    separate(V1, c("Col1", "Col2"), "\\s")
    #write.csv(df1, i, row.names = FALSE, col.names = FALSE)
  
    write.table(df1,i, sep = "," ,col.names = FALSE,row.names = FALSE ,quote = F)

  }
}

#--------------------------------------------------------

#Loading the raw data

library(googledrive)

url_link<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSd_6jg0lYTLtcYk56FZdUi2063RR9kYEGs4wOjrJciQ1HicsSxY1cjz3Z3IEBosHpxvKHci_1ipymH/pub?output=csv"

All_idenfications <-  read.csv(url(url_link))

All_idenfications$Sample.Number <- gsub(" ","", All_idenfications$Sample.Number)

All_idenfications$Sample.Number <- gsub("\\.*","", All_idenfications$Sample.Number)


write.csv(All_idenfications,"All_idenfications.csv")

#--------------------------------------------------------------

calf <- All_idenfications[All_idenfications$Species.Identification=="Calf",]
calf <- calf$Sample.Number

list1 <- list.files("D:\\MALDIQuant\\S_C_L_All_CSV")
list <- sub('\\_.*', '', list1) # remove. csv


for (i in list){
  
  if (i %int% calf){
    mainDir <- "D:\\MALDIQuant\\New_data_folder\\Calf"
    subDir <- i
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    setwd(file.path(mainDir, subDir))
    
  }else{
    print("NA")
  }
    
}

################organising into folders####################33

parent.folder <- "D:\\MALDIQuant\\Complete_data_after_rename"
files <- list.files(path = parent.folder, full.names = T, recursive = T, include.dirs = T)

list1 <- list.files("D:\\MALDIQuant\\Complete_data_after_rename")
list <- sub('\\_.*', '_', list1) # remove. csv

a <- 0 # to see work progress

#test_csv <- read.csv("D:/MALDIQuant/test.csv")
All_idenfications <- read.csv("D:/MALDIQuant/All_idenfications.csv")

for (j in unique(All_idenfications$Species.Identification)){
  setwd("D:\\MALDIQuant\\Complete_data_after_rename")
  dir.create(j, recursive = T)
  samplename <- All_idenfications[All_idenfications$Species.Identification==j,]
  samplename <- samplename$Sample.Number
  samplename <- paste(samplename,"_",sep = "")
  for (i in list){
    
    if (i %in% samplename){
      files1 <- files[grepl(paste("/",i,sep=""), files)]
      setwd(paste("D:\\MALDIQuant\\Complete_data_after_rename\\",j,sep=""))
      i <- gsub("_","",i)
      dir.create(i, recursive = T)
      
      newnames <- paste0(paste(i,"/",sep = ""), basename(files1))
      mapply(file.copy, from=files1, to=newnames)
      
    }else{
      print("NA")
    }
    print(a<-a+1)
  }

}


#################### file ren
