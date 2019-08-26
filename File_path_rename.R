##############----File_path_rename------#####################

##################### ----Calf----- ######################

setwd("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Calf")
x <- list.files()
#y <- as.data.frame()
#y<- data.frame()

for (i in x){
  z1 <- rev(gregexpr("_",i)[[1]])[2]
  z2 <- rev(gregexpr("\\.",i)[[1]])[1]
  
  
  y1 <- substr(i,z1+1,(z2+3))
  file.rename(from = i,to = y1)
}

#--------------------------------------

# Folder creation from data For Calf

################organising into folders####################33

parent.folder <- "D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Calf"

files <- list.files(path = parent.folder, full.names = T, recursive = T, include.dirs = T)

list1 <- list.files( "D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Calf")
list <- sub('\\_.*', '_', list1) # remove. csv

a <- 0 # to see work progress

#test_csv <- read.csv("D:/MALDIQuant/test.csv")
All_idenfications <- read.csv("D:/MALDIQuant/All_idenfications.csv")

for (j in unique(All_idenfications$Species.Identification)){
  setwd("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Calf")
  dir.create(j, recursive = T)
  samplename <- All_idenfications[All_idenfications$Species.Identification==j,]
  samplename <- samplename$Sample.Number
  samplename <- paste(samplename,"_",sep = "")
  for (i in list){
    
    if (i %in% samplename){
      files1 <- files[grepl(paste("/",i,sep=""), files)]
      setwd(paste("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Calf\\",j,sep=""))
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

#-------------------------------------------------------------------------
##############----File_path_rename for Goat------#####################

setwd("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Goat")
x <- list.files()
#y <- as.data.frame()
#y<- data.frame()

for (i in x){
  z1 <- rev(gregexpr("_",i)[[1]])[2]
  z2 <- rev(gregexpr("\\.",i)[[1]])[1]
  
  
  y1 <- substr(i,z1+1,(z2+3))
  file.rename(from = i,to = y1)
}

#--------------------------------------

# Folder creation from data

################organising into folders####################33

parent.folder <- "D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Goat"

files <- list.files(path = parent.folder, full.names = T, recursive = T, include.dirs = T)

list1 <- list.files( "D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Goat")
list <- sub('\\_.*', '_', list1) # remove. csv

a <- 0 # to see work progress

#test_csv <- read.csv("D:/MALDIQuant/test.csv")
All_idenfications <- read.csv("D:/MALDIQuant/All_idenfications.csv")

for (j in unique(All_idenfications$Species.Identification)){
  setwd("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Goat")
  dir.create(j, recursive = T)
  samplename <- All_idenfications[All_idenfications$Species.Identification==j,]
  samplename <- samplename$Sample.Number
  samplename <- paste(samplename,"_",sep = "")
  for (i in list){
    
    if (i %in% samplename){
      files1 <- files[grepl(paste("/",i,sep=""), files)]
      setwd(paste("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Goat\\",j,sep=""))
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

##############----File_path_rename for Sheep------#####################

setwd("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Sheep")
x <- list.files()
#y <- as.data.frame()
#y<- data.frame()

for (i in x){
  z1 <- rev(gregexpr("_",i)[[1]])[2]
  z2 <- rev(gregexpr("\\.",i)[[1]])[1]
  
  
  y1 <- substr(i,z1+1,(z2+3))
  file.rename(from = i,to = y1)
}

#--------------------------------------

###############- Organising into folders for Sheep -###################

parent.folder <- "D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Sheep"

files <- list.files(path = parent.folder, full.names = T, recursive = T, include.dirs = T)

list1 <- list.files( "D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Sheep")
list <- sub('\\_.*', '_', list1) # remove. csv

a <- 0 # to see work progress

#test_csv <- read.csv("D:/MALDIQuant/test.csv")
All_idenfications <- read.csv("D:/MALDIQuant/All_idenfications.csv")

for (j in unique(All_idenfications$Species.Identification)){
  setwd("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Sheep")
  dir.create(j, recursive = F)
  samplename <- All_idenfications[All_idenfications$Species.Identification==j,]
  samplename <- samplename$Sample.Number
  samplename <- paste(samplename,"_",sep = "")
  for (i in list){
    
    if (i %in% samplename){
      files1 <- files[grepl(paste("/",i,sep=""), files)]
      setwd(paste("D:\\MALDIQuant\\Mass-up\\Mass_UP_Input\\Sheep\\",j,sep=""))
      i <- gsub("_","",i)
      dir.create(i, recursive = F, showWarnings = T)
      
      newnames <- paste0(paste(i,"/",sep = ""), basename(files1))
      mapply(file.copy, from=files1, to=newnames)
      
    }else{
      print("NA")
    }
    print(a<-a+1)
  }
  
}

