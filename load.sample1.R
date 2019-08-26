load.sample1 <- function(froot,name="Sample",spots,fext=".txt"){
  
  # TODO: These are example values:
  # froot  <- "~/tmp/bioarch_keri/20160909_Keri13/20160909_Keri13_0_"
  # spots  <- c("G7","G10","G13")
  # sample <- "C1"
  
  #check that froot exists
  if(is.na(froot)){
    message("Froot is not defined")
    return(NA)
  }
  
  
  #TODO: we need error checking on this!
  s1 <- read.table(sprintf("%s%s%s",froot,spots[1],fext))
  s2 <- read.table(sprintf("%s%s%s",froot,spots[2],fext))
  s3 <- read.table(sprintf("%s%s%s",froot,spots[3],fext))
  s4 <- read.table(sprintf("%s%s%s",froot,spots[4],fext))
  s5 <- read.table(sprintf("%s%s%s",froot,spots[5],fext))
  s6 <- read.table(sprintf("%s%s%s",froot,spots[6],fext))
  s7 <- read.table(sprintf("%s%s%s",froot,spots[7],fext))
  s8 <- read.table(sprintf("%s%s%s",froot,spots[8],fext))

  
  colnames(s1) <- c("mass","intensity")
  colnames(s2) <- c("mass","intensity")
  colnames(s3) <- c("mass","intensity")
  colnames(s4) <- c("mass","intensity") 
  colnames(s5) <- c("mass","intensity")
  colnames(s6) <- c("mass","intensity")
  colnames(s7) <- c("mass","intensity")
  colnames(s8) <- c("mass","intensity")
 
  
  sampleobject <- list("name" = name, "spot" = spots, "s1" = s1, "s2" = s2, "s3" = s3, "s4"=s4, "s5"=s5, "s6"=s6, "s7"= s7, "s8"=s8)
  
  return(sampleobject)
  
}
