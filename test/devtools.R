# devtools.R
library(devtools)
library(GISsource)
help(package="GISsource")
data(package="GISsource")

# copy.files ####
#'  .dir<-system.file("extdata",package="GISsource")
#'   filename <- paste0(.dir,"/test2.svg")

dir0<-""
dir1<-"/home/i/R/x86_64-pc-linux-gnu-library/4.3/GISsource/extdata/"
dir2<-"/home/i/8T/Dropbox/00D/R/Source/GIS-source/GISsource/inst/extdata/"

cp<-function(fn,dir0,dir1,dir2){
  # dir0 <- "" ; fn <-""
  file.copy(paste0(dir0,fn),paste0(dir1,fn))
  file.copy(paste0(dir0,fn),paste0(dir2,fn))
}


