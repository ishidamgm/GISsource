% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GISsource.r
\name{svgXY}
\alias{svgXY}
\title{Import svg line vector}
\usage{
svgXY(f)
}
\arguments{
\item{f}{}
}
\description{
Import svg line vector
}
\examples{
 # Pools of Midagahara more drawed with GIMP
 .dir<-system.file("extdata",package="GISsource")
  filename <- paste0(.dir,"/test2.svg")
  dat<-svgXY(filename)
  rng<-dat$range
  xy<-dat$xy
  names(dat)
  str(dat)
  plot(1,type="n",xlim=rng$x,ylim=rng$y)
  for(ii in 1:length(xy))polygon(xy[[ii]],col="skyblue")
  for(ii in 1:length(lll))text(mean(xy[[ii]][,1]),mean(xy[[ii]][,2]),ii,cex=0.7,col="red")
}
