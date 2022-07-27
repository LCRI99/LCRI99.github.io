############
# Updated R scripts to produce graphics and files from
# the SUNY Plattsburgh data buoys. 
#
# This version has SML data buoy removed. 
#
# July 26, 2022
# Original file created June 1, 2016 and Spring 2018, 2019
############

# Clean up the environment
rm(list=ls())

#SWITCHS
doNDBCxml <- FALSE
sendNDBC  <- FALSE
writeOut  <- TRUE
valStopped<- FALSE

# Set the working directory 
#setwd('C:\\Buoy\\')
setwd('/Users/eleibensperger/OneDrive - Ithaca College/forTim/')
# Source helper routines
source('read_write_buoy_v3.R')
source('current_conditions_v3.R')
source('make_plots_gg_v3.R')

val.Depths <- c(1:29,c(1:10)*2+29)

val.firstGood<-as.POSIXct("2022-06-15 12:30:00",tz='America/New_York')
valcour <- read_Valcour(val.firstGood)

if(doNDBCxml){
  dummy <- write_NDBC(valcour,val.Depths,sendNDBC)
}

current_conditions(valcour,val.Depths,valStopped)

# Make graphics
if(length(valcour$date > 0)){
  make_plots_gg(valcour,val.Depths)
#  therm.anim(valcour,val.Depths,sml,sml.Depths)
}

# Write out public csv files
if(writeOut){
  out<-write_data_buoy(valcour)
}




