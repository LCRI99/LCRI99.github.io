###############
# R scripts to read/write SUNY Plattsburgh data buoy data
# Eric M. Leibensperger (eleib003@plattsburgh.edu)
# July 26, 2022
# Original version: June 2016, updated May 26, 2017, June 13, 2019
library(RCurl)
read_Valcour<-function(firstGood){
  file1 <- "master.valcour.csv"
  file2 <- "latest.csv"
  
  # Read the existing master csv file; it has just one line of a header
  data1 <- read.table(file1,sep=',',header=TRUE)
 
  # Read latest file. Note that the header has four lines, so let's 
  # just hardwire the variable names.
  data2 <- read.table(file2,sep=",",header=FALSE,skip=4,
                      col.names=c('date','p.power','rtc.power','primary.power','sec.power',
                                  'sensor.power','sys.current','sen.current','int.pres','int.temp',
                                  'int.humidity','cell.strength','cell.status',
                                  "T1m","T2m","T3m","T4m","T5m","T6m","T7m",
                                  "T8m","T9m","T10m","T11m","T12m","T13m","T14m",
                                  "T15m","T16m","T17m","T18m","T19m","T20m","T21m",
                                  "T22m","T23m","T24m","T25m","T26m","T27m","T28m","T29m",
                                  "T31m","T33m","T35m","T37m","T39m","T41m","T43m","T45m","T47m","T49m",
                                  'UTC','lat','lon','alt','cog1','cog2','pres','airTemp','RH','dewpoint',
                                  'windDir','magWindDir','windSpd','windDir2','windSpd2','pitch','roll'))
                      #col.names=c("date","battery","UTC","lat","lon","pres",
                      #            "airTemp","windDir","windSpd","head","Txx",
                      #            "T1m","T2m","T3m","T4m","T5m","T6m","T7m",
                      #            "T8m","T9m","T10m","T11m","T12m","T13m","T14m",
                      #            "T15m","T16m","T17m","T18m","T19m","T20m","T21m",
                      #            "T22m","T23m","T24m","T25m","T26m","T27m","T28m","T29m",
                      #            "T31m","T33m","T35m","T37m","T39m","T41m","T43m","T45m","T47m","T49m"))
  data1$date <- as.POSIXct(data1$date,tz='America/New_York')#,format="%m-%d-%Y %H:%M:%S %p")  
  data2$date <- as.POSIXct(data2$date,format='%m-%d-%Y %H:%M:%S',tz='America/New_York')
  
  # Find out which dates are unique to data2 (the new file):
  these <- which(!(data2$date %in% data1$date))
  if(sum(these) > 0) {
    newData <- rbind(data1,data2[these,])
    newData <- newData[order(newData$date),]
    newData[newData < -90000] <- NA
    
    # Write out new data table
    write.table(newData,file=paste('master.valcour_',format(Sys.time(),"%y%m%d_%H%M"),'.csv',sep=""),row.names=FALSE,sep=',',
                col.names=TRUE)
    
    write.table(newData,file='master.valcour.csv',row.names=FALSE,sep=',',
                col.names=TRUE)
  
  } else {
    newData <- data1
  }
  
  newData <- newData[newData$date>=firstGood,]
  return(newData)
  
}

# FUNCTION TO WRITE OUT AND SEND DATA TO NATIONAL DATA BUOY CENTER
# Updated June 2019
write_NDBC<-function(valcour,Z,sendNDBC){
  # Include stringr for strpad
  library(stringr)
  library(RCurl)
  nT <- length(valcour$date)
  # Create XML for NDBC
  lastXML<-file('lastXML'); a<-readLines(con=lastXML);close(lastXML)
  num<-which(valcour$date == as.POSIXct(a))
    if(sum(num>0)){
      if(is.na(num) | num == length(valcour$date)){return(FALSE)}
    } else {num<-0}

  NOW<-Sys.time()
  attr(NOW,"tzone")<-'GMT'
  XMLfile<-'Sta45178.latest.xml'
  if(file.exists(XMLfile)){file.remove(XMLfile)}
#  XMLfile<-paste('Sta45178',format(NOW,"%Y%m%d.%H%M"),'xml',sep='.')
  fileXML<-XMLfile
  cat('<?xml version="1.0" encoding="ISO-8859-1"?>',file=fileXML,sep='\n')
  missingVal<-format(-9999,digits=4)
  Zdesire<-c(1,2,3,4,5,6,7,8,9,10,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49)
  nRec<-4
  for (n in (num+1):nT){
    Rec<-n
    cat('<message>',file=fileXML,sep='\n',append=TRUE)
    cat('   <station>45178</station>',file=fileXML,sep='\n',append=TRUE)
    timeNow<-valcour$date[Rec]
    attr(timeNow,"tzone")<-"GMT"
    d1<-format(timeNow,'%m/%d/%Y %H:%M')
    if(is.na(d1)){d1<-missingVal}
    cat(paste('   <date>',d1,'</date>',sep=''),file=fileXML,sep='\n',append=TRUE)
    cat('   <missing>-9999</missing>',file=fileXML,sep='\n',append=TRUE)
    cat('   <roundtime>no</roundtime>',file=fileXML,sep='\n',append=TRUE)
    cat('   <met>',file=fileXML,sep='\n',append=TRUE)
    cat('      <gpslat>44.60317</gpslat>',file=fileXML,sep='\n',append=TRUE)
    cat('      <gpslon>-73.393783</gpslon>',file=fileXML,sep='\n',append=TRUE)
      
    w1<-format(round(valcour$windDir[Rec],1))
    if(valcour$windDir[Rec]<0 | is.na(valcour$windDir[Rec])){w1<-missingVal}
    cat(paste('      <wdir1>',w1,'</wdir1>',sep=''),file=fileXML,sep='\n',append=TRUE)
      
    ws1<-format(round(valcour$windSpd[Rec],2),nsmall=1)
    if(valcour$windSpd[Rec]<0 | is.na(valcour$windSpd[Rec])){ws1<-missingVal}
    cat(paste('      <wspd1>',ws1,'</wspd1>',sep=''),file=fileXML,sep='\n',append=TRUE)
      
    p1<-format(round(valcour$pres[Rec]*33.86,1),nsmall=1)
    if(valcour$pres[Rec]<0 | is.na(valcour$pres[Rec])){p1<-missingVal}
    cat(paste('      <baro1>',p1,'</baro1>',sep=''),file=fileXML,sep='\n',append=TRUE)
      
    T1<-format(round(valcour$airTemp[Rec],1),nsmall=1)
    if(valcour$airTemp[Rec]<0 | is.na(valcour$airTemp[Rec])){T1<-missingVal}
    cat(paste('      <atmp1>',T1,'</atmp1>',sep=''),file=fileXML,sep='\n',append=TRUE)
      
    WT1<-format(round(valcour$T1m[Rec],2),nsmall=2)
    if(valcour$T1m[Rec]<0 | is.na(valcour$T1m[Rec])){WT1<-missingVal}
    cat(paste('      <wtmp1>',WT1,'</wtmp1>',sep=''),file=fileXML,sep='\n',append=TRUE)
    cat('      <fm64iii>820</fm64iii>',file=fileXML,sep='\n',append=TRUE)
    cat('      <fm64xx>99</fm64xx>',file=fileXML,sep='\n',append=TRUE)
    cat('      <fm64k1>7</fm64k1>',file=fileXML,sep='\n',append=TRUE)
    for (d in 1:30){
      dd<-which(Z == Zdesire[d])
      nam <- paste("T",Z[dd],"m",sep="")
      eval(parse(text=paste("there<-",paste("valcour$",nam,sep=""))))
      cd<-str_pad(d,3,pad=0)
      dpb<-paste('      <dp',cd,'>',format(Zdesire[d]),'</dp',cd,'>',sep='')
      TTT<-format(there[Rec],digits=3)           
      if(there[Rec]>40 | there[Rec]<0 | is.na(there[Rec])){TTT<-missingVal}
      tpb<-paste('      <tp',cd,'>',TTT,'</tp',cd,'>',sep='')
      cat(dpb,append=TRUE,sep='\n',file=fileXML)
      cat(tpb,append=TRUE,sep='\n',file=fileXML)
    }
    cat('   </met>',file=fileXML,sep='\n',append=TRUE)
    cat('</message>',file=fileXML,sep='\n',append=TRUE)
  }
  if(sendNDBC){
    Sys.sleep(15)
    status<-1
    try(status<-ftpUpload(fileXML,paste("ftp://suny-pbrg:s5N4nx6Dwh3f@comms.ndbc.noaa.gov/",fileXML,sep='')))
    #status<-0
    if(status==0){lastXML<-file('lastXML'); writeLines(as.character(valcour$date[nT]),con=lastXML);close(lastXML)}
  }
  lastXML<-file('lastXML'); writeLines(as.character(valcour$date[nT]),con=lastXML);close(lastXML)
}


# Write out public csv files
write_data_buoy<-function(valcour){
  # Remove records we don't want in public csv files
  valcour.sub<-subset(valcour,select=c(date,pres,airTemp,windSpd2,windDir2,T1m,
                                   T2m,T3m,T4m,T5m,T6m,T7m,T8m,T9m,T10m,T11m,T12m,
                                   T13m,T14m,T15m,T16m,T17m,T18m,T19m,T20m,T21m,T22m,
                                   T23m,T24m,T25m,T26m,T27m,T28m,T29m,T31m,T33m,T35m,
                                   T37m,T39m,T41m,T43m,T45m,T47m,T49m))
  colnames(valcour.sub)[colnames(valcour.sub)=='windSpd2']<-'windSpd'
  colnames(valcour.sub)[colnames(valcour.sub)=='windDir2']<-'windDir'
  valcour.sub$head<-NA
  valcour.sub<-valcour.sub[,c("date","pres","airTemp","windSpd","windDir","head",
                            "T1m","T2m","T3m","T4m","T5m","T6m","T7m","T8m","T9m",
                            "T10m","T11m","T12m","T13m","T14m","T15m","T16m","T17m",
                            "T18m","T19m","T20m","T21m","T22m","T23m","T24m","T25m",
                            "T26m","T27m","T28m","T29m","T31m","T33m","T35m","T37m",
                            "T39m","T41m","T43m","T45m","T47m","T49m")]
  
  # Write out all of this year's data to csv
  write.table(valcour.sub,file='all.valcour.csv',row.names=FALSE,sep=',')

  maxDate<-max(valcour$date)
  minDate<-maxDate-7*86400
  write.table(valcour[valcour$date>=minDate,],file='last7.valcour.csv',row.names=FALSE,sep=',')
}