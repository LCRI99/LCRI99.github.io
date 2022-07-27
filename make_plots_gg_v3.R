###############
# R scripts to Create data buoy graphics
# Eric M. Leibensperger (eleib003@plattsburgh.edu)
# Original code - June 2016
# Switching to ggplot - May 2017
# Updated - June 2019
# Updated - July 2022

make_plots_gg<-function(valcour,val.Depths){
  # Load ggplot
  library(ggplot2)
  library(reshape2)
  library(RColorBrewer)

  # Standardize graphic size
  width=700;height=350
  
  # Number of timestamps
  val.nT<-length(valcour$date)
  minValid<--1000
  
  # Plot line thickness
  thick<-1
  
  # Filled contour map
  # Create single T array. This is an inelegant method, but it works:
  val.nZ <-length(val.Depths)
  #Temps <- data[,12:50]# Dependent on formatting!!!
  # Old less elegant, but more generalizable method:
  val.T    <- array(dim=c(length(valcour$date),val.nZ))
  for (i in 1:val.nZ)
  {
    nam <- paste("T",val.Depths[i],"m",sep="")
    eval(parse(text=paste("temporary<-",paste("valcour$",nam,sep=""))))
    val.T[,i]<-temporary
  }
  # Set to save plot
  png('all_Temp_valcour.png',width=width,height=height)
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  
  val.T[valcour$date<minDate,]<-NA
  xlim<-c(minDate,maxDate)
  filled.contour(valcour$date,val.Depths,val.T,ylim=c(50,0),
                 plot.axes={axis.POSIXct(1,at=seq(minDate,maxDate,by="day"),format="%D") 
                   axis(2,at=seq(0,50,by=10))},
                 color=function(x)rev(rainbow(x,end=0.7)),zlim=c(2,26),
                 xlim=xlim,xlab='Date',ylab='Depth  (m)',main='Valcour Temp. (C)')
  dev.off()

  
  # Last seven days
  png('last7_Temp_valcour.png',width=width,height=height)
  minDate<-maxDate - 7*86400 # seconds
  xlim<-c(minDate,maxDate)
  filled.contour(valcour$date,val.Depths,val.T,ylim=c(50,0),
                 plot.axes={axis.POSIXct(1,at=seq(minDate,maxDate,by="day"),format="%D") 
                   axis(2,at=seq(0,50,by=10))},
                 color=function(x)rev(rainbow(x,end=0.7)),zlim=c(2,26),
                 xlim=xlim,xlab='Date',ylab='Depth  (m)',main='Valcour Temp. (C)')
  dev.off()
  
  # Resolution for line graphs
  width=900; height=300
  
  # Air Temp.
  valcour$airTemp[valcour$airTemp < minValid]<-NA
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  minVal<-min(valcour$airTemp[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<-max(valcour$airTemp[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)

  ggplot(data=valcour) + geom_line(size=thick,aes(x=date,y=airTemp),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Air Temp. (F)'))+
    ylab('Air Temp. (C)')+xlab('Date')+
    ggtitle('Lake Champlain Air Temperature')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'all_airTemp.png',width=9,height=3)

  minDate<-maxDate-7*86400
  minValcour<- min(valcour$airTemp[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxValcour<- max(valcour$airTemp[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)

  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=airTemp),color='blue')+
    xlim(minDate,maxDate)+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Air Temp. (F)'))+
    ylab('Air Temp. (C)')+xlab('Date')+
    ggtitle('Lake Champlain Air Temperature')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'last7_airTemp.png',width=9,height=3)
  # Wind speed
  maxDate<-max(valcour$date)
  minDate<-maxDate-7*86400
  minVal<-0
  maxVal<- max(valcour$windSpd[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)

  if(maxVal>0){
  ggplot(data=valcour) + geom_path(size=0.75,aes(x=date,y=windSpd),color='blue')+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Wind Speed (m/s)')+
    ggtitle('Lake Champlain Wind Speed')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.94,name='Wind Speed (knots)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))  
  ggsave(filename = 'last7_wind.png',width=9,height=3)
  }
  minDate<-min(valcour$date)
  minVal<-0
  maxVal<-max(valcour$windSpd,na.rm=T)
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=windSpd),color='blue')+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Wind Speed (m/s)')+
    ggtitle('Lake Champlain Wind Speed')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.94,name='Wind Speed (knots)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))  
  ggsave(filename = 'all_wind.png',width=9,height=3)
  
  # Air Pressure
  minDate<-maxDate-7*86400
  minVal<- min(valcour$pres[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<- max(valcour$pres[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=pres),color='blue')+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Pressure (in. Hg)')+
    ggtitle('Lake Champlain Air Pressure')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*33.8639,name='Pressure (hPa)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))    
  ggsave(filename = 'last7_pres.png',width=9,height=3)
  
  minDate<-min(valcour$date)
  minVal<-min(valcour$pres,na.rm=T)
  maxVal<-max(valcour$pres,na.rm=T)
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=pres),color='blue')+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Pressure (in. Hg)')+
    ggtitle('Lake Champlain Air Pressure')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*33.8639,name='Pressure (hPa)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))    
  ggsave(filename = 'all_pres.png',width=9,height=3)

  #T1m
  valcour$T1m[valcour$T1m.val < minValid]<-NA
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  minVal<-min(valcour$T1m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<-max(valcour$T1m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)

  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T1m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+
    ggtitle('Lake Champlain Temp. @ 1m (3.3 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'all_T1m.png',width=9,height=3)
  
  minDate<-maxDate-7*86400
  minVal<- min(valcour$T1m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<- max(valcour$T1m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)

  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T1m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+xlim(minDate,maxDate)+
    ggtitle('Lake Champlain Temp. @ 1m (3.3 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'last7_T1m.png',width=9,height=3)
  
  # 5m T
  valcour$T5m[valcour$T5m.val < minValid]<-NA
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  minVal<-min(valcour$T5m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<-max(valcour$T5m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T5m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+
    ggtitle('Lake Champlain Temp. @ 5m (16.5 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'all_T5m.png',width=9,height=3)
  
  minDate<-maxDate-7*86400
  minVal<- min(valcour$T5m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<- max(valcour$T5m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T5m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+xlim(minDate,maxDate)+
    ggtitle('Lake Champlain Temp. @ 5m (16.5 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'last7_T5m.png',width=9,height=3)
  
  # 10m T
  valcour$T10m[valcour$T10m.val < minValid]<-NA
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  minVal<-min(valcour$T10m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<-max(valcour$T10m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T10m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+
    ggtitle('Lake Champlain Temp. @ 10m (33 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'all_T10m.png',width=9,height=3)
  
  minDate<-maxDate-7*86400
  minVal<- min(valcour$T10m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<- max(valcour$T10m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T10m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+xlim(minDate,maxDate)+
    ggtitle('Lake Champlain Temp. @ 10m (33 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'last7_T10m.png',width=9,height=3)
  
  # 14m T
  valcour$T14m[valcour$T14m.val < minValid]<-NA
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  minVal<-min(valcour$T14m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<-max(valcour$T14m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T14m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+
    ggtitle('Lake Champlain Temp. @ 14m (46 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'all_T14m.png',width=9,height=3)
  
  minDate<-maxDate-7*86400
  minVal<- min(valcour$T14m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  maxVal<- max(valcour$T14m[valcour$date >= minDate & valcour$date <= maxDate],na.rm=T)
  
  ggplot(data=valcour) + geom_path(size=thick,aes(x=date,y=T14m),color='blue')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    ylab('Water Temp. (C)')+xlab('Date')+xlim(minDate,maxDate)+
    ggtitle('Lake Champlain Temp. @ 14m (46 ft.)')+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))
  ggsave(filename = 'last7_T14m.png',width=9,height=3)
  
  # 25m T
  valcour$T25m[valcour$T25m < minValid]<-NA
  
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  df<-data.frame(time=valcour$date[valcour$date>=minDate & valcour$date<=maxDate],T25m=valcour$T25m[valcour$date>=minDate & valcour$date<=maxDate])
  minVal<-min(df$T25m,na.rm=T)
  maxVal<-max(df$T25m,na.rm=T)
  
  ggplot(df)+
    geom_path(aes(y=T25m,x=time),color='blue',size=thick)+
    theme(legend.key=element_rect(fil="white"))+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Water Temp. (C)')+#ylim(minVal,maxVal)+
    ggtitle('Lake Champlain Temp. @ 25m (82.5 ft.)')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))   
  
  ggsave(filename = 'all_T25m.png',width=9,height=3)
  
  minDate<-maxDate-7*86400
  df<-data.frame(time=valcour$date[valcour$date>=minDate & valcour$date<=maxDate],T25m=valcour$T25m[valcour$date>=minDate & valcour$date<=maxDate])
  minVal<-min(df$T25m,na.rm=T)
  maxVal<-max(df$T25m,na.rm=T)
  
  ggplot(df)+
    geom_path(aes(y=T25m,x=time),color='blue',size=thick)+
    theme(legend.key=element_rect(fil="white"))+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Water Temp. (C)')+#ylim(minVal,maxVal)+
    ggtitle('Lake Champlain Temp. @ 25m (82.5 ft.)')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))   
  
  ggsave(filename = 'last7_T25m.png',width=9,height=3)
  
  # 49m T
  valcour$T49m[valcour$T49m < minValid]<-NA
  
  minDate<-min(valcour$date)
  maxDate<-max(valcour$date)
  df<-data.frame(time=valcour$date[valcour$date>=minDate & valcour$date<=maxDate],T49m=valcour$T49m[valcour$date>=minDate & valcour$date<=maxDate])
  minVal<-min(df$T49m,na.rm=T)
  maxVal<-max(df$T49m,na.rm=T)
  
  ggplot(df)+
    geom_path(aes(y=T49m,x=time),color='blue',size=thick)+
    theme(legend.key=element_rect(fil="white"))+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Water Temp. (C)')+#ylim(minVal,maxVal)+
    ggtitle('Lake Champlain Temp. @ 49m (162 ft.)')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))   
  
  ggsave(filename = 'all_T49m.png',width=9,height=3)
  
  minDate<-maxDate-7*86400
  df<-data.frame(time=valcour$date[valcour$date>=minDate & valcour$date<=maxDate],T49m=valcour$T49m[valcour$date>=minDate & valcour$date<=maxDate])
  minVal<-min(df$T49m,na.rm=T)
  maxVal<-max(df$T49m,na.rm=T)
  
  ggplot(df)+
    geom_path(aes(y=T49m,x=time),color='blue',size=thick)+
    theme(legend.key=element_rect(fil="white"))+
    xlab('Time')+xlim(minDate,maxDate)+
    ylab('Water Temp. (C)')+#ylim(minVal,maxVal)+
    ggtitle('Lake Champlain Temp. @ 49m (162 ft.)')+
    scale_y_continuous(limit=c(minVal,maxVal),sec.axis = sec_axis(~.*1.8+32,name='Water Temp. (F)'))+
    theme(panel.grid.major = element_line(colour = "grey",size=0.5),
          panel.grid.minor = element_line(colour = "grey",size=0.15), panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=16),plot.title=element_text(size=18),
          axis.title=element_text(size=14,face="bold"))   
  
  ggsave(filename = 'last7_T49m.png',width=9,height=3)
  
}  



