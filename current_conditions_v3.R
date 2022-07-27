###############
# R scripts to create current conditions graphic and vertical profile
# Eric M. Leibensperger (eleib003@plattsburgh.edu)
# Original code - June 2016
# Updated to verson 2 - June 2019
current_conditions <- function(valcour,val.Depths,valStopped){
  library(ggplot2)
  library(reshape2)

  # Create graphic of current conditions
  # Set up figure space
  png('current_conditions.png',width=1200,height=486)  
  val.nT<-length(valcour$date)

  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1),ann=FALSE,bty='n',type='n',xaxt='n',yaxt='n')
  col1<-0.025; col2<-0.25; col3<-0.45; col4<- 0.65; col5<-0.85;indent<-0.15
  col2.5<-0.35; col4.5<-0.70
  row0<-0.98; row1<-0.8; row2<-0.71; row3<-0.62; row4<-0.53; row5<-0.44; row6<-0.35; row7<-0.26; row8<-0.17; row9<-0.08
  row0.5<-0.03; row0.75 <- 0.89
  cex<-2.0; cex1<-2.3; cex2=1.8
  
  # Place date/time
  now<-as.character(valcour$date[val.nT])
  NOW.PRINT<-as.character(valcour$date[val.nT])
  NOW.POSIX <- Sys.time()
  NOW<-as.character(NOW.POSIX)
  text(x=col1,y=row0.5,adj=0,bquote('Last uploaded/analyzed:' ~ .(NOW) ),cex=1.25)
  textColor='black'
  if(as.numeric(NOW.POSIX)-as.numeric(valcour$date[val.nT]) > 3600) {textColor='red'}
  text(x=col3,y=row0.5,adj=0,bquote(~ .(now) ),cex=1.25,col=textColor)
  textColor='black'
  text(x=col1,y=row0,adj=0,bquote(bold("Current conditions as of: " ~ .(NOW.PRINT) )),cex=cex1)
  text(x=col2.5,y=row0.75,adj=0,bquote(bold("Valcour")),cex=cex)
  # Place surface temp.
  if(!valStopped){
    val.airT<-round(valcour$airTemp[val.nT],1) #extract latest
    val.airTF<-round(val.airT*1.8 + 32,1) #temp. in F
  } else {
    val.airT<-NA
    val.airTF<-NA
  }
  text(x=col1,y=row1,adj=0,bquote("Surface Air Temp.: "),cex=cex)
  text(x=col2,y=row1,adj=0,bquote(~ .(val.airT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row1,adj=0,bquote(~ .(val.airTF) ~degree~"F"),cex=cex)

  # Place wind speed
  val.wind<-valcour$windSpd[val.nT] #extract latest
  val.wdir <-valcour$windDir[val.nT] # extract latest direction
  count<-0
  if(!is.na(val.wind)){
  while(val.wind < 0){
    val.wind<-valcour$windSpd[val.nT-(count+1)]
    val.wdir<-valcour$windDir[val.nT-(count+1)]
    count<-count+1
    if(count==5) {val.wind<--100000}
  }
  if(val.wind<0){val.wind<-NA; val.wdir<-NA}
  } else{val.wind<-NA; val.wdir<-NA}
  if(is.na(val.wind)){val.windK<-NA} else{val.windK<-round(val.wind*1.94,1)} # wind in knots
  
  # Figure out where the wind is coming from
  if(!is.na(val.wdir)){
  if (val.wdir > 202.5 & val.wdir <= 247.5) val.direct='Southwest'
  if (val.wdir > 247.5 & val.wdir <= 292.5) val.direct='West'
  if (val.wdir >292.5 & val.wdir <= 337.5) val.direct='Northwest'
  if (val.wdir >337.5 || val.wdir <= 22.5) val.direct='North'
  if (val.wdir >22.5 & val.wdir <= 67.5) val.direct='Northeast'
  if (val.wdir >67.5 & val.wdir <= 112.5) val.direct='East'
  if (val.wdir >112.5 & val.wdir <=157.5) val.direct='Southeast'
  if (val.wdir >157.5 & val.wdir <= 202.5) val.direct='South'
  } else{val.direct<-NA}
  text(x=col1,y=row2,adj=0,bquote("Wind Speed: "),cex=cex)
  text(x=col2,y=row2,adj=0,bquote(~ .(val.wind) ~ "m/s"),cex=cex)
  text(x=col3,y=row2,adj=0,bquote(~ .(val.windK) ~"knots"),cex=cex)
  text(x=col2,y=row3,adj=0,bquote("from the " ~ .(val.direct) ~(~.(val.wdir)~degree)),cex=cex2)

  # Water temps.
  #1m
  if(!valStopped){
    val.h2oT<-round(valcour$T1m[val.nT],2)
    val.h2oTF<-round(val.h2oT*1.8+32,2)
  } else{
    val.h2oT<-NA
    val.h2oTF<-NA
  }
  text(x=col1,y=row4,adj=0,bquote("1m Water Temp.: "),cex=cex)
  text(x=col2,y=row4,adj=0,bquote( ~ .(val.h2oT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row4,adj=0,bquote(~ .(val.h2oTF) ~ degree ~"F"),cex=cex)
  #5m
  if(!valStopped){
    val.h2oT<-round(valcour$T5m[val.nT],2)
    val.h2oTF<-round(val.h2oT*1.8+32,2)
  } else{
    val.h2oT<-NA
    val.h2oTF<-NA
  }  
  text(x=col1,y=row5,adj=0,bquote("5m Water Temp.: "),cex=cex)
  text(x=col2,y=row5,adj=0,bquote( ~ .(val.h2oT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row5,adj=0,bquote(~ .(val.h2oTF) ~ degree ~"F"),cex=cex)
  #10m
  if(!valStopped){
    val.h2oT<-round(valcour$T10m[val.nT],2)
    val.h2oTF<-round(val.h2oT*1.8+32,2)
  } else{
    val.h2oT<-NA
    val.h2oTF<-NA
  }
  text(x=col1,y=row6,adj=0,bquote("10m Water Temp.: "),cex=cex)
  text(x=col2,y=row6,adj=0,bquote( ~ .(val.h2oT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row6,adj=0,bquote(~ .(val.h2oTF) ~ degree ~"F"),cex=cex)
  #14m
  if(!valStopped){
    val.h2oT<-round(valcour$T14m[val.nT],2)
    val.h2oTF<-round(val.h2oT*1.8+32,2)
  } else{
    val.h2oT<-NA
    val.h2oTF<-NA
  }
  text(x=col1,y=row7,adj=0,bquote("14m Water Temp.: "),cex=cex)
  text(x=col2,y=row7,adj=0,bquote( ~ .(val.h2oT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row7,adj=0,bquote(~ .(val.h2oTF) ~ degree ~"F"),cex=cex)
  #20m
  if(!valStopped){
    val.h2oT<-round(valcour$T20m[val.nT],2)
    val.h2oTF<-round(val.h2oT*1.8+32,2)
  } else{
    val.h2oT<-NA
    val.h2oTF<-NA
  }
  text(x=col1,y=row8,adj=0,bquote("20m Water Temp.: "),cex=cex)
  text(x=col2,y=row8,adj=0,bquote( ~ .(val.h2oT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row8,adj=0,bquote(~ .(val.h2oTF) ~ degree ~"F"),cex=cex)
  #49m
  if(!valStopped){
    val.h2oT<-round(valcour$T49m[val.nT],2)
    val.h2oTF<-round(val.h2oT*1.8+32,2)
  } else{
    val.h2oT<-NA
    val.h2oTF<-NA
  }
  text(x=col1,y=row9,adj=0,bquote("49m Water Temp.: "),cex=cex)
  text(x=col2,y=row9,adj=0,bquote( ~ .(val.h2oT) ~ degree ~ "C"),cex=cex)
  text(x=col3,y=row9,adj=0,bquote(~ .(val.h2oTF) ~ degree ~"F"),cex=cex)

  dev.off()

  ### Make current profile
  maxValDate<-valcour$date[val.nT]
  T1here<-which(colnames(valcour)=='T1m')
  T49here<-which(colnames(valcour)=='T49m')
  val.temps<-as.numeric(valcour[val.nT,T1here:T49here])
  if(NOW.POSIX - as.numeric(maxValDate) > 86400 | valStopped) {val.temps<-rep(NA,T49here-T1here+1)}

  df<-data.frame(depths=val.Depths,
                 valcour=val.temps)

  xMin<-min(df$valcour,na.rm=T)
  xMax<-max(df$valcour,na.rm=T)
  xBreaks<-pretty(df$valcour)
  numTick<-length(xBreaks)
  xTicks<-seq(xBreaks[1],xBreaks[numTick])
  if(length(xTicks==1)){xTicks<-seq(xBreaks[1],xBreaks[numTick],by=((xBreaks[numTick]-xBreaks[1])/4))}
  xLabels<-xTicks
  xLabels<-as.character(xLabels)
  xLabels[!(xTicks %in% xBreaks)]<-''
  
  xBreaksF<-pretty(xBreaks*1.8+32)
  numTickF<-length(xBreaksF)
  xTicksF<-seq(xBreaksF[1],xBreaksF[numTickF],by=5)
  xLabelsF<-xTicksF
  xLabelsF<-as.character(xLabelsF)
  xLabelsF[!(xTicksF %in% xBreaksF)]<-''
  yTicksFt<-seq(0,165,by=5)
  yLabelsFt<-as.character(yTicksFt)
  yLabelsFt[(yTicksFt %% 20) != 0]<-''
  yTicksM<-seq(0,50,by=1)
  yLabelsM<-as.character(yTicksM)
  yLabelsM[(yTicksM %% 5) != 0]<-''
  ggplot(data=df) + 
    geom_point(size=0.75,aes(y=depths,x=valcour),color='blue')+
    geom_path(size=0.25,aes(y=depths,x=valcour),color='blue')+
    xlab('Temperature (C)')+
    ylab('Depth (m)')+theme_bw()+
    ggtitle(paste('Temperature Profile \n',maxValDate,sep=''))+
    scale_x_continuous(breaks=xTicks,labels = xLabels,limits =c(min(xTicks),max(xTicks)),
                       sec.axis=sec_axis(~.*1.8+32,name='Temperature (F)',
                                         breaks = xTicksF,labels = xLabelsF))+
    scale_y_reverse(limit=c(50,0),breaks=yTicksM,labels=yLabelsM,
                    sec.axis = sec_axis(~.*3.28,name='Depth (ft.)',
                                        breaks=yTicksFt,labels=yLabelsFt))+
    #theme(legend.key=element_rect(fil="white"))+
    theme(panel.grid.major = element_blank(),#element_line(colour = "grey",size=0),
          panel.grid.minor = element_blank(),#element_line(colour = "grey",size=0), 
          panel.border=element_rect(color="black",fill=NA),
          panel.background = element_rect(fill = "white"),
          axis.text=element_text(size=6),plot.title=element_text(size=6),
          axis.title=element_text(size=6,face="bold"),
          legend.text = element_text(size=6),
          legend.background = element_blank())
  ggsave(filename = 'temp_profile.png',width=4,height=3.5)
}