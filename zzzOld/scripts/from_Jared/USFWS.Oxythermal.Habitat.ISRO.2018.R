#CISCO OXYTHERMAL HABITAT
library(gganimate)
library(ggplot2)
library(doBy)
library(magick)
library(ggmap)
#RICHIE
Richie.01<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Richie/7450-338212.txt",header=TRUE,na.strings="NA")
Richie.03<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Richie/7450-506308.txt",header=TRUE,na.strings="NA")
Richie.05<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Richie/7450-730440.txt",header=TRUE,na.strings="NA")
Richie.07<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Richie/7450-855854.txt",header=TRUE,na.strings="NA")
Richie.09<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Richie/7450-854914.txt",header=TRUE,na.strings="NA")
Richie.11<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Richie/7450-514202.txt",header=TRUE,na.strings="NA")
Richie.01$Depth<-1
Richie.03$Depth<-3
Richie.05$Depth<-5
Richie.07$Depth<-7
Richie.09$Depth<-9
Richie.11$Depth<-11
Richie<-rbind(Richie.01, Richie.03, Richie.05, Richie.07, Richie.09, Richie.11)
Richie$Lake<-"Richie"
#SARGENT
Sargent.01<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-527969.txt",header=TRUE,na.strings="NA")
Sargent.03<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-457033.txt",header=TRUE,na.strings="NA")
Sargent.05<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-433226.txt",header=TRUE,na.strings="NA")
Sargent.07<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-326790.txt",header=TRUE,na.strings="NA")
Sargent.09<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-708861.txt",header=TRUE,na.strings="NA")
Sargent.11<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-826935.txt",header=TRUE,na.strings="NA")
Sargent.13<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Sargent/7450-821901.txt",header=TRUE,na.strings="NA")
Sargent.01$Depth<-1
Sargent.03$Depth<-3
Sargent.05$Depth<-5
Sargent.07$Depth<-7
Sargent.09$Depth<-9
Sargent.11$Depth<-11
Sargent.13$Depth<-13
Sargent<-rbind(Sargent.01, Sargent.03, Sargent.05, Sargent.07, Sargent.09, Sargent.11, Sargent.13)
Sargent$Lake<-"Sargent"
#SISKIWIT
Siskiwit.01<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-813606.txt",header=TRUE,na.strings="NA")
Siskiwit.03<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-541933.txt",header=TRUE,na.strings="NA")
Siskiwit.05<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-636945.txt",header=TRUE,na.strings="NA")
Siskiwit.07<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-639850.txt",header=TRUE,na.strings="NA")
Siskiwit.09<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-328801.txt",header=TRUE,na.strings="NA")
Siskiwit.11<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-870449.txt",header=TRUE,na.strings="NA")
Siskiwit.13<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-772031.txt",header=TRUE,na.strings="NA")
Siskiwit.15<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-354904.txt",header=TRUE,na.strings="NA")
Siskiwit.17<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-506814.txt",header=TRUE,na.strings="NA")
Siskiwit.22<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-668203.txt",header=TRUE,na.strings="NA")
Siskiwit.27<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-552282.txt",header=TRUE,na.strings="NA")
Siskiwit.32<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-464014.txt",header=TRUE,na.strings="NA")
Siskiwit.37<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-863458.txt",header=TRUE,na.strings="NA")
Siskiwit.42<-read.table("C:/Myers/Projects/Cisco Oxythermal Habitat/Data/2018/Siskiwit/7450-390893.txt",header=TRUE,na.strings="NA")
Siskiwit.01$Depth<-1
Siskiwit.03$Depth<-3
Siskiwit.05$Depth<-5
Siskiwit.07$Depth<-7
Siskiwit.09$Depth<-9
Siskiwit.11$Depth<-11
Siskiwit.13$Depth<-13
Siskiwit.15$Depth<-15
Siskiwit.17$Depth<-17
Siskiwit.22$Depth<-22
Siskiwit.27$Depth<-27
Siskiwit.32$Depth<-32
Siskiwit.37$Depth<-37
Siskiwit.42$Depth<-42
Siskiwit<-rbind(Siskiwit.01, Siskiwit.03, Siskiwit.05, Siskiwit.07, Siskiwit.09, Siskiwit.11, Siskiwit.13, Siskiwit.15, Siskiwit.17, Siskiwit.22, Siskiwit.27, Siskiwit.32, Siskiwit.37, Siskiwit.42)
Siskiwit$Lake<-"Siskiwit"
#COMBINE LAKES
ISRO<-rbind(Richie, Sargent, Siskiwit)
ISRO$Date.CST<-as.Date(ISRO$Date.CST)
ISRO<-subset(ISRO, Date.CST>="2018-06-02" & Date.CST<="2018-09-04")
df1<-summaryBy(Temperature + Dissolved.Oxygen ~ Date.CST + Depth + Lake, data=ISRO, FUN=c(mean))
Fig.1<-ggplot(df1, aes(Depth, Temperature.mean, fill = Dissolved.Oxygen.mean)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 12.5, ymax = 16.5, alpha = .2)+
  geom_hline(yintercept = 23.8, linetype="dashed")+
  geom_hline(yintercept = 12.5, linetype="dashed")+
  geom_hline(yintercept = 16.5, linetype="dashed")+
  geom_bar(stat="identity", colour="grey50", width=1)+
  coord_flip()+
  scale_fill_gradient2(name="DO (mg/L)", low="red4", mid='white', high="#0072B2", space = 'rgb', guide = "colourbar", midpoint = 6, breaks=c(3,6,9), labels=c(3,6,9))+ 
  ggtitle(df1$Date.CST)+
  theme_bw()+
  theme(panel.spacing = unit(1.5, "lines"))+
  scale_x_reverse()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,25))+
  facet_grid(Lake ~ ., scales="free", space="free")+
  labs(title = 'Date: {frame_time}', x = 'Depth (m)', y = 'Temperature (°C)')+
  transition_time(Date.CST) +
  ease_aes('linear')
Fig.1
anim_save(file="C:/Myers/Projects/Cisco Oxythermal Habitat/Fig.1.gif")
#
#
#


