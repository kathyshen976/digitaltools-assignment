install.packages("lubridate")
install.packages("patchwork")
installed.packages("hrbrthemes")
library(tidyverse)
library(scales)
library(xlsx)
library(xlsx2dfs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(patchwork) 
library(hrbrthemes)

#lct <- Sys.getlocale("LC_TIME") 
#Sys.setlocale("LC_TIME", "C")

###Data Importing and Date format Transformation
data1<-read.xlsx("./Data for SMI/ABBN Historical Data.xlsx")
data1$Date<-as.Date(data1$Date, '%b %d,%Y')
data1<-data1%>%
  mutate(name = 'ABBN')
data2<-read.xlsx("./Data for SMI/ALCC Historical Data.xlsx")
data2$Date<-as.Date(data2$Date, '%b %d,%Y')
data2<-data2%>%
  mutate(name = 'ALCC')
data3<-read.xlsx("./Data for SMI/CFR Historical Data.xlsx")
data3$Date<-as.Date(data3$Date, '%b %d,%Y')
data3<-data3%>%
  mutate(name = 'CFR')
data4<-read.xlsx("./Data for SMI/CSGN Historical Data.xlsx")
data4$Date<-as.Date(data4$Date, '%b %d,%Y')
data4<-data4%>%
  mutate(name = 'CSGN')
data5<-read.xlsx("./Data for SMI/GEBN Historical Data.xlsx")
data5$Date<-as.Date(data5$Date, '%b %d,%Y')
data5<-data5%>%
  mutate(name = 'GEBN')
data6<-read.xlsx("./Data for SMI/GIVN Historical Data.xlsx")
data6$Date<-as.Date(data6$Date, '%b %d,%Y')
data6<-data6%>%
  mutate(name = 'GIVN')
data7<-read.xlsx("./Data for SMI/LHN Historical Data.xlsx")
data7$Date<-as.Date(data7$Date, '%b %d,%Y')
data7<-data7%>%
  mutate(name = 'LHN')
data8<-read.xlsx("./Data for SMI/LONN Historical Data.xlsx")
data8$Date<-as.Date(data8$Date, '%b %d,%Y')
data8<-data8%>%
  mutate(name = 'LONN')
data9<-read.xlsx("./Data for SMI/NESN Historical Data.xlsx")
data9$Date<-as.Date(data9$Date, '%b %d,%Y')
data9<-data9%>%
  mutate(name = 'NESN')
data10<-read.xlsx("./Data for SMI/NOVN Historical Data.xlsx")
data10$Date<-as.Date(data10$Date, '%b %d,%Y')
data10<-data10%>%
  mutate(name = 'NOVN')
data11<-read.xlsx("./Data for SMI/PGHN Historical Data.xlsx")
data11$Date<-as.Date(data11$Date, '%b %d,%Y')
data11<-data11%>%
  mutate(name = 'PGHN')
data12<-read.xlsx("./Data for SMI/ROG Historical Data.xlsx")
data12$Date<-as.Date(data12$Date, '%b %d,%Y')
data12<-data12%>%
  mutate(name = 'ROG')
data13<-read.xlsx("./Data for SMI/SCMN Historical Data.xlsx")
data13$Date<-as.Date(data13$Date, '%b %d,%Y')
data13<-data13%>%
  mutate(name = 'SCMN')
data14<-read.xlsx("./Data for SMI/SGSN Historical Data.xlsx")
data14$Date<-as.Date(data14$Date, '%b %d,%Y')
data14<-data14%>%
  mutate(name = 'SGSN')
data15<-read.xlsx("./Data for SMI/SIKA Historical Data.xlsx")
data15$Date<-as.Date(data15$Date, '%b %d,%Y')
data15<-data15%>%
  mutate(name = 'SIKA')
data16<-read.xlsx("./Data for SMI/SLHN Historical Data.xlsx")
data16$Date<-as.Date(data16$Date, '%b %d,%Y')
data16<-data16%>%
  mutate(name = 'SLHN')
data17<-read.xlsx("./Data for SMI/SRENH Historical Data.xlsx")
data17$Date<-as.Date(data17$Date, '%b %d,%Y')
data17<-data17%>%
  mutate(name = 'GIVN')
data18<-read.xlsx("./Data for SMI/UBSG Historical Data.xlsx")
data18$Date<-as.Date(data18$Date, '%b %d,%Y')
data18<-data18%>%
  mutate(name = 'UBSG')
data19<-read.xlsx("./Data for SMI/UHR Historical Data.xlsx")
data19$Date<-as.Date(data19$Date, '%b %d,%Y')
data19<-data19%>%
  mutate(name = 'UHR')
data20<-read.xlsx("./Data for SMI/ZURN Historical Data.xlsx")
data20$Date<-as.Date(data20$Date, '%b %d,%Y')
data20<-data20%>%
  mutate(name = 'ZURN')
data21<-read.xlsx("./Data for SMI/SMI Historical Data.xlsx")
data21$Date<-as.Date(data21$Date, '%b %d,%Y')
data21<-data21%>%
  mutate(name = 'SMI')
###Data with and without SMI
dataSum1<-rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,
               data11,data12,data13,data14,data15,data16,data17,data18,
               data19,data20,data21)
dataSum2<-rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,
                data11,data12,data13,data14,data15,data16,data17,data18,
                data19,data20)

###Data Visualization
library(hrbrthemes)
###SMI with smooth curve:
###To see the general stock price change of the period from February to December
###in Swizerland
SMI<-ggplot(data21, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("SMI Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))



###SMI Price and Volume:
### To see the price change and the market activity. Trading volume can
###reflect people's thoughts about the market. Like in the first wave, we
### notice that there is a sudden lage jump of trading volume accompanied by
###the big decrease of the stock price, which may be because people have a
###vague picture of the future.
coeff <- 10000
PriceColor <- "red"
VolumeColor <- rgb(0.2, 0.6, 0.9, 1)
ggplot(data21, aes(x=Date)) +
  geom_line( aes(y=Price), size=0.2, color=PriceColor) + 
  geom_line( aes(y=Volume / coeff), size=0.2, color=VolumeColor) +
  scale_y_continuous(
    name = "Price",
    sec.axis = sec_axis(~.*coeff, name="Volume")) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = PriceColor, size=13),
    axis.title.y.right = element_text(color = VolumeColor, size=13)
  ) +
  ggtitle("SMI Plot")
 

###Plot for ABBN
ABBN<-ggplot(data1, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("ABBN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for ALCC
ALCC<-ggplot(data2, aes(Date, Price)) +
    geom_line(na.rm=TRUE, color="red", size=0.2) + 
    geom_smooth()+
    ggtitle("ALCC Plot") +
    xlab("Date(month) of 2020") + ylab("Price")+ 
    (scale_x_date(limits=start.end,
                  breaks=date_breaks("1 month"),
                  labels=date_format("%m")))
                
###Plot for CFR
ggplot(data3, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("CFR Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for CSGN
ggplot(data4, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("CSGN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for GEBN
ggplot(data5, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("CEBN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for GIVN
ggplot(data6, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("GIVN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for LHN
ggplot(data7, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("LHN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for LONN
ggplot(data8, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("LONN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for NESN
ggplot(data9, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("NESN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for NOVN
ggplot(data10, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("NOVN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for PGHN
ggplot(data11, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("PGHN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for ROG
ggplot(data12, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("ROG Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for SCMN
ggplot(data13, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("SCMN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for SGSN
ggplot(data14, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("ABBN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))


###Plot for SIKA
ggplot(data15, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("SIKA Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for SLHN
ggplot(data16, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("SLHN Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for SRENH
ggplot(data17, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("SRENH Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Plot for UBSG
ggplot(data18, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("UBSG Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for UHR
ggplot(data19, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("UHR Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Plot for ZURN
ggplot(data20, aes(Date, Price)) +
  geom_line(na.rm=TRUE, color="red", size=0.2) + 
  geom_smooth()+
  ggtitle("SIKA Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###HERE we just divide the SMI by the industry the companies are in.
###Pharmacy
data12$Price<-data12$Price/3
dataP<-rbind(data2,data10,data12)
ggplot(dataP, aes(Date, Price)) +
  geom_line(aes(color = name), size=0.2) + 
  geom_smooth()+
  ggtitle("Pharmacy Plot(With (Price of ROG)/3)") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Banks
dataB<-rbind(data4,data18)
ggplot(dataB, aes(Date, Price)) +
  geom_line(aes(color = name), size=0.2) + 
  geom_smooth()+
  ggtitle("Banks Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))

###Chemistry
data6$Price<-data6$Price/10
dataC<-rbind(data6,data8,data15)
ggplot(dataC, aes(Date, Price)) +
  geom_line(aes(color = name), size=0.2) +
  geom_smooth()+
  ggtitle("Chemistry Plot(With (Price of GIVN)/10)") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))
###Insurance
dataI<-rbind(data16,data17,data20)
ggplot(dataI, aes(Date, Price)) +
  geom_line(aes(color = name), size=0.2) + 
  geom_smooth()+
  ggtitle("Insurance Plot") +
  xlab("Date(month) of 2020") + ylab("Price")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 month"),
                labels=date_format("%m")))


