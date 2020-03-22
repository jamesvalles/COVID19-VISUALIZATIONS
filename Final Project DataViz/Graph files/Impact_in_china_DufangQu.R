c <- read.csv("confirmed.csv", header=TRUE, stringsAsFactors = TRUE)
d <- read.csv("deaths.csv", header=TRUE)
r <- read.csv("recovered.csv", header=TRUE)

head(confirmed)

confirmed <- c[,c(1,54)]
confirmed <- melt(confirmed, id=c("Province.State"))
confirmed <- confirmed[,c(1,3)]

colnames(confirmed)[2] <- "Confirmed"
confirmed$Date <- sub("X","", confirmed$Date)
confirmed$Date <-gsub("\\.","-",confirmed$Date)
confirmed$Date <- as.Date(confirmed$Date, format="%m-%d-%Y")
colnames(confirmed)[6] <- "confirmed"
head(confirmed)

deaths <- d[,c(1,54)]
deaths <- melt(deaths, id=c("Province.State"))
deaths <- deaths[,c(1,3)]
head(deaths)


colnames(deaths)[2] <- c("Deaths")
deaths$dDate <- sub("X","", deaths$dDate)
deaths$dDate <-gsub("\\.","-",deaths$dDate)
deaths$dDate <- as.Date(deaths$dDate, format="%m-%d-%Y")
head(deaths)

recovered <- r[,c(1,54)]
recovered <- melt(recovered, id=c("Province.State"))
recovered <- recovered[,c(1,3)]
head(recovered)


colnames(recovered)[2] <- c("Recovered")
recovered$rDate <- sub("X","", recovered$rDate)
recovered$rDate <-gsub("\\.","-",recovered$rDate)
recovered$rDate <- as.Date(recovered$rDate, format="%m-%d-%Y")
head(recovered)

ncv <- cbind(confirmed, recovered, deaths)
head(ncv)
ncv <- ncv[,c(1,2,4,6)]

ncvbp <- ncv %>%
  group_by(Province.State) %>%
  summarize_each(sum)




summary(ncv)
write.csv(ncv, "ncv.csv")


library(dplyr)
# Seperate lat and long info to another table
location <- ncv[,c(1,2,3,4)]
location <- location[!duplicated(location[,c(1:4)]),]
str(location)
ncv <- ncv[,-c(3,4)]
head(ncv)
write.csv(location, "location.csv")


#Aggregate data by country
ncv_w <- read.csv("ncv_world.csv",header=TRUE)
head(ncv_w)
sum_ncv <- ncv_w %>%
  subset(select=c(3,6,7,8,9)) %>%
  group_by(Country.Region, Date) %>%
  summarise_each(sum)
head(sum_ncv)
sum_ncv$Country.Region <- gsub("North Ireland", "Northen Ireland", sum_ncv$Country.Region)
write.csv(sum_ncv, "sum_ncv.csv")


sum_ncv2 <- sum_ncv %>%
  subset(select=c(1,3,4,5)) %>%
  group_by(Country.Region) %>%
  summarise_each(sum)
write.csv(sum_ncv2, "sum_ncv2.csv")




# Aggregate China data by province
ncv <- read.csv("ncv.csv",header=TRUE)
chinabypr <- ncv
chinabypr <- chinabypr[,-c(1, 3:6)]

chinabypr <- chinabypr %>%
  group_by(Province.State) %>%
  summarise_each(sum)

chinabypr$Death_Rate <- chinabypr$Deaths/chinabypr$confirmed
chinabypr$Recover_Rate <- chinabypr$Recovered/chinabypr$confirmed
write.csv(chinabypr, "chinabypr.csv")

str(chinabypr)
library(readxl)
prgdp <- read_excel("China-gdp.xlsx")
head(prgdp)

# explore correlations between GDP and recover rate
cor <- merge(chinabypr,prgdp, by.x=1, by.y=2)
colnames(cor)[8] <- "gdp_pc"
head(cor)

cor2 <- cor[-which(cor$Province.State=="Hubei",)]
cor2$gdp_pc <- cor2$gdp_pc/10000
str(cor2)
cor(cor2$Recover_Rate, cor2$gdp_pc)
lm = lm(Recover_Rate~ gdp_pc, data=cor2)
summary(lm)


#Swine flu data cleansing
swflu <- read.csv("Swine flu 2009.csv", header = TRUE)
head(swflu)
swflu$Date <- gsub("/","-",swflu$Date)
swflu$Date <- as.Date(swflu$Date,format="%m-%d-%Y")
swflu$Year <- factor(format(swflu$Date,"%Y"))

swflu_sum <- swflu %>%
  subset(select=-4) %>%
  group_by(Year, Country) %>%
  summarise_each(sum)
write.csv(swflu_sum,"sfsum.csv")
head(swflu_sum)

##############################################
#Aggregate China Case
china <-read.csv("ncv_china.csv")
head(china)
china <- china[,c(2,7,8,9)]
aggchina <- china %>%
  group_by(Province.State) %>%
  summarise_each(sum)
head(aggchina)

write.csv(aggchina, "aggchina.csv")

# Add half year of data to covid data
covid19 <- read.csv("covid_19.csv")
head(covid19)
covid19 <- covid19[,-c(3,4)]
covid19$Date <-gsub("/","-",covid19$Date)
covid19$Date <- as.Date(covid19$Date, format="%m-%d-%Y")

China <- covid19[which(covid19$Country.Region == "China"),]

china <- China[,-c(1,2)]
head(china)

china_day <- china %>%
  group_by(Date) %>%
  summarise_each(sum)
head(china_day)
write.csv(china_day, "c.csv")

#Date = seq(from=as.Date("2019-09-20"), to=as.Date("2019-12-20"), by = 'month')
Date = c("2019-01-20","2019-02-20","2019-03-20","2019-04-20",
         "2019-05-20","2019-06-20","2019-07-20","2019-08-20",
         "2019-09-20","2019-10-20","2019-11-20","2019-12-20")
Date <- as.data.frame(Date)
str(Date)

df <- matrix(0, ncol=3, nrow=12)
df <- as.data.frame(df)
head(df)
df <- cbind(Date, df)
colnames(df) <- c("Date", "Confirmed", "Deaths", "Recovered")
df$Date <- as.Date(df$Date)
head(df)

china_day <- rbind(df, china_day)
head(china_day)
cfmd <- china_day[,-(3:4)]
head(cfmd)

write.csv(cfmd, "cfmd.csv")
#############################################################

#scale data
summary(mftr$Value)
summary(nonmftr$Value)
summary(sse$Close)
summary(china_sep$Confirmed)

china_sep$Confirmed_sc <- china_sep$Confirmed*(65/80026) 
summary(china_sep$Confirmed_sc)


### Complete economy data 
Date <- seq(as.Date("2019-09-19"), as.Date("2020-02-20"), by="days")
Date <- as.data.frame(Date)
str(Date)

left <- left_join(Date, mftr)
left

####### Legend #######
library("cowplot")
legend_m <- get_legend(mp)
legend_nm <- get_legend(nmp)




ggdraw() +
  draw_plot(mp1, x=0, y = 0.6, width = 1, height=.3) +
  draw_plot(nmp1, x=0, y = 0.3, width = 1, height=.3) +
  draw_plot(chinabyday, x=0, y = 0, width = .75, height=.3)


mpl + geom_line(data = chinabyday, aes(x = month(date) , y = Confirmed))


# Bar chart for air pollution
air <- read.csv("wuhanair.csv")
head(air)
air$Date.x <- gsub("/","-", air$Date.x)
air$Date.x <- as.Date(air$Date.x, format="%m-%d-%Y")

plt <- air[which(air$Type=="AQI"),]
plt$pos <- plt$hndif < 0
ggplot(plt, aes(x=Date.x, y=hndif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("AQI Difference Compared To Previous Year")

plt <- air[which(air$Type=="CO"),]
plt$pos <- plt$whdif < 0
ggplot(plt, aes(x=Date.x, y=whdif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("CO Difference Compared To Previous Year")

plt <- air[which(air$Type=="NO2"),]
plt$pos <- plt$whdif < 0
ggplot(plt, aes(x=Date.x, y=whdif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("NO2 Difference Compared To Previous Year")

plt <- air[which(air$Type=="O3"),]
plt$pos <- plt$whdif < 0
ggplot(plt, aes(x=Date.x, y=whdif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("03 Difference Compared To Previous Year")

pm25 <- air[which(air$Type=="PM2.5"),]
pm25$pos <- pm25$whdif < 0
ggplot(pm25, aes(x=Date.x, y=whdif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("PM2.5 Difference Compared To Previous Year")

plt <- air[which(air$Type=="PM10"),]
plt$pos <- plt$whdif < 0
ggplot(plt, aes(x=Date.x, y=whdif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("PM10 Difference Compared To Previous Year")

plt <- air[which(air$Type=="SO2"),]
plt$pos <- plt$whdif < 0
ggplot(plt, aes(x=Date.x, y=whdif, fill=pos)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("SO2 Difference Compared To Previous Year")

airbym <- read.csv("wuhan1719.csv")
head(airbym)
airbym <- na.omit(airbym)
airbym$Date <- as.Date(paste(airbym$Date, "-01", sep=""))
airbym$Year <- as.numeric(format(as.Date(airbym$Date), "%Y"))
airbym$Year <- as.factor(airbym$Year)
airbym$Month <- as.numeric(format(as.Date(airbym$Date), "%m"))
airbym$MonthAbb <- months(as.Date(airbym$Date), abbreviate=TRUE)


airp <- ggplot(airbym, aes(Month, AQI, group=Year, color=Year)) +
  geom_line() + xlab("") +
  scale_x_continuous(breaks=airbym$Month, labels=airbym$MonthAbb, limits=c(1,3))
airp



library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape)

#New graph
aqi <- read.csv("AQI.csv")
aqi <- aqi[1:59,]
head(aqipivot)
aqi$Date <- mdy(aqi$Date)

aqipivot <- aqi[,c(1,5,6,7)]
aqipivot <- melt(aqipivot, id="Date")
head(aqipivot)
ggplot(aqipivot, aes(Date, value, color=variable)) +
  geom_line()

aqipivot <- na.omit(aqipivot)

ggplot(aqipivot %>% count(variable, value) %>%     
         mutate(pct=n/sum(n)),              
       aes(variable, n, fill=value)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5)) +
  scale_fill_discrete(breaks=c("Good","Moderate","Slightly Unhealthy","Unhealthy","Very Unhealthy")) +
  scale_fill_manual(values=c("#006d2c", "#31a354", "#f7fcb9","#fb6a4a","#de2d26"), name="Air Quality") +
  coord_flip()

aqipivot$value <- as.factor(aqipivot$value)
levels(aqipivot$value)
summary(aqipivot)

aqi$dif19 <- (aqi$X2020 - aqi$X2019)/aqi$X2019
aqi$dif18 <- (aqi$X2020 - aqi$X2018)/aqi$X2018
aqi$pos19 <- aqi$dif19 < 0
aqi$pos18 <- aqi$dif18 < 0

aqipivot[which(aqipivot$value == ""),]

whaqi <- ggplot(aqi, aes(x=Date, y=dif19, fill=pos19)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("Air Quality Index Of Wuhan")

ggplot(aqi, aes(x=Date, y=dif18, fill=pos18)) +
  geom_col(position="identity") +
  scale_x_date(limits=as.Date(c("2019-01-01","2019-03-03")), date_breaks = "1 month", labels = date_format("%b")) +
  theme(legend.position = "none") +
  xlab("") + ylab("Air Quality Index Of Wuhan")

 


###  PMI chart  #######



####  pmi line chart

pmiplot <- pp + scale_x_date(breaks = as.Date(c("2019-01-01", "2019-04-01",
                                                "2019-07-01", "2019-10-01",
                                                "2020-01-01", "2020-02-01"), data_label="%Y,%b")) +
  theme(axis.line = element_line(colour = "dark gray"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        axis.ticks=element_blank())+
  theme_light()

pmiplot

#### PMI bar chart
ggplot(pmip, aes(Date, Value)) +
  geom_col(color="blue", fill="blue",alpha=0.2) +
  geom_line(data=sse, aes(x=Date, y=Close)) +
  geom_line(data=china_day, aes(x=Date, y=Confirmed))

pmiplot + geom_line(data=sse, aes(x=Date, y=Close, inherit.aes=FALSE)) +
  geom_line(data=china_day, aes(x=Date, y=Confirmed, inherit.aes=FALSE))

write.csv(cbi, "cbi.csv")

##############################   final #######################
library(RColorBrewer)
library(ggplot2)
library(scales)
library(lubridate)
install.packages("extrafont")
library(gridExtra)
library(grid)
library(gtable)
library(extrafont)
###### heatmap - airpolution ######
data <- read.csv("wuhanaqi.csv")
head(data)
data$abbr <- ifelse(data$Month == 1, "Jan",
                    ifelse(data$Month == 2, "Feb",
                           ifelse(data$Month == 3, "Mar",
                                  ifelse(data$Month==4, "Apr",
                                         ifelse(data$Month==5, "May",
                                                ifelse(data$Month==6, "Jun",
                                                       ifelse(data$Month==7, "Jul", 
                                                              ifelse(data$Month==8, "Aug",
                                                                     ifelse(data$Month==9, "Sept",
                                                                            ifelse(data$Month==10, "Oct",
                                                                                   ifelse(data$Month==11, "Nov", "Dec"
                                                                                   )))))))))))
head(data)  
data$Date <- gsub("/","-", data$Date)  
data$Date <- as.Date(data$Date, format="%m-%d-%Y")
data$ym <- zoo::as.yearmon(paste(data$Year, data$Month), "%Y %m") 
data$Month <- as.factor(data$Month)
data1 <- data
data1$Day <- as.factor(data1$Day)
class(data$Day)
levels(data$Year) <- c("Before the Outbreak", "Outbreak")
data$Levels <- as.factor(data$Levels)
data$label[data$Year == 2019] <- "Before the Outbreak"
data$label[data$Year == 2020] <- "Outbr"

plot <- ggplot(data, aes(Day, y=factor(Month, levels = rev(levels(factor(Month)))))) + 
  geom_tile(aes(fill= Levels), color="white")+
  facet_grid(rows = vars(label), scale="free_y", space="free_y")+
  scale_x_continuous(limits=c(0, 32), breaks=c(1,5,10,15,20,25,30), expand=c(0,0)) +
  scale_y_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                   labels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))


aqiplot <- plot +  ylab("") + xlab("") + ggtitle("Wuhan Air Quality Index")+
  theme(plot.title=element_text(hjust=0, color="#6F6E6E", face="bold",size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(),
        panel.border=element_rect(fill = NA, colour='gray',size=0.5),
        legend.title = element_blank(),
        axis.text.y =element_text(size=10, color="#929292"),
        strip.text.y=element_text(size=11, face="bold",color ="#929292"),
        legend.position = "top",
        legend.key.size = unit(0.5, "cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.text = element_text(size=9, color="#929292"))

cols <- c("5" = "#626060", "4" = "#bababa", "3" = "#f7f7f7", "2" = "#a6dba0", "1"="#008837")

aqiplot <- aqiplot + scale_fill_manual(values=cols, labels=c("Good","Moderate",
                                                          "Slightly Unhealthy",
                                                          "Unhealthy",  
                                                          "Very Unhealthy")) +
  guides(fill = guide_legend(reverse = TRUE))

library(ggplot2)
aqiplotm <- aqiplot + theme(
  plot.margin = margin(0.5, 2, 0.5, 0.6,"cm"),
  plot.background = element_rect(
    fill = "white",
    colour = "white",
    size = 1)) 
 

aqiplotm
#######  SSE data  #######

sse <- read.csv("SSE-2020.csv", header=TRUE )
head(sse)
sse$Date <- gsub("/","-",sse$Date)
sse$Date <- as.Date(sse$Date, format="%m-%d-%Y")

ssebymonth <- sse %>%
  group_by(Date=floor_date(Date, "month")) %>%
  summarize_each(funs=(mean))

sseplot <- ggplot(sse, aes(Date, Close)) +
  geom_line(aes(color=(Date > as.Date("2020-01-21")))) +
  scale_colour_manual(values=c("#929292", "#fb6a4a")) +
  scale_x_date(date_labels = "%m/%Y") +
  scale_y_continuous(breaks=c(2600,2800,3000,3200),labels=c("2.6","2.8","3.0","3.2"))+
  ggtitle("") +  theme_light() +
  theme(legend.position="None", 
        plot.title=element_text(hjust=0, color="#6F6E6E", face="bold", size=10),
        axis.line = element_line(colour = "#929292"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color="#929292"),
        axis.text =element_text(color="#929292"))+
  xlab("") + ylab("") +ggtitle("Shanghai Stock Exchange Composite Index (thousand)")

sseplotm <- sseplot + theme(
  plot.margin = margin(0.5, 0.6, 0.5, 2, "cm"),
  plot.background = element_rect(
    fill = "white",
    colour = "white",
    size = 1))
sseplotm

#### pmi area ####
library(lubridate)
library(dplyr)
library(reshape)
library(ggplot2)
pmi <- read.csv("cpmi.csv", header=TRUE)
pmi <- pmi[c(3),]
pmi <- melt(pmi, id=c("Indicators"))
head(pmi)
colnames(pmi) <- c("Indicator", "Date", "Value")

write.csv(pmi,"pmi.csv")

pmi <- read.csv("pmi.csv")
head(pmi)
pmi$Date <- sub("X","", pmi$Date)
pmi$Date <- gsub("/","-", pmi$Date)
pmi$Date <- mdy(pmi$Date)

summary(pmi)
pmi <- pmi[,-c(1,2)]
pmi$Date <- as.Date(pmi$Date)
pmip <- pmi
pmip$Centraled <- pmip$Value - 50
pmip <- pmip %>%
  mutate(label=if_else(Centraled > 0, "P","N"))

interp <- approx(pmip$Date, pmip$Centraled, n=1000)
cbi <- data.frame(Date=interp$x, Centraled=interp$y)
cbi$label[cbi$Date >= 18261.16] <- "neg"
cbi$label[cbi$Date < 18261.16] <- "pos"


write.csv(cbi, "cbi1.csv")
cbi <- read.csv("cbi1.csv")


pmiplot <- ggplot(cbi,  aes(x=Date, y=Centraled)) +
  geom_line(aes(color=label)) +
  geom_hline(yintercept=0, linetype="dotted", color = "#929292", size=0.4) +
  scale_color_manual(values=c("#fb6a4a", "#929292"), labels=c("During the outbreak", "Before the outbreak")) +
  scale_x_continuous(expand=c(0, 0), limits = c(17880, 18310),
                     breaks=c(17896, 17981.29, 18066.58,18151.87,
                              18237.16),
                     label=c("01/2019","04/2019","07/2019", "10/2019",
                             "01/2020")) +
  scale_y_continuous(limits=c(-25, 10), breaks=c(-20, -10, 0, 10), 
                     label=c("30","40","50","60")) +
  xlab("") + ylab("") +
  ggtitle(label="Composite Purchasing Manager's Index",
          subtitle = "(Lower than 50 indicates contract)") +
  theme(plot.title = element_text(hjust=0, color="#6F6E6E",face="bold", size=10),
        plot.subtitle = element_text(hjust=0, color="#6F6E6E", size=9),
        legend.position = c(1, 3.6),
        legend.title = element_blank(),
        legend.text = element_text(color="#929292"),
        legend.key.size = unit(0.7,"line"),
        axis.line = element_line(colour = "#929292"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(color="#929292"),
        axis.ticks.x = element_line(color="#929292")) 


pmiplotm <-  pmiplot + theme(
  plot.margin = margin(0.5, 0.6, 0.5, 2, "cm"),
  plot.background = element_rect(
    fill = "white",
    colour = "white",
    size = 1)) 

pmiplotm

#### Air traffic ######
traffic <- read.csv("airpassenger.csv")
head(traffic)
traffic$Date <- gsub("/","-",traffic$Date)
traffic$Date <- as.Date(traffic$Date, format="%m-%d-%Y")
summary(traffic)

tinterp <- approx(traffic$Date, traffic$Air.Passenger.yoy, n=1000)
tcbi <- data.frame(Date=tinterp$x, Centraled=tinterp$y)
tcbi$label[tcbi$Date >= 18249] <- "neg"
tcbi$label[tcbi$Date < 18249] <- "pos"
head(tcbi)

write.csv(tcbi,"tcbi1.csv")
tcbi <- read.csv("tcbi1.csv")
tplot <- ggplot(tcbi,  aes(x=Date, y=Centraled)) +
  geom_line(aes(color=label)) +
  geom_hline(yintercept=0, linetype="dotted", color = "#929292", size=0.3, alpha=0.5) +
  scale_color_manual(values=c("#fb6a4a", "#929292")) +
  scale_x_continuous(expand=c(0, 0), limits = c(17910, 18310),
                     breaks=c(17918, 18001.54, 18085.08,18168.62,
                              18252.16),
                     label=c("01/2019","04/2019", "07/2019", 
                             "10/2019","01/2020") ) +
  scale_y_continuous(labels=c("-5","0","5","10","15"))+
  xlab("") + ylab("") +
  ggtitle("Air Passenger Traffic (%)") +
  theme(plot.title = element_text(hjust=-0, color="#6F6E6E", face="bold", size=10),
        legend.position = "none",
        axis.line = element_line(colour = "#929292"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(color="#929292"),
        axis.ticks.x = element_line(color="#929292")) 

tplotm <- tplot + theme(
  plot.margin = margin(0.5, 2, 0.5, 0.6,"cm"),
  plot.background = element_rect(
    fill = "white",
    colour = "white",
    size = 1))

tplotm

######  Covid-19 ######

cfmd <- read.csv("cfmd.csv")
cfmd$Date <- gsub("/","-", cfmd$Date)
cfmd$Date <- mdy(cfmd$Date)
head(cfmd)



# china_sep <- china_day[nonmftr[,2] > as.Date("2019-08-20"),]

covid <- ggplot(cfmd, aes(x=Date, y=Confirmed)) +
  geom_area(aes(fill="#fb6a4a"), alpha=0.8)  +
  ylab("") + xlab("") + ggtitle("Confirmed Covid-19 Cases in China (thousand)") +
  scale_y_continuous(labels = c("0","20","40","60","80")) +
  scale_x_date(date_labels = "%m/%Y")+
  theme(plot.title = element_text(hjust=0, color="#6F6E6E", face="bold", size=10),
        legend.position = "none",
        axis.line = element_line(colour = "#929292"),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(color="#929292"),
        axis.ticks.x = element_line(color="#929292"))

covidm = covid + theme(
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(0.5, 0.6, 0.5, 2, "cm"),
    plot.background = element_rect(
      fill = "white",
      colour = "white",
      size = 1)) +
  labs(fill="") 

covidm

########## Title #########


tgrob <- textGrob(expression(bold("Covid-19 Impact In China")), 
                  gp = gpar(col = "#6F6E6E", fontsize = 20))

###### Panel  #######
gl <- list(covidm, pmiplotm, sseplotm, tplotm, aqiplotm,tgrob)

lay <- rbind(c(6,6),
             c(1,4),
             c(1,4),
             c(2,5),
             c(2,5),
             c(3,5),
             c(3,5))
grid.arrange(grobs=gl, ncol=2,
              layout_matrix=lay)
