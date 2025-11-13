re <- readline("for dataset together write 1, for homework one right 2:")
if (re == "2"){
    print("2 detected")
    #path to dataset 2 
    setwd("/Data")
}else{
    #path to dataset 1
    setwd("/Data")
}
#read in library
library(tidyverse)

#data file
if (re == "1"){
    print("1 detected")
    #dataset 1 name
    dat <- read.csv("crustacean.txt",header = T,sep = "\t")
}else{
    #dataset 2 name
    dat <- read.csv("rotifer.txt",header = T,sep = "\t")
}
#modify data file with additional arguments 
#dat <- read.csv("crustacean.txt",header = T,sep = "\t")
#check data class (type of object)
class(dat)
#Check column-data class
class(dat$lake.name)
class(dat$sum_mg_ww_L)
# preview of data (prints below)
head(dat)
#preview data type
str(dat)
#create a new column with a calculation 
dat$round <- round(dat$sum_mg_ww_L,digits = 2)


#preview, wont keep this all the time but yes
head(dat)

summary(dat$round)
#calculate the average (mean)
avg <- mean(dat$round)
avg
max(dat$round)
min(dat$round)
head(dat)
#if you have scientific notation...
options(scipen = 999)
# quick peek at data


head(dat)
# use pipe function to group obj together
# ctrl+shift+m (pipe shorthand)
all_avg <- dat %>%
group_by(lake.name) %>%
summarise(avg = mean(round))


ann_avg <- dat %>%
group_by(lake.name, Year) %>%
summarise(avg = mean(round),
stdev = sd(round))
# filter out data ("filter" in tidyverse)
bgm <- filter(dat,
    lake.name == "Big Moose")
# filter for Big Moose AND G Lake
two_lakes <- filter(dat,
lake.name == "Big Moose" |
lake.name == "G")
Large <- filter(two_lakes, round > 2)
#less than or equal to 2

small <- filter(two_lakes, round <= 2)
write.csv(ann_avg,"annual_average_per_lake.csv",row.names = F)
# create figures in R using ggplot2
library(ggplot2)
head (bgm)
# Create a figure with points
ggplot(data = bgm, aes(x = Year, y = round))+
    geom_point()
head(ann_avg)
ggplot(data = ann_avg, aes(x = Year, y = avg))+
    geom_point()+
facet_wrap(~lake.name)
#add axis labels
ggplot(data = ann_avg, aes(x = Year, y = avg))+
    geom_point(color = "blue")+
    facet_wrap(~lake.name)+
    ylab("Animal Density (mg/L)")+ xlab("")+
    theme_classic()
#print("test0")
head(bgm)
ggsave("scatter2.jpeg")
rr <- paste("R^2 = ", round(summary(lm(round ~ Year, data = bgm))$r.squared, 4))
p <- ggplot(data = bgm, aes(x= Year, y = round))+
    geom_point()+
    geom_smooth(method = "lm", se = F,
        color = "red", linetype = "dashed")+
    theme_classic()+
    ylab("Crustacean Density (mg ww / L)")+
    xlab("Date, Years")+
    annotate("text", x = max(bgm$Year) - (max(bgm$Year) - min(bgm$Year)) * 0.1, y = max(bgm$round),label = rr, size = 7,color = "red")
    
p = p + ggtitle(label = "Big Moose Lake Crustacean Density Over Time") + labs(caption = "Crustacean Density scatter plotted for Big Moose lake (mg ww / L) along the Y axis each year since 1994 along the X axis.\n The line of best fit for the data is plotted with a red dotted line \n and the fit % measureing adherence to the model in R^2 is displayed")
# save your plot using ggsave or "Export"
ggsave("scatter.jpeg", plot = p)

#bar
head(two_lakes)
# filter for just 2010-2012
last_years <- filter(two_lakes, Year>= 2010)
#print("test1")
# check data again
head(last_years)
# get average and standard deviation
mean_2_lakes <- last_years %>%
    group_by(lake.name) %>%
    summarise(mean = mean(round),
        sd = sd(round))
head(mean_2_lakes)
#print("test2")

plot2 <- ggplot(data = mean_2_lakes,
    aes(x = lake.name, y = mean,
        fill = lake.name))+
    geom_bar(stat = "identity")+
    theme_classic()+
    xlab("Name of Lake")+
    ylab("Crustacean Density (mg ww / L)")+
    labs(fill = "Name of Lake")+
    scale_fill_manual(values = c("blue", "orange"))

plot2 <- plot2 + ggtitle("Mean Crustacean Density for Lakes In New York") + labs(caption = "Bar graph comparing crustacean density in mg ww / L along the Y axis for different lakes along the x axis.") + theme( plot.caption.position ="plot")

ggsave("bargraph.jpeg")
#print("test3")
#make this graph look nice and save it
## word doc with scatter plot, bar plot, figure captions take away messages
### do again with rotifer data create a new r script
