library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

print(getwd())
setwd("/media/patrick/250GB/school/comp bio/Lab 8 01/data")
info <- read.csv("Info_Data.csv")
data <- read.csv("Interpolated_Data.csv")
lakes <- c("131" = "Lake George","76" = "76","1" = "1","55" = "55")
print("input lake id:")
lakeId <- as.integer(readline())
#print(lakes[[as.character(lakeId)]])
#print(lakes[[as.character(76)]])
measureDepth <- 5

lakeLookUp <- function(iD) {
re <- info %>%
  filter(lake_id == iD) %>%
  pull(name)
return(re)
}

print(lakeLookUp(131))
dat <- data %>% filter(lake_id == lakeId) %>% filter(depth <= measureDepth)
#print(dat)
dat <- dat %>% filter({ 
    m <- month(ymd(date)) 
    m %in% c(7,8)
})
#print(dat)

col_names <- c("temp","do_con")


byYear <- dat %>% group_by(year) %>% summarize(temp = mean(temp), do_con = mean(do_con))

print(byYear)

regr <- lm(temp ~ year, data = byYear)
print(summary(regr))
rr <- paste("slope:",round(coef(regr)[2],4)," R^2: ",round(summary(regr)$r.squared,3), " p-value: ",round(summary(regr)$coefficients[4],4),sep="")
#temp
ggplot(data = byYear, aes(x= year, y = temp))+
    geom_point()+
    #geom_smooth(method = "lm", se = F,color = "red", linetype = "dashed")+
    geom_smooth(method = "lm")+
    theme_classic()+
    ylab("Annual Temp Celcius of Lake Surface")+
    xlab("Year")+
    ggtitle(label = paste("Lake Temperature at first ",measureDepth, "Meters Depth for Lake: ",info %>% filter(lake_id == lakeId) %>% pull(name), sep = ""))+
    annotate("text", x = max(byYear$year) - (max(byYear$year) - min(byYear$year)) * 0.4, y = max(byYear$temp),label = rr, color = "red")
    
#p = p + ggtitle(label = paste("Lake Temperature at Depth ",measureDepth, " Meters for Lake: ",lakes[[as.character(lakeId)]], sep = ""))
# save your plot using ggsave or "Export"
ggsave(paste("Average Temp ",lakeId," Scatter Plot.jpeg", sep = ""))


#o2
regr2 <- lm(do_con ~ year, data = byYear)
#print(summary(regr))
#print(paste("slope:",summary(regr)$coefficients["year"],sep=""))
#print("\n\n\n\n")
#print("\n\n\n\n")
rr2 <- paste("slope:",round(coef(regr2)[2],4)," R^2: ",round(summary(regr2)$r.squared,3), " p-value: ",round(summary(regr2)$coefficients[4],4),sep="")
#print(rr2)
ggplot(data = byYear, aes(x= year, y = do_con))+
    geom_point()+
    #geom_smooth(method = "lm", se = F,color = "red", linetype = "dashed")+
    geom_smooth(method = "lm")+
    theme_classic()+
    ylab("Annual Disolved Oxygen mg/l of Lake Surface")+
    xlab("Year")+
    ggtitle(label = paste("Lake Disolved Oxygen for First ",measureDepth, " Meters Depth for Lake: ",info %>% filter(lake_id == lakeId) %>% pull(name), sep = "")) +   
    annotate("text", x = max(byYear$year) - (max(byYear$year) - min(byYear$year)) * 0.45, y = max(byYear$do_con),label = rr2, color = "red")
    
#p = p + ggtitle(label = paste("Lake Disolved Oxygen of first",measureDepth, " Meters of surface for Lake: ",lakes[[as.character(lakeId)]], sep = ""))
# save your plot using ggsave or "Export"
ggsave(paste("Average Disolved Oxygen ",lakeId," Scatter Plot.jpeg", sep = ""))