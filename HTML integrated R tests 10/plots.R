library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

print(getwd())
setwd("/media/patrick/250GB/school/comp bio/Lab 8 01/data")
csv <- "final_data"

info <- read.csv(csv)
chromo <- info %>% filter(Ch._Num > 0)

regr <- lm(C_value ~ ch.Num, data = chromo)
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