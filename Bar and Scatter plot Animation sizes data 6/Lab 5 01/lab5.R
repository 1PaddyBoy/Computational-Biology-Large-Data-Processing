print(getwd())

library(dplyr)
library(ggplot2)
setwd("/media/patrick/250GB/school/comp bio/Lab 5/Lab 5 01/Data/Finch Data")
print("start sucess")

dat <- read.csv("finches_data_for_lab.csv")
#total <- dat %>% group_by(drought_survival) %>% summarise(weight = mean(body_mass_g), sd = sd(body_mass_g))
survivers <- dat %>% group_by(drought_survival) %>% summarise(weight = mean(wing_length_mm), sd = sd(body_mass_g))
total <- dat %>% group_by(drought_survival) %>% summarise(across(where(is.numeric), list(mean=mean), na.rm=TRUE))
totalsd <- dat %>% group_by(drought_survival) %>% summarise(across(where(is.numeric), list(sd=sd), na.rm=TRUE))
total <- dat %>% group_by(drought_survival) %>% summarise(weight = mean(wing_length_mm))
print(as.data.frame(total))
dat <- dat %>% group_by(drought_survival)
total <- dat %>% summarise(across(everything(),mean))
#print(survivers)
print(as.data.frame(total))
total <- as.data.frame(total)

print("test0")
data=total %>% select(beak_depth_mm)
print(data)
names <- c("Body Mass (g)","Wing length (mm)","Tarsus Length (mm)","Beak Depth (mm)")
realnames <- c("body_mass_g","wing_length_mm","tarsus_length_mm","beak_depth_mm")
for (var in realnames)
{
total <- dat %>% group_by(drought_survival) %>% summarise(weight = mean(.data[[var]]),sd =sd(.data[[var]]))
print(total)
ggplot(data=total, aes(x=drought_survival, y=weight, fill=drought_survival)) +
  geom_bar(stat="identity")+
  ggtitle(paste(names[match(var,realnames)],"for Birds That Did and Didn't Survive Drought of 1977"))+
  geom_errorbar(aes(ymin=weight-sd, ymax=weight+sd), width=.2) +
  ylab(names[match(var,realnames)])+
  xlab("Nonsurvivor vs Survivor of Drought")+
    theme_classic()+
  scale_fill_manual(values=c('blue','orange'))
#ggsave(paste(var,"bar.jpeg", sep =""))

}
for(var in realnames)
{
  print(dat[[var]])
    print(dat$drought_survival)
    print(t.test(dat[[var]] ~ dat$drought_survival))
}
datS <- dat %>% filter(drought_survival == "survivor")
print(as.data.frame(datS))
rr <- paste("R^2 = ", round(summary(lm(beak_depth_mm ~ wing_length_mm, data = datS))$r.squared, 4))
p <- ggplot(data = datS, aes(x= wing_length_mm, y = beak_depth_mm))+
    geom_point()+
    geom_smooth(method = "lm", se = F,
        color = "red", linetype = "dashed")+
    theme_classic()+
    ylab("Beak Depth (mm)")+
    xlab("Wing Length (mm)")+
    annotate("text", x = max(datS$wing_length_mm) - (max(datS$wing_length_mm) - min(datS$wing_length_mm)) * 0.1, y = max(datS$beak_depth_mm),label = rr, size = 7,color = "red")+
    ggtitle(label = "Wing Length vs Beak Depth for Finches that survived the 1977 drought")    
#p = p + ggtitle(label = "Big Moose Lake Crustacean Density Over Time") + labs(caption = "Crustacean Density scatter plotted for Big Moose lake (mg ww / L) along the Y axis each year since 1994 along the X axis.\n The line of best fit for the data is plotted with a red dotted line \n and the fit % measureing adherence to the model in R^2 is displayed")
# save your plot using ggsave or "Export"
ggsave("wing_length vs beakdepth.jpeg", plot = p)
print(datS$beak_depth_mm)
print(datS$dat$wing_length_mm)
print(cor.test(datS$wing_length_mm, datS$beak_depth_mm))