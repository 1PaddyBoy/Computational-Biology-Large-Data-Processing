# this is code for you
library(ggplot2)
library(dplyr)
# check working directory!
print(getwd())
# read in your data set as a csv
# this is an example data set you need to create your own spread sheet from the 
# student data sheet and load that in instead!
#this is for baboon
dat <- read.csv("Data/student_data_sheet.csv")
head(dat)

# first figure cpg island map 
# this is tricky to figure out so here is the bare bones code go ahead and modify it make it look nice for your data
# this 'maps' the location for each sequence
# first identify data
ggplot(data = dat)+
  # next we want to make a segment from the start to the end with the sequence number being the y axis
  geom_segment(aes(x = start_site, y = Sequence, xend = end_site, yend = Sequence), linewidth=5)+
  # just make a nice theme
  theme_classic()+
  # change x axis label
  xlab("Site Location in characters of 5k length gene")+
  ylim(0,15)
# you should add axis labels change colors etc
ggsave("plottest.jpeg")
# save plot and add to word document with caption and 
# bullet point take away



# Create box and whisker plot for GC content and length 
# try to figure this out on your own and refer to powerpoint
# hint use geom_boxplot()
# leave x blank and make y = to your variable of choice
# modify graphs to make them look nice
# paste into word document

# length box plot
ggplot(data = dat, aes(y=length))+
  geom_boxplot(fill="light grey",color="red")

ggsave("boxplottest.jpeg")


# GC content box plot
ggplot(data = dat, aes(y=GC_content))+
  geom_boxplot(fill="light grey",color="red")

ggsave("boxplottest2.jpeg")


# calculate summary statistics for 
# gc content and length
#print(summary(dat$GC_content))
#print(summary(dat$length))
df <- rbind(summary(dat$GC_content),summary(dat$length))
rownames(df) <- c("GC_content","length")
print(df)
#df <-merge(as.data.frame(summary(dat$GC_content)),as.data.frame(summary(dat$length)))
#print(merge(as.data.frame(summary(dat$GC_content)),as.data.frame(summary(dat$length))))

write.csv(df, "summary.csv")
# find the mean median min max did you learn a function to return all of this information?
# put this into a table add to word doc 
# make the table in microsoft word or excel
#print(mean(dat$length))

