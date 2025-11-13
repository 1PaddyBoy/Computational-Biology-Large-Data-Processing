
library(ggplot2)
library(dplyr)
print(getwd())
setwd("/media/patrick/250GB/school/comp bio/Lab 4/Lab 4/data")
a <- c("19129AreaResults.csv","34859AreaResults.csv","36659AreaResults.csv","19129lengthsResults.csv","34859lengthsResults.csv","36659lengthsResults.csv")
print(a)
#print(list.files(pattern="\\.csv$"))
#a <- list.files(pattern="\\.csv$")
d1 <- read.csv("19129AreaResults.csv")
print(d1)
dat <- list()
for(i in 1:6){
    print(i)
    print(a[i])
    dat[[i]] <- read.csv(file = a[i])
}
#print(data)
#s <- data.frame()
s <- summary(as.data.frame(dat[1])$Area)
#print(dat[1])
#print(as.data.frame(dat[1])$Area)
#print(summary(as.data.frame(dat[1])$Area))
#s1 <- data.
standardD <- c(sd(as.data.frame(dat[1])$Area))
for(i in 2:3)
{
    #print(i)
    #print(data[i])
    #print(as.data.frame(data[i])$Area)
    s <- rbind(s, summary(as.data.frame(dat[i])$Area))
    #s1[i] <- summary(read.csv(a[i])$Area)
    standardD[i] <- c(sd(as.data.frame(dat[i])$Area))
}
for(i in 4:6)
{
    #print(i)
    #print(data[i])
    #print(as.data.frame(data[i])$Area)
    s <- rbind(s,summary(as.data.frame(dat[i])$Length))
    #s1[i] <- summary(read.csv(a[i])$length)
    standardD[i] <- c(sd(as.data.frame(dat[i])$Length))
}
print("onto s")
#print(s[1])
#print(s1[1])
#print(s1)

#print(read.csv(a[1]))

#print(summary(as.data.frame(read.csv(a[1]))$Area))
rownames(s) <- a

print(s)
print("between")
write.csv(s,"summary.csv")
#print(rownames(s))
#print("between")
#print(sort(rownames(s)))
#s <- as.data.frame(s)
#s %>% arrange(sort(rownames(s)))
#s <- s[order(sort(rownames(s)))]
print(s)
print(standardD)


for(i in 1:3)
{
    ggplot(data = as.data.frame(dat[i]), aes(x=Area))+
        geom_histogram(bins = 5) + 
        geom_histogram(bins = 5, fill = "navy blue") +
        xlab("Area of cells in square micro meters") + 
        ggtitle(paste("Image ", substring(a[i], first = 1, last = 5)," Cell Area"))
    ggsave(paste(a[i],"histogram.jpeg", sep =""))

}

for(i in 4:6)
{
    ggplot(data = as.data.frame(dat[i]), aes(x=Length))+
        geom_histogram(bins = 5, fill = "navy blue") + 
        xlab("Length of cells in micro meters") + 
        ggtitle(paste("Image ", substring(a[i], first = 1, last = 5)," of  Cell Length"))
    ggsave(paste(a[i],"histogram.jpeg", sep =""))

}
