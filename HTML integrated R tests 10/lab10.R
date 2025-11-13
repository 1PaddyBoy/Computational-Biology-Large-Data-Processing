library(ggplot2)
library(dplyr)
library(ggtext)
print(getwd())
setwd("/media/patrick/250GB/school/comp bio/Lab 10 01/data")
a <- "36659lengthsResults.csv"
print(a)
#print(list.files(pattern="\\.csv$"))
#a <- list.files(pattern="\\.csv$")
dat <- read.csv(file = a)
#print(data)
#s <- data.frame()

#print(i)
    #print(data[i])
    #print(as.data.frame(data[i])$Area)
    s <- summary(as.data.frame(dat)$Length)
    #s1[i] <- summary(read.csv(a[i])$length)
    standardD <- c(sd(as.data.frame(dat)$Length))
    
ggplot(data = as.data.frame(dat), aes(x=Length))+
        geom_histogram(bins = 15, fill = "blue") + 
        xlab("Length of Cells in Micro Meters") + 
        ylab("Count of Cells") + 
        labs(
            title = paste0(
      "Image ", substring(a, 1, 5),
      " of <i style='color:blue;'>Paramecium multimicronucleatum</i><br>",
      "Dried Fixed Cell Lengths"
    )
        )+
         theme(
    plot.title = element_markdown(size = 14, face = "bold", lineheight = 1.1)
  )+
  annotate("text", x = max(as.data.frame(dat)$Length) - (max(as.data.frame(dat)$Length) - min(as.data.frame(dat)$Length)) * 0.3, y = 15,label = paste("Median of Cell Lengths is: ", s[3]," Micro Meters",sep=""), size = 4,color = "red")
        #ggtitle(paste0("&#9733; Image ", substring(a, first = 1, last = 5)," of  <u style='color:blue;'>Paramecium multimicronucleatum</u> \n dried fixed Cells Length &#9733;"))
    ggsave(paste(a,"histogram.jpeg", sep =""))
    print(s)