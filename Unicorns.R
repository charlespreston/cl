
library(ggplot2)
library(reshape2)
source(multiplot)

# build a data frame of common IT build and operate data
hardware  = c(45, 35, 10, 25, 19, 20, 21, 30, 32, 35, 28, 29)
osinstall = c(35, 25, 10, 21, 18, 19, 20, 25, 27, 30, 27, 29)
config    = c(25, 20, 10, 18, 17, 18, 19, 20, 22, 25, 25, 26)
upkeep    = c( 5, 10, 10, 12, 15, 17, 18, 19, 20, 20, 21, 22)
weeknum   = c( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12)

itdf = data.frame(hardware, config, osinstall, upkeep, weeknum)

# build a data fram of common pcf-devops data
hardware  = c(45, 43,  3,  6,  4,  4.5,  5,  6,  7,  5.7,  6,  7)  
osinstall = c(41, 42,  3, 3.5, 3.8, 4.1, 4.5, 4.7, 5.1, 5.5, 5.8, 6.1)
config    = c(40, 41,  3, NA, NA, NA, NA, NA, NA, NA, NA, NA)
upkeep    = c( 1,  3,  3, 3.3, 3.6, 3.9, 4.3, 4.6, 4.9, 5.3, 5.6, 5.9)

pcfdf = data.frame(hardware, config, osinstall, upkeep, weeknum)

moltenit  <- melt(itdf, id="weeknum")
moltenpcf <- melt(pcfdf, id="weeknum")

# server count
servers   = c( 0,  5,  8,  8, 10, 11, 12, 15, 16, 17, 18, 19)
newservers= c( 5,  3,  0,  2,  1,  1,  3,  1,  1,  1,  1,  1)
serverdf  <- data.frame(servers, newservers, weeknum)
moltenserver <- melt(serverdf, id="weeknum")

p1 <- ggplot(data=moltenit,  aes(x=weeknum, y=value, color=variable)) + geom_line() + 
      scale_x_discrete(limits=1:12) + theme(axis.text.x = element_blank()) +
      theme(axis.ticks = element_blank()) + theme(legend.position = "none") +
      labs(x = "", y="# of Hours")

p2 <- ggplot(moltenserver, aes(x=weeknum, y = value, fill=variable)) + geom_bar(stat="identity") + 
      scale_x_continuous(breaks = seq(1,12, 1), minor_breaks = NULL) + labs(x = "Week #") +
      theme(legend.position = "none") + labs(y = "Servers")

p3 <- ggplot(data=moltenpcf, aes(x=weeknum, y=value, color=variable)) + geom_line() + 
      scale_x_discrete(limits=1:12) + theme(axis.text.x = element_blank()) +
      theme(axis.ticks = element_blank()) +
      labs(x = "", y = "")

p4 <- ggplot(moltenserver, aes(x=weeknum, y = value, fill=variable)) + geom_bar(stat="identity") + 
      scale_x_continuous(breaks = seq(1,12, 1), minor_breaks = NULL) + labs(x = "Week #") +
      labs(y="")

multiplot(p1, p2, p3, p4, cols=2)

