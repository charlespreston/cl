library(ggplot2)
library(reshape2)
source(multiplot)

# build a data frame of common IT build and operate data
hardware  = c(45, 35, 10, 25, 39, 20, 21, 30, 32, 35, 28, 29, 27)
osinstall = c(35, 25, 10, 21, 38, 19, 20, 25, 37, 30, 27, 29, 27)
config    = c(25, 20, 10, 18, 27, 18, 19, 20, 32, 33, 25, 26, 28)
ops    = c( 5, 20, 10, 12, 25, 27, 28, 19, 20, 30, 31, 22, 26)
weeknum   = c( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12)

itdf = data.frame(hardware, config, osinstall, ops, weeknum)

# build a data fram of common pcf-devops data
hardware  = c(4.5, 4.3,  3,  .6, .4, 4.5, .50, .60, 1.7,  .7,  .6,  .7)
osinstall = c(4.1, 4.2,  3, .5, .8, .1, .5, .7, 1.1, .5, .8, .1)
config    = c(4.0, 4.1,  3, 0, 0, 0, 0, 0, 0, 0, .5, 0)
ops    = c( 1,  3,  3.5, 3.3, 1.6, .9, 4.3, .6, .9, 1.3, 1.6, 1.9)

pcfdf = data.frame(hardware, config, osinstall, ops, weeknum)

moltenit  <- melt(itdf, id="weeknum")
moltenpcf <- melt(pcfdf, id="weeknum")

# server count
effort   = c( 0,  5,  8,  8, 10, 11, 8, 4, 3, 8, 12, 5)
neweffort= c( 5,  3,  0,  2,  1,  1,  3,  1,  1,  1,  1,  1)
serverdf  <- data.frame(effort, neweffort, weeknum)
moltenserver <- melt(serverdf, id="weeknum")

p0 <- ggplot(data=moltenit,  aes(x=weeknum, y=value, color=variable)) + geom_line() +
  scale_x_continuous(breaks = seq(1,12, 1), minor_breaks = NULL) + theme(axis.text.x = element_blank()) +
  theme(axis.ticks = element_blank())  + ggtitle("Traditional Operations") +
  labs(x = "Time", y="Spend")

p1 <- ggplot(data=moltenit,  aes(x=weeknum, y=value, fill=variable)) + geom_area(stat="identity") +
  scale_x_discrete(limits=1:20) + theme(axis.text.x = element_blank()) + coord_cartesian(ylim=c(0:150)) +
  theme(axis.ticks = element_blank())  + theme(legend.position = "none") + ggtitle("Traditional Operations") +
  labs(x = "Time", y="Spend")

p2 <- ggplot(moltenserver, aes(x=weeknum, y = value, fill=variable)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(1,12, 1), minor_breaks = NULL) + labs(x = "Week #") +
  theme(legend.position = "none") + labs(y = "effort")

p3 <- ggplot(data=moltenpcf, aes(x=weeknum, y=value, fill=variable)) + geom_area(stat="identity") +
  scale_x_discrete(limits=1:20) + theme(axis.text.x = element_blank()) + coord_cartesian(ylim=c(0:20)) +
  theme(axis.ticks = element_blank()) + ggtitle("PCF Operations") +
  labs(x = "", y = "")

p4 <- ggplot(moltenserver, aes(x=weeknum, y = value, fill=variable)) + geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(1,12, 1), minor_breaks = NULL) + labs(x = "Week #") +
  labs(y="")

multiplot(p1, p3, p2, p4, cols=2)
