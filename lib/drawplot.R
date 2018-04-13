library(ggplot2)
library(reshape)


ggplot.data <- melt(stock_norm[,1:10], id="date")

ggplot(data=ggplot.data, aes(x=date, y=value, colour=variable)) + geom_line()

ggplot.data <- melt(stock_norm[,c(1,10:15)], id="date")

ggplot(data=ggplot.data, aes(x=date, y=value, colour=variable)) + geom_line()
  
dev.off()
