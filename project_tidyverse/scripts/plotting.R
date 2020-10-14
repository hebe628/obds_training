library(tidyverse)
library(gridExtra)
library(cowplot)
library(patchwork)
#library(biomaRt)
#library(pheatmap)

data("mtcars")

par()$lwd
par(mar=c(6,6,5,5), lwd = 2, mfrow=c(1,2)) #mfrow sets plotting area into a 1*2 array
par()

str(mtcars)
table(mtcars$gear)

barplot(table(mtcars$gear),
        xlab = "Number of gears",
        ylab = "Number of cars",
        main = "A main title\nspread across two lines",
        col = "white",
        border = "black")
#end of line characters so string go to 2nd line.
abline(h=6, lwd = 2)

#making vector of colours, named by gears.
colours <- c("red", "green", "blue")
names(colours) <- c(3,4,5)
colours

gears_column <- as.character(mtcars$gear) #subset colour by its name (which is 4, 3, 5)
gears_column
colours[gears_column]


par(mar=c(6,6,5,5), lwd = 2)
plot(mtcars$mpg, mtcars$hp,
     col = colours[gears_column],
     pch = 16,
     cex = 2,
     xlab = "Miles per gallon",
     ylab = "Horse power",
     cex.lab = 1)

legend(legend = sort(unique(mtcars$gear)), x = "right",
       fill = c("red", "green", "blue"))

#plot 2 plots side-by-side by changing global graphical paras
