#ggplot exercise 13/10 Tuesday

library(tidyverse)
library(gridExtra)
library(cowplot)
library(patchwork)
library(scales)

#import
coding_gene_region <- read.table("data/coding_gene_region.bed")
getwd()

#add col names and a new col for genomic_interval
colnames(coding_gene_region) <- c("chrom", "start", "end", "ID", "score", "strand") 
coding_gene_region$genomic_interval <- coding_gene_region[["end"]] - coding_gene_region[["start"]]

#plot a histogram 
#add plot title
#change x and y axis title/size
#change size and angle of x tick labels
#change x axis upper limit to 500,000
#change no of bins or bin width #size of genomic going to each bar.
#change fill and border colour of bars
ggplot(coding_gene_region, aes(x = genomic_interval)) +
  geom_histogram(bins=100, fill = "white", colour = "black") +
  labs(title = "Histogram of genomic interval",
    x = "genomic_interval", 
       y = "Count") +
  theme(axis.title.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(angle = 45, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
  xlim(0, 500000) +
  scale_x_continuous(labels = comma) 
#axis.title will do for both axes.
#you want numbers to be normal not scientific notation - will get warning but fine.
#bin will splits into no of bins/bars whereas bin width is the width for each bar.




#


