###15/10 exercise 1 - dimensionality reduction###

-------------------------------------------------------
#Q1 - perform PCA - how many PCs should be kept?
#Q2 - use exp metadata to visualise which cell types visually cluster together in PC space.
#Q3 - which groups of cells the top PCs are separating
#Q4 - find top genes associated w/ top PCs.Visualise gene exp values against PC coordinates.
#Q5 - compare PCA to tSNE to UMAP
-------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(cowplot)
library(umap)

#loading data
log_counts <- read.csv("data/logcounts.csv", row.names = 1)
View(log_counts) #has cell and gene ID
class(log_counts) #df

cell_metadata <- read.csv("data/cell_metadata.csv", row.names = 1)
View(cell_metadata) #infection, status, time, lane, plate, well
class(cell_metadata) #df

gene_metadata <- read.csv("data/gene_metadata.csv", row.names = 1)
View(gene_metadata) #gene ID and name.
class(gene_metadata) #df

#convert from df to matrix first - turn its argument into a matrix
log_count_matrix <- as.matrix(log_counts)
class(log_count_matrix) #matrix array
#should check dim before running PCA so know what to expect e.g.time.
dim(log_count_matrix) #1000 rows x 342 cols

#PCA using prcomp - performs on 'data matrix'
log_count_pca <- prcomp(t(log_count_matrix), center = TRUE, scale. = TRUE)
summary(log_count_pca) #has PC342

str(log_count_pca) #under Value under help page - this list corresponds to there.
View(log_count_pca$x) #we mistakenly done PCA on gene rather than sample*.
#we want row names as cells; sample are cols!
#so need to transpose table

#plotting PCA
plot(log_count_pca) #quick and dirty view - x-axis different PCs so PC1 most variable.
#but need to convert to df first otherwise won't plot.
#ggplot on PC1 v PC2
ggplot(as.data.frame(log_count_pca$x), aes(x = PC1, y = PC2)) +
    geom_point()

#plots variances against no of PCs
#plotting 50 components as line plot
screeplot(log_count_pca, npcs = 50, type = 'lines')
#best to pick where the 'bottom of elbow' is; where it becomes linear. pick 10-12 here.

#plot % of variance
#first create a new df, 1 row per PC
log_count_pca$sdev #numeric
#grabbing all these values and put int a new df. #ending up w/ 3 cols: sdev, pc, %_var (342 rows)
scree_pca_sdv <- data.frame(
    var = log_count_pca$sdev ^ 2,
    pc = seq(1, length(log_count_pca$sdev))) %>%
    mutate(
        percentage_var = var/sum(var)* 100,
        cumulative_var = cumsum(percentage_var))
#cumulative wil add up after each PC.
class(scree_pca_sdv)
#tibble let you to refer to col you just created.

View(scree_pca_sdv)
#alternative seq_along(log_count_pca$sdev) will also list 1-342.

scree_pca_sdv[1:100, ]#showing 3 cols for only 100 rows.

ggplot(scree_pca_sdv[1:100, ], aes(x = pc, y = percentage_var)) +
    geom_point()
#PC one has huge variance.

ggplot(scree_pca_sdv[1:100, ], aes(x = pc, y = cumulative_var)) +
    geom_point()

#use these plots to decide how many PCs to give UMAP.

pca_df <- as.data.frame(log_count_pca$x)
View(pca_df)
pca_df <- rownames_to_column(pca_df) #calling rowname for cells.
View(pca_df)
cell_md <- rownames_to_column(cell_metadata)
View(cell_md)

#combining 2 tables
pca_df <- pca_df %>%
    left_join(cell_md)
View(pca_df)

#by status
ggplot(pca_df, aes(x = PC1, y = PC2, color = Status)) +
geom_point()
#by time - time seems a big component.
ggplot(pca_df, aes(x = PC1, y = PC2, color = Time)) +
    geom_point()
#by infection
ggplot(pca_df, aes(x = PC1, y = PC2, color = Infection)) +
    geom_point()

ggplot(pca_df) +
    geom_density(aes(PC1, fill = Status), color = "black", alpha = 0.5) +
    facet_grid(Time~Infection) +
    theme_cowplot()
#cowplot- simple half-open frame and no grid
#0.5 opaque so easier to see overlap.
#pc1 separating time pts


str(log_count_pca)
View(log_count_pca$rotation)

#extracting all genes from PC1.
topgenes_PC1 <- log_count_pca$rotation[, "PC1"] #cant use $ for matrix.
View(topgenes_PC1)
print(topgenes_PC1)

#getting top genes from PC1 by sorting.
sorted_topgenes_PC1 <- sort(topgenes_PC1, decreasing = TRUE)
View(sorted_topgenes_PC1)
#alternatively head(sort(topgenes_PC1, decreasing = TRUE))
#identifoies (ENSG00000172183) ISG20 as top gene in PC1.

#####exercise 2 - clustering###

--------------------------------------------------------------------------
#Q1 - cluster the data using kmeans
#Q2 - how many clusters would you choose
#Q3 - compare known cluster labels and known metadata
--------------------------------------------------------------------------

#kmeans and hierarical
#need to transpose table so samples in rows
kmeans <- kmeans(t(log_count_matrix), center = 4)
kmeans
#centers= no of clusters;usage everything in name with value are default.
#str(kmeans)

#head(kmeans$cluster, 10)
#View(pca_df)
#View(kmeans$cluster)

pca_df$cluster <- as.factor(kmeans$cluster[pca_df$rowname]) #ordering kmeans$cluster according to rowname
pca_df$cluster
#kmeans$cluster
#pca_df$rowname

#unsupervised
#adding color to each cluster
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point() #4 clusters separate well.

#checking
kmeans$withinss #Vector of within-cluster sum of squares, one component per cluster.
kmeans$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

candidate_k = 2:20 #better to give a var name as you may forget to update the numbers later.
km <- sapply(candidate_k, function(i){
    km <- kmeans(t(log_count_matrix), centers = i)
    sum(kmeans$withinss)
})
km
kmeans_sums <- data.frame(sumwithinss = km, k = candidate_k)
kmeans_sums

ggplot(kmeans_sums, aes(x = k, y = sumwithinss)) +
    geom_point()

#compare cluster labels
cluster_label <- data.frame(cluster = pca_df$cluster, time = pca_df$Time)
table(cluster_label)

cluster_infection <- data.frame(cluster = pca_df$cluster, time = pca_df$Infection)
table(cluster_infection)

gg_cluster <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point()

gg_time <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Time)) +
    geom_point()

gg_infection <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Infection)) +
    geom_point()

plot_grid(gg_cluster, gg_time, gg_infection)


###cluster using UMAP components
umap_dc <- umap(log_count_pca$x)
umap_dc
str(umap_dc)

head(umap_dc$layout) #,1 and ,2 refer to coordinates (as supposed to PC1/2)
umap_coordinate <- as.data.frame(umap_dc$layout)
head(umap_coordinate)

#Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively
umap_coordinate <- cbind(umap_coordinate, cell_metadata)
head(umap_coordinate)


p1 <- ggplot(as.data.frame(umap_coordinate), aes(x = V1, y = V2, color = Time)) +
    geom_point() +
    theme_cowplot()

p2 <- ggplot(as.data.frame(umap_coordinate), aes(x = V1, y = V2, color = Status)) +
    geom_point() +
    theme_cowplot()

p3 <- ggplot(as.data.frame(umap_coordinate), aes(x = V1, y = V2, color = Infection)) +
    geom_point() +
    theme_cowplot()

plot_grid(p1, p2, p3)
