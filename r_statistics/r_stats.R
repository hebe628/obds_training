###exercise 1 - generate/visualise a distribution###

library(ggplot2)
library(tibble)

#generate vector of 1000 normally distributed values w/ mean of 10 and stdev 5.
num_vector <- rnorm(1000, mean = 10, sd = 5) #random
summary(num_vector) #inspecting output

#compute mean and std dev
mean_vector <- mean(num_vector)
sd(num_vector)

#compute deciles (ie. 10 evenly spaced quantiles)
quantile(num_vector, probs = seq(0, 1, 0.1)) #(from, to, increment)
#or seq(0, 1, length.out = 10)

#visualise distribution as hist using baseR.
hist(num_vector, breaks = 20)

#ggplot2
vector_df <- data.frame(NAME = num_vector)
head(vector_df)
ggplot(data = vector_df, aes(x=NAME)) +
    geom_histogram() +
    geom_vline(xintercept = mean_vector)
#what is x?> tells you with head(vector_df)
#add a vertical line where the mean is on hist

#generate a new vector with a lot more values
large_vector <- rnorm(1e6, mean = 10, sd = 5)
summary(large_vector)
hist(large_vector, breaks = 100) #breakpts btw his cells.

--------------------------------------------------------------------------
###exercise 2 - query distributions and probabilities###
#for standard normal distribution (mean = 0, sd = 1)
    #Q1-plot cumulative distribution function in range [-5, 5]
    #Q2-Plot the inverse cumulative distribution function for quantiles in 0.01 increment.
    #Q3-Plot the density function in the range [-5, 5].
    #Q4-What is the probability of observing a value greater than 2?
    #Q5-What is the probability of observing a value between -2 and 2?
    #Q6-What is the probability of observing a value more extreme than -2 or 2?
----------------------------------------------------------------------------
#Q1-plot cumulative distribution function in range [-5, 5]
#set up x coordinate
x_index = seq(-5, 5, length.out = 1000) #-5 to 5 on x-axis

#generate required func of distribution and inpput it as col in tidyverse table (ie tibble)
#pnorm cumulative distribution function(cdf)
dst <- tibble(
    x_coordinate = x_index,
    cdf_normal_distribution = pnorm(q = x_index, mean = 0, sd = 1)
)

#plot by ggplot
dst %>%
    ggplot(aes(x = x_coordinate, y = cdf_normal_distribution)) +
    geom_point()

#Q2-Plot the inverse cumulative distribution function for quantiles in 0.01 increment.
#set up x coordinate for probabliity
x_prob = seq(0, 1, 0.01) #length.out = 1000

dst <- tibble(
    x_prob = x_prob,
    inverse_cdf_normal = qnorm(p = x_prob, mean = 0, sd = 1)
)

dst %>%
    ggplot(aes(x = x_prob, y = inverse_cdf_normal)) +
    geom_point()

#Q3-Plot the density function in the range [-5, 5].
#set up x coordinate
x_index = seq(-5, 5, length.out = 1000)

#generate required func of distribution and input it as col in tidyverse table (ie tibble)
dst <- tibble(
    x_coordinate = x_index,
    density_normal_distribution = dnorm(x = x_index, mean = 0, sd = 1)
)

#plot by ggplot
dst %>%
    ggplot(aes(x = x_coordinate, y = density_normal_distribution)) +
    geom_point()

#Q4-What is the probability of observing a value greater than 2?
1 - pnorm(2)

#Q5-What is the probability of observing a value between -2 and 2?
pnorm(2) - pnorm(-2)

#Q6-What is the probability of observing a value more extreme than -2 or 2?
1 - (pnorm(2) - pnorm(-2))


--------------------------------------------------------------------------
###exercise 3 -Compute an Empirical Cumulative Distribution Function
#Use the   function to compute the empirical cumulative distribution function for the variable in the data set.
#Use the   function to visualise the empirical cumulative distribution function.
#Use the   function on the output and compare this with the list of unique values for the
--------------------------------------------------------------------------
View(iris)
out <- ecdf(iris$Sepal.Length)
out(5:10)
out(5)
plot(out)
summary(iris)
out(4)
out(7.89)

--------------------------------------------------------------------------
###exercise 4 -Statistical tests
#iris gives measurements in cm of variable sepal length/width and petal length/width for 50 flowers from each of 3 species of iris.
#Q1- Use the summary() function to view some information about each column.
#Q2-Visualise the distribution of sepal length, stratified by species.
#Q3-Is sepal length normally distributed? Overall? Within each species?
#Q4-is there * variation of sepal length btw various species?
-------------------------------------------------------------------------

#Q1- Use the summary() function to view some information about each column.
summary(iris)
#Q2-Visualise the distribution of sepal length, stratified by species.
ggplot(iris, aes(Sepal.Length)) +
    geom_histogram(color = "black") +
    facet_wrap(~Species, ncol = 1)

#testing if it's normally distributed (all species combined)
shapiro.test(iris$Sepal.Length)

#Q3-Is sepal length normally distributed? Overall? Within each species?
#compare btw species
species_setosa <- subset(iris, Species == "setosa", select = Sepal.Length)
species_versicolor <- subset(iris, Species == "versicolor", select = Sepal.Length)
species_virginica <- subset(iris, Species == "virginica", select = Sepal.Length)

shapiro.test(species_setosa$Sepal.Length) #shapiro needs numeric vector
#it is not * deviating from normal distribution so can use t-test. p<0.5
shapiro.test(species_versicolor$Sepal.Length)
shapiro.test(species_virginica$Sepal.Length)
#all are normal distribution.

#Q4-is there * variation of sepal length btw various species?
#3+ groups, normal dis, not paired > 1-way ANOVA
anova <- aov(formula = Sepal.Length ~ Species, data = iris)
summary(anova) #***

levels(iris$Species)

#trying out 2 groups comparison so subset data to 2 groups first.
pair_species <- subset(iris, Species == "versicolor", select = Sepal.Length)
#comparing x to y #all *
ttest1 <- t.test(species_setosa$Sepal.Length, species_versicolor$Sepal.Length)
str(ttest1)

ttest2 <- t.test(species_setosa$Sepal.Length, species_virginica$Sepal.Length)
ttest2

ttest3 <- t.test(species_versicolor$Sepal.Length, species_virginica$Sepal.Length)
ttest3

#can try wilcoxon to see if it is powerful enough to detect *.

--------------------------------------------------------------------------
###exercise 5 -linear regression - fitting a linear model to measure effect of time & diet on weight.
#the chickweight data set measures the impact of different diets on the early growth of chicks.
#Q1- Fit a linear mode to measure the effect of time and diet.
#Q2- Which diet leads to the fastest increase in body weight?
#Q3- How much does weight increase per unit of time for top diet?
#Q4- Does the top diet drive an increase in body weight that is *faster than the next best diet?
-------------------------------------------------------------------------
ChickWeight
head(ChickWeight)

formula <- formula(weight ~ Diet + Time)
lm_chick <- lm(formula = formula, data = ChickWeight) #preditor on the R; what you want to predict on the L.
summary(lm_chick)
#(intercept the estimate of weight of chick at time 0; using diet 1 as ref)
#relative to diet 1 (coeff of 0 as ref), each diet would have that much more weight vs diet 1 (16g heavier , 30g heavier, 36g heavier for respective diet).
#coeff estimate is an average effect of e.g. diet 2 relative to diet 1).

#weight on y, time on x.
ggplot(ChickWeight, aes(x = Time, y = weight, color = Diet)) +
    geom_point() +
    geom_smooth(method = "lm")
    geom_abline(slope =  8.7505, intercept = 10.9244)
#smooth see pattern in presence of overplotting
#abline drawing average across diets, not on individual diet.

#taking diet and time instead of separately.
formula2 <- formula(weight ~ Diet * Time)
lm_chick <- lm(formula = formula2, data = ChickWeight)
summary(lm_chick)
#with diet 2 - what's extra weight on chick with respective to time.

#on average all gain 6.8g over time. diet 2 gives extra 1.7g on top etc..
ChickWeight$Diet <- relevel(ChickWeight$Diet, "3")
lm_chick <- lm(formula = formula2, data = ChickWeight)
summary(lm_chick)
levels(ChickWeight$Diet) #all diets defined by diet 3 (ref) #3>1>2>4

ggplot(ChickWeight, aes(x = Time, y = weight, color = Diet)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_abline(slope =  11.4229, intercept = 18.2503) +
    geom_abline(slope = (11.4229 - 4.5811) , intercept = (18.2503 + 12.6807))
#



--------------------------------------------------------------------------
###exercise 6 - multiple test correction
#Q1 - for each gene (row) in logcounts.csv, use cell_metadata.csv and a stats test to identify gene differentially expressed in cells infected w/ Salmonella relative to ctrl uninfected cells.
    #test 1 gene first, refactor the code into a function that returns p-value.
    #use vapply to apply that func to all genes.
#Q2 -correct p-values for multiple testing. How many genes remain before/after multiple testing?

-------------------------------------------------------------------------

log_counts <- read.csv("data/logcounts.csv", row.names = 1)
View(log_counts)
cell_md <- read.csv("data/cell_metadata.csv", row.names = 1)
View(cell_md)

gene1 <- data.frame("log_count" = as.numeric(log_counts[1,]), infection = cell_md$Infection)
test1 <- t.test(log_count~infection, gene1)
str(test1)
test1[['p.value']]

#
diff_exp <- function(gene_index, matrix, groups){
    gene_row <- data.frame("log_count" = as.numeric(matrix[gene_index,]), infection = groups)
    test1 <- t.test(log_count~infection, gene_row)
    return(test1[['p.value']])
}

diff_exp(3, log_counts, cell_md$Infection)
#apply(log_counts, 1, diff_exp, matrix = log_counts, groups = cell_md$Infection)
p_values <- vapply(seq(1, nrow(log_counts)), diff_exp, numeric(1), matrix = log_counts, groups = cell_md$Infection)
hist(p_values)
names(p_values) <- rownames(log_counts)
p_values

#correcting p-value
padj_values <- p.adjust(p_values, method = "holm")
hist(padj_values)

head(sort(padj_values))

human_go_bp <- read.csv("data/human_go_bp.csv")
go_info <- read.csv("data/go_info.csv")

#make list
#take 1st pathway
#turn 1st pathway's code into vapply
#do vapply
#for each , build table with Fisher's, how many belong in the pathway or not (over-rep)
