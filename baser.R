###vectors, factors, and lists exercises ###

#generate a character vector of length 5 and assign names.
zoo <- c("giraffe", "tiger", "monkey", "zebra", "lion")
names(zoo) <- c("Ben", "George", "Harry", "Jim", "Ken")
names(zoo)
zoo
#access element 1 and 3 by name. 
zoo[c("Ben", "Harry")]
#access last element
length(zoo)
zoo[length(zoo)]
#replace element 4 with a new element.
zoo[4] <- c("bear")
zoo

num <- c(6, 3, 2, 18)
bool <- c(TRUE, FALSE, TRUE, TRUE)
num[bool]
sum(num[bool])
num * bool #true indicates multiply by 1; false by 0.

num_2 <- c(3, 4, 12)
num + num_2
#count how many NA vlues
missing <- c(9, 4, NA, 2, 43, NA, 4, 2)
is.na(missing) #specific to na function. 
sum(is.na(missing)) #sum up
which(is.na(missing)) #tells location.

#generate an ordered factor of length 6. check the level of factor. 
signal <- factor(c("red", "yellow", "yellow", "green", "red", "green"), 
                 levels = c("red", "yellow", "green"), ordered = TRUE)
levels(signal)
#replace element 2 with a value not listed in levels 
signal[2] <- c("blue")
#make a list a character vector, a bool vector, a factor, and a numeric vector.
new_list <- list(zoo, bool, signal, num)
new_list
#use [[]] [] to access 1) 2nd & 3rd items in list; 2) 2nd element of the 3rd item in list.
new_list[2:3]
subset_list <- (new_list[2:3])
subset_list
class(subset_list)
new_list[c(2,4)]

class(new_list[[3]])
new_list[[3]][2]
new_list[[3]][[2]]
class(new_list[3])

#add names to list elements and access using $
names(new_list) <- c("animals", "booleons", "trafficlight", "numbers")
new_list
class(new_list$animals)
#change the order of factor levels inside the list and add a new level to the factor.
new_list$trafficlight
levels(new_list$trafficlight)
levels(new_list$trafficlight) <- c("yellow", "red", "green")

###lapply/sapply exercises###

#create a numeric vector of length 10.
numeric_vector <- 1:10
numeric_vector
length(numeric_vector)

#lappy and sapply to square each element
lapply(numeric_vector, function(x) {x **2}) #to the power
lapply(numeric_vector, function(x) {x ^2}) 
1 <- lappy(numeric_vector, function(x) x ^ 2)
class(1)
sapply(numeric_vector, function (x) x ^ 2)
classs(sapply(numeric_vector, function (x) x ^ 2))
numberic_vector


num_bool <- list(1:2, 3:9, c(TRUE, FALSE), c(FALSE, FALSE))
num_bool

num_bool <- list(
  1:2,
  3:9,
  c(TRUE, FALSE),
  c(FALSE, FALSE)
)
lapply(num_bool, sum)

sapply(num_bool, sum)

rep(num_bool[[1]], 3)
sapply(num_bool[[1]], function(x) rep(x, 3))
rep(num_bool[[1]], each = 3)


#
sapply(num_bool, function(x) rep(x, each = 3))

#convert new list into a single vector.
unlist(sapply(num_bool, function(x) rep(x, each = 3))) #6 0 mean 6 false - bool changing to no. 
unlist(sapply(num_bool, function(x) rep(as.character(x), each = 3))) #forcing to characters. 

###matrix exercises###

#create a matrix with 5 rows 2-100 w/ increments of 2. 
my_matrix <- matrix(seq(2, 100, 2), nrow =5, byrow = TRUE)
my_matrix

#sum of squares of each row # 1 is row; 2 is col.
apply(my_matrix, 1, function(x) sum(x ^ 2))
#sanity check
2^2 + 4^2 + 6^2 + 8^2 + 10^2 + 12^2 + 14^2 + 16^2 + 18^2 + 20^2

#min and max for each col
my_matrix[,1:10]
apply(my_matrix, 2, function(x) c(min(x), max(x)))

second_matrix <- matrix(seq(200:259), nrow = 10, ncol = 6)
#checking dimensions before joining.
dim(second_matrix)
second_matrix
transposed_matrix <- t(second_matrix)
dim(transposed_matrix)
dim(my_matrix)
my_matrix
transposed_matrix

#joining 2 matrix together.
joined_matrix <- rbind(my_matrix, transposed_matrix) 

#convert joined matrix into a df.
conversion1 <- as.data.frame(joined_matrix)
class(conversion1)
#convert df to a list.
as.list(conversion1)

###Dataframe exercise###
#loading file into R > filezilla it to your folder.
setwd("/Users/hchen/desktop/OBDS/Downloads/Course material/Week5/1210_monday/")
getwd()
coding_gene_region <- read.table("coding_gene_region.bed")
dim(coding_gene_region) #checking dimension of df 16471 x 6
class(coding_gene_region) #df
class(coding_gene_region[,4]) #apply doesnt work on dataframe. #matrix 1 data type.
apply(coding_gene_region, 2, class) #displaying V1-V6 cols all as characters
colnames(coding_gene_region) #listing V1-V6 w/o acutal names. 

#assign col names
colnames(coding_gene_region) <- c("chrom", "start", "end", "ID", "score", "strand") 
View(coding_gene_region) #view on dataframe

#add a new col calucating legnth of genomic interval
coding_gene_region$genomic_interval <- coding_gene_region[["end"]] - coding_gene_region[["start"]]
#coding_gene_region$genomic_interval <- coding_gene_region$end - coding_gene_region$start (alternative)
View(coding_gene_region) #[]extract col [[]]


View(coding_gene_region)
class(coding_gene_region["end"]) #df
class(coding_gene_region[["end"]]) #[[]]integer

#sorting genomic_interval col
sorted <- coding_gene_region[order(-coding_gene_region$genomic_interval),]
#- indicates descending otherwise default is ascending; ,all rows & cols
View(sorted)

#extract element at row 30, col 3.
coding_gene_region[30, 3]
#extract 2nd col by index and name (using 2 methods)
coding_gene_region[ , 2] #all rows but 2nd col - start
coding_gene_region$start #alternative

#which chr has largest interval
max(coding_gene_region$genomic_interval)
coding_gene_region[coding_gene_region$genomic_interval == max(coding_gene_region$genomic_interval),] #this gives the whole row info.
coding_gene_region[["chrom"]][coding_gene_region$genomic_interval == max(coding_gene_region$genomic_interval)] #this just indicates which chr.
max_chrom <- coding_gene_region$chrom[coding_gene_region$genomic_interval == max(coding_gene_region$genomic_interval)]

#subsetting df to contain 100001-200000b and write to a txt file to inlude only col names.
sorted[sorted$genomic_interval <= 200000 & sorted$genomic_interval >= 100001,] #only the rows that meet the critera.
subset_sorted <- sorted[sorted$genomic_interval <= 200000 & sorted$genomic_interval >= 100001,] #now gives 1 col.
write.table(subset_sorted,
            "subet_sorted.txt",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE) #quotes false removes "" in txt; 1st col is shown as row ID but dont want that so FLASE for row.names.

#in original df, replace the score value with 100 for genomic_intervals on chr4 and 17 that are + strand & > 200000bp.
coding_gene_region$score[coding_gene_region$chrom %in% c("chr4", "chr17") & 
  coding_gene_region$strand == "+" & coding_gene_region$genomic_interval > 200000] <- 100
#%in% extracts rows where score is one of elements in the vector provided
sum(coding_gene_region$score == 100)

View(coding_gene_region)
class(coding_gene_region)

#add a new row w/ random values.
coding_gene_region[2,]
new_row <- data.frame("chr1", 100, 200, "TNSG00000278566", 0, "-", 100)
colnames(new_row) <- colnames(coding_gene_region)
class(new_row)
new_count_matrix <- rbind(coding_gene_region, new_row) #joining
tail(new_count_matrix)

#remove the score variable from dataframe.
test <- coding_gene_region[,-5] #- mean to remove
View(test)

#use apply to find range of each col, with na.rm and finite arguments set to TRUE
apply(coding_gene_region, 2, function(x) range(x, na.rm = T, finite = T)) #apply function converts output to characters.
#na.rm to remove any NAs missing values.


###loops exercises###

#write a loop to print colours in colours_vector with 4 characters
colours_vector <- c("red", "orange", "purple", "yellow", "pink", "blue")
for (i in colours_vector){
  if (nchar(i) == 4)
    print(i)
}

nchar("red")
#write a loop to print colours at even positions.
for (i in seq(2, length(colours_vector), by = 2)){print(colours_vector[i])}
#subset the colour by position

###functions exercises### 
#write a fuction that uses a for loop to calculate mean of a numeric vector of any length
#Don't use mean()

my_num <- c(5, 10, 15, 20, 25)
for (num in my_num){
  
}

#write a function that returns a colour name and the no of vowels it contains, but only for elements in colours_vector with fewer than 6 chars.

colours_vector <- c("red", "orange", "purple", "yellow", "pink", "blue")
get_vowel_count <- function(phrase) {
  count <- 0
  for (i in phrase) {
    if ((i == 'a') | (i == 'e') | (i == 'i') | (i == 'o') | (i == 'u')) {
      count <- count + 1 
    }   
  }
  output <- paste("Your colour has", count, "vowels in it!" )
  print(output)
}

for (i in colours_vector){
  if (nchar(i) < 6 )
    print(i)
get_vowel_count(colours_vector)
