#tidying up RNAseq data on mouse T cells.
#knockin, KO, +/-virus, 3 biological repeats (n=12)
#2 files.

library(tidyverse)
library(biomaRt)
library(pheatmap)

#tidy count file - 3 cols
sample_table <- read_tsv("data/obds_sampletable.tsv") 
count_table <- read_tsv("data/obds_countstable.tsv.gz")
View(sample_table) 
#5cols
#to remove library_layout & read_count
#sample_title has 3 info

View(count_table)
#all samples and geneid

#gathers multiple cols into key-value pairs.
processed_counttable <- count_table %>%
  pivot_longer(-Geneid, names_to = "sample", values_to = "count")
View(processed_counttable) # -Geneid will keep that col.
#note that 1 gene can occur in several samples
#now have 3cols 

#join with gene info to get mgi_symbol
listMarts()
ensembl <- useMart("ensembl") #connecting to specific BioMart database
datasets <- listDatasets(ensembl) #list dataset
head(datasets)
View(datasets) #copying the full name of your specie.
ensembl <- useMart("ensembl",dataset="mmusculus_gene_ensembl") #changing to your specice.
View(ensembl)

#getBM()has 3 arguments: filters, attributes, values.
#Filters define a restriction on the query. e.g,to restrict the output to all genes located on the human X chromosome then the filter chromosome_name can be used with value ‘X’.
filters <- listFilters(ensembl)
filters[1:5,]
View(filters)

#Attributes define the values to retrieve. e.g.to retrieve the gene symbols or chr coordinates. 
attributes = listAttributes(ensembl)
attributes[1:5,]
View(attributes)
#matching
matching <- getBM(attributes = c("ensembl_gene_id", "mgi_symbol"),
      values = unique(processed_counttable$Geneid),
      mart = ensembl)
#remove duplicates
View(matching)
      
#joining tables to match geneid note different naming btw 2 tables.
processed_counttable <- processed_counttable %>% 
  left_join(matching, by = c("Geneid" = "ensembl_gene_id"))

#tidy metadata file: 1 var per col & remove species and library_layout cols
#separating by cell type, KO/KI, replicate
View(sample_table)

processed_sample_table <- sample_table %>% 
  separate(sample_title, c("gene", "genotype", "cell type", "replicates"), sep = "_") %>%
  unite("Genotype", gene, genotype, sep = "_")  %>% 
  dplyr::select(-library_layout, -read_count)
View(processed_sample_table)
#remove cols

#joining 2 tables
processed_joined <- processed_counttable %>% 
  left_join(processed_sample_table, by = c("sample" = "Sample_accession"))
View(processed_joined)

#calculate CPM (group_by() and mutate()) #log2
calculated <- processed_joined %>% 
  group_by(sample) %>% 
  mutate(total_count_per_sample = sum(count)) %>% 
  mutate(total_count_in_million = total_count_per_sample / 1000000)  %>% 
  mutate(CPM= count / total_count_in_million) %>% 
  mutate(log_CPM = log2(CPM+1))
View(calculated)

#add metadata to table w/ counts & gene info
