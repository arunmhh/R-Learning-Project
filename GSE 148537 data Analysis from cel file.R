library(tidyverse)
library(affy)
library(GEOquery)
# fetch supplimentary files
getGEOSuppFiles("GSE148537")
getwd()
# untar files
untar("GSE148537/GSE148537_RAW.tar",exdir = "Data/")
 
# read .cel files

raw.data <- ReadAffy(celfile.path = "Data/")

# Normalize data
normalized.data <- rma(raw.data)

# get expression data
norm.exp <- exprs(normalized.data)
# convert into a data frame
norm.data <- as.data.frame(norm.exp)
# map probids to gene symbol
gse <- getGEO("GSE148537",GSEMatrix = TRUE)

# fetch feature data to get ID- gene symbol mapping
feature.data <- gse$GSE148537_series_matrix.txt.gz@featureData@data
# sub set the feature data means select column
subFeature.data <- feature.data[,c(1,11)]
# merge data using the column ids
norm.data %>% 
  rownames_to_column(var = "ID") %>% 
  inner_join(.,subFeature.data, by = "ID")-> Newdata
write.csv(Newdata,"GSE148537.txt")

# differential expression analysis using limma
library(limma)
pData(normalized.data)
groups <- c("MCF", "MCF", "SHSP","SHSP")
design <- model.matrix(~factor(groups))
colnames(design) <- c("SHSP","SHSPvsMCF")
fit <- lmFit(normalized.data,design)
fit <- eBayes(fit)
options(digits =2)
res <- topTable(fit,coef = 1,number = Inf,adjust.method = "none")

res %>% 
  rownames_to_column(var = "ID") %>% 
  inner_join(.,subFeature.data, by = "ID") -> Diffexp1
write.csv(Diffexp1,"FinalDEGS.txt")
colSums(is.na(Diffexp1))






