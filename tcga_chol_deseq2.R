library(tidyverse) 
library(TCGAbiolinks) 
library(maftools) 
library(pheatmap) 
library(SummarizedExperiment) 
library(GDCRNATools) 
library(DESeq2)
library(biomaRt)
### get gdc project 
gdcprojects <- getGDCprojects() 
getProjectSummary("TCGA-CHOL") 
## build query 
query_TCGA <- GDCquery(project = "TCGA-CHOL", 
                      data.category = "Transcriptome Profiling") 
output_query <- getResults(query_TCGA)  
## fileter the data with more parameters  
query_TCGA <- GDCquery(project = "TCGA-CHOL",  
                       data.category = "Transcriptome Profiling",  
                       experimental.strategy = "RNA-Seq",  
                       workflow.type = "STAR - Counts",   
                       access = "open") 
getResults(query_TCGA)  
### download filter data  
GDCdownload(query_TCGA) 
## prepare data 
TCGA_CHOL_data <- GDCprepare(query_TCGA,summarizedExperiment = TRUE) 
chol_data <- assay(TCGA_CHOL_data, 'fpkm_unstrand') ## Matrix
# as data frame
chol_counts <- as.data.frame(chol_data)
### meta data

df_meta = data.frame(colData=TCGA_CHOL_data@colData)
meta_fit <- df_meta %>% select(colData.barcode,
                               colData.sample_type,
                               colData.tissue_type)

meta_fit$colData.sample_type <- as.factor(meta_fit$colData.sample_type)

summary(meta_fit)

all(colnames(chol_counts) == rownames(meta_fit))

### Differential expression analysis

dds <- DESeqDataSetFromMatrix(countData = round(chol_counts),
                              colData = meta_fit,
                              design = ~ colData.sample_type)

# pre-filtering: removing rows with low gene counts
# keeping rows that have at least 10 reads total
keep <- rowSums(counts(dds)) >= 10
ddsa <- dds[keep,]

# set the factor level
ddsa$colData.sample_type <- relevel(ddsa$colData.sample_type, 
                                    ref = "Solid Tissue Normal")
### run DEseq
ddEG <- DESeq(ddsa)
DEGs <- results(ddEG)

# Explore Results ----------------

summary(DEGs)

DEGs0.01 <- results(ddEG, alpha = 0.01)
summary(DEGs0.01)
#####

resultsNames(DEGs0.01)


plotMA(DEGs0.01, ylim=c(-2,2))

##### convert rownames in first column
library(data.table)
setDT(loglfc,keep.rownames = "ensemble.ids")

#### conver ensenble id into gene
listEnsembl()
ensemble <- useEnsembl(biomart = "genes")
datasets <- listDatasets(ensemble)
ensembl.con <- useMart("ensembl", dataset = 'hsapiens_gene_ensembl')


attr <- listAttributes(ensembl.con)
filters <- listFilters(ensembl.con)
getBM(attributes = c('ensembl_gene_id','external_gene_name'),
      filters = "ensembl_gene_id",
      values = loglfc$ensemble.ids,
      mart = ensembl.con)

## Annotalbles
library(annotables)
grch38 %>%
  filter(ensgene %in% enids$ensemble_ids)

###########################################################
write.csv(loglfc, "Chol_DEGslfc.csv")

