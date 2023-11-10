library(tidyverse)
library(magrittr)
library(varhandle)
library(broom)
library(plyr)
library(ggpubr)

# Clean the environemt
rm(list =  ls())
# Load the RData files
load(file = "./Results/file01-tidy results.RData")

# Split up based on each group you want to create a separate LoD for e.g., Reading type
# This allows calculation of LoD for each group - nice and tidy
splitting <- function(df) {
  df %<>% dplyr::group_by(Reading.type) %>% dplyr::group_split() 
  return(df)
}
df <- splitting(df)
df <- lapply(df, function(x){
  if(sum(is.na(x$Reading)) > 0 ){
    x <- NULL
    print("Overflowed - dataframe is NULL")
  } else {
    print("No overflow")
  }
  return(x)
})
df <- df[(!sapply(df, is.null))] # remove null dfs

# LoD function ####
lod <- function(df, is_absolute) {
  # get the LoB ####
  LOB <-
    df$Reading[df$Sample.concs == 0] + (3 * df$sd[df$Sample.concs == 0])
  
  # Fit the linear regression
  lm_fit <-
    lm(Reading ~ Sample.concs, data = df) # Linear regression of reading against GzmB concentration
  lm_fit %<>% stats::coef() # extract coefficients
  Intercept <- unname(lm_fit["(Intercept)"])
  Slope <- unname(lm_fit["Sample.concs"])
  LOD <- (LOB - Intercept) /
    Slope
  # Add to df
  df %<>% dplyr::mutate(LOB.pgml = LOB) %>% dplyr::mutate(LOD.pgml = LOD)
  # Add pM
  df %<>% dplyr::mutate(LOD.pM = LOD / 32)
  # absolute value of LoD (LoDs aren't negative)
  if(is_absolute == TRUE){
    df %<>% dplyr::mutate(LOD.pgml = abs(LOD.pgml)) %>% dplyr::mutate(LOD.pM = abs(LOD.pM)) %>% dplyr::mutate(Note = c("LoD is absolute values"))
    print("Returning absolute values of LoD")
  }
  
  # Generate a column with a text that can be used to annotate a graph
  df %<>% dplyr::mutate(Annotate = paste0(
    "LoD = ",
    sprintf("%.2f", LOD.pgml),
    " pg/ml = ",
    sprintf("%.2f", LOD.pM),
    " pM",
    sep = ""
  ))
  return(df)
}
# Calculate the LoD ####
df %<>% lapply(., lod, is_absolute = TRUE) # is_absolute specifies if you return the absolute LoD value or not
df %<>% dplyr::bind_rows()


# create an LoD only file and save it ####
df_lod <- df %>% dplyr::select(-Sample.concs, -Replicates, -Reading, -sd, -Annotate) %>% unique()

write.csv(df, "./Results/file02-lod.csv", row.names = F)
write.csv(df_lod, "./Results/file02-lod only.csv", row.names = F)
# keeping df as name rather than df_lod because dataframes saved into different file and easier for plotting
save(df, df_raw, file = "./Results/file02-lod.RData")

