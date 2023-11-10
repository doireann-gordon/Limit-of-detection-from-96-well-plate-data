# Open packages etc ####
library(tidyverse)
library(plyr)
library(magrittr)

rm(list = ls()) # clean the environment

layout <-
  read_csv("./sample layout.csv", show_col_types = F) # load the layout
df <-
  read_csv("./raw/sample result.csv", show_col_types = F) # load the data

tidy_plates <- function(df, time) {
  df %<>% tidyr::gather("col", "Reading", `1`:`12`) %>% # gather the columns into a longer format
    dplyr::rename(row = ...1, Reading.type = ...14) %>% # renaming
    tidyr::fill(row) %>% # fill the remaining rows (imported as NA) for the 1+nth reading type
    dplyr::mutate(Wells = paste0(row, ".", col, sep = "")) %>% # create well column from row and cols
    dplyr::select(-row, -col) # not needed
  # replace the Reading type values
  df %<>%
    dplyr::mutate(Reading.type = as.factor(case_when(Reading.type == "450" ~ "A450")))
  
  df %<>%
    dplyr::inner_join(layout, by = "Wells") %>% # merge layout file, keeping only the wells required in layout
    dplyr::select(-Wells) # remove the wells column
  if (nrow(df) == nrow(layout)) {
    # return error message if the number of rows is wrong
    print("Correct number of rows")
  } else{
    print("Incorrect number of rows")
  }
  
  # Count the replicates
  df %<>%
    dplyr::group_by_at(setdiff(names(.), c("Reading"))) %>% # group by everything except the Reading
    dplyr::mutate(Replicate = seq_along(Reading.type)) %>%
    dplyr::ungroup()
  # Convert data types if required ####
  print(colnames(df)) # print the colnames so it's easier for the user to define the factors, integers, and numerics
  factors <-
    c("Reading.type")
  integers <-
    c("Replicate")
  numerics <-
    c("Reading", "Sample.concs")
  convertData <-
    function(df, factors, integers, numerics) {
      # create the function
      if (length(factors) >= 1) {
        df[factors] <-
          lapply(df[factors], as.factor)
      } else{
        df <- df
      }
      if (length(numerics) >= 1) {
        df[numerics] <- sapply(df[numerics], as.numeric)
      } else{
        df <- df
      }
      if (length(integers) >= 1) {
        df[integers] <- lapply(df[integers], as.integer)
      } else{
        df <- df
      }
      return(df)
    }
  df %<>% convertData(factors, integers, numerics) # apply the function
  
  if (!is.na(time)) {
    df %<>% dplyr::mutate(Time = as.factor(time)) # add time, if applicable
  }
  return(df)
}

# if there are multiple timepoints, we can normalise the final timepoint to the first timepoint
# if there's no Time, let time = NA
df %<>% tidy_plates(time = NA)

#df <- dplyr::bind_rows(df1, df2) # if there are two datasets with different times, merge into one dataset
if("Time" %in% colnames(df)){ # if there is a time column
if (length(unique(df$Time) > 1)) { # if there are at least 2 times
  df %<>% # normalise the final reading (24 hours) to the baseline (0 hours) if necessary
    tidyr::pivot_wider(names_from = Time, values_from = Reading) %>%
    dplyr::mutate(Reading = `24 hours` / `0 hours`) %>% # to specify from input time observation
    dplyr::select(Sample.concs, Reading.type, Replicate, Reading)
}
}




df_raw <- df
df <- df_raw %>%
  dplyr::group_by_at(setdiff(names(.), c("Reading", "Replicate"))) %>% # group by everything except the Reading
  dplyr::summarise(
    Replicates = n(),
    sd = sd(Reading),
    Reading = mean(Reading)
  ) %>% # get the replicates, SD, and mean of Reading
  dplyr::ungroup()
df %<>% dplyr::arrange(Sample.concs) #nicer arrangement




save(df, df_raw, file = "./Results/file01-tidy results.RData")
# uncomment if wanted
#write.csv(df, file = "./Results/file01-tidy results.csv", row.names = F)
#write.csv(df_raw, file = "./Results/file01-tidy results raw.csv", row.names = F)
