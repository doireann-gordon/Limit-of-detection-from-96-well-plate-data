# Open packages etc ####
library(tidyverse)
library(plyr)
library(magrittr)

rm(list = ls()) # clean the environment

wells.cols <- paste0(rep(LETTERS[1:8], times = 12), ".", rep(1:12, each = 8))
wells.rows <- paste0(rep(LETTERS[1:8], each = 12), ".", rep(1:12, times = 8))

# use wells.cols if you filled the plate column by colum (down) or wells.rows if you filled by row (across)

# create a range of concentrations
my_concs <- function(my_length, my_dilution, highest_conc){
x <- vector() # creates empty vector
x[1] <- highest_conc # first index of vector
for(i in 2:my_length){
  x[i] <- x[i-1]/my_dilution
}
x %<>% rev() # reverse order from low to high
return(x)
}
concs <- my_concs(
  my_length = 8,# how many concentrations (excluding 0)
  my_dilution = 2,# dilution factor
  highest_conc = 500# highest concentration
)
samples <- c(rep(0, 10), rep(concs, each = 3)) # add 0s (there must be 10 for LoD) and write out each conc 3 times
samples

layout <- tibble(Sample.concs = samples,
                 Wells = wells.cols[1:length(samples)]) # the number of wells is the number of samples

write.csv(layout, "sample layout.csv", row.names = F)


# creating fake data
set.seed(123)

# Define the parameters of the line
slope <- 2
intercept <- 20

# Generate x values
x_values <- c(samples, rep(NA,96-length(samples)))  # need to add NAs for the empty wells

# Generate y values using the linear equation
y_values <- slope * x_values + intercept

# Add some random noise to y values to make them random
y_values <- y_values + rnorm(length(x_values), mean = 0, sd = 100)

fake <- data.frame(matrix(y_values, ncol = length(y_values) / 8, byrow = F)) 
rownames(fake) <- LETTERS[1:8]
colnames(fake) <- c(1:12)

fake %>% write.csv(file = "./raw/fake aid.csv")

