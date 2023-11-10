# Open packages etc ####
library(tidyverse)
library(plyr)
library(magrittr)
library(ggpubr)


# Clean the environemt
rm(list =  ls())
# Load the RData files
load(file = "./Results/file02-lod.RData")

# prepare for plotting by splitting if necessary
splitting <- function(df) {
  df %<>% dplyr::group_by(Reading.type) %>%
    dplyr::group_split()
  return(df)
}

# for labelling
max_conc <- round(max(df$Sample.concs), 2)
min_conc <- round(min(df$Sample.concs[df$Sample.concs > 0]), 2)
conc_label <-
  paste0("Range: ", min_conc, " - ", max_conc, " pg/ml", sep = "")

df %<>% splitting()
df_raw %<>% splitting()
#df <- df[[1]]

# Plotting ----------------------------------------------------------------
plotting <- function(df, include_lod, my_filename) {
  p <-
    ggplot(df, aes(x = Sample.concs, y = Reading)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = "y~x",
                colour = "black")  +
    geom_errorbar(aes(ymin = Reading - sd, ymax = Reading + sd),
                  width = 0.5,
                  linewidth = 0.5)
  p <- p + ylim(min(df$Reading) * 0.9, max(df$Reading) * 1.2)
    p <-
      p + labs(
        x = "Concentration (pg/ml)",
        y = "Reading",
        title = "Your title",
        subtitle = "Your subtitle" # sometimes I like to use paste0 to label specific conditions
      )
    
  # labels: LoD at the top, line equation in the middle, range on the right
  # lod label
  if (include_lod == TRUE) {
    p <- p +
      geom_text(
      data = dplyr::distinct(df,
                             Annotate),
      aes(
        x = 0,
        y = max(df$Reading) * 1.2,
        label = Annotate
      ),
      hjust = 0,
      vjust = 1,
      inherit.aes = TRUE,
      parse = FALSE,
      size = 3
    )
  }
  # line equation label
  p <- p +
    ggpmisc::stat_poly_eq(
    data = df,
    formula = y ~ x,
    aes(label = paste(after_stat(eq.label), ..rr.label.., sep = "*\",\"~~")),
    parse = TRUE,
    label.x = 0.05,
    label.y = 0.9,
    size = 3,
    hjust = 0,
    vjust = 1
  )
  # Range label
  p <- p + annotate(
    "label",
    x = max_conc,
    y = max(df$Reading) * 1.2,
    hjust = 1,
    vjust = 1,
    label = conc_label,
    size = 3
  )
    p <-
    p + theme_classic() + theme(
      strip.background = element_rect(fill = "white"),
      plot.subtitle = element_text(size = 10),
      panel.background = element_rect(colour = "black")
    )
  
  print(p)
  ggsave(
    filename = my_filename,
    plot = p,
    width = 15,
    height = 10,
    units = "cm"
  )
  print("Done")
  return(p)
}
# test in one
plotting(df = df[[1]],
         include_lod = TRUE,
         my_filename = "./Results/lod plot.png")
# all (if there are multiple)
# for (i in 1:length(df)) {
#   plotting(df = df[[i]],
#            include_lod = TRUE,
#            my_filename = paste0("./Results/lod plot", i, ".png")) #
# } 
