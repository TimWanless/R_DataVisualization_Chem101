#how to make a radar chart
#installs packages needed
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggradar")
install.packages("scales")
#uses libraries
library(ggplot2)
library(ggiraphExtra)
library(ggradar)
library(dplyr)
library(scales)

#selects dataset
mtcars %>%
  #attribute rownames to a variable 
  add_rownames(var = "group") %>%
  #assign each variable -- car names -- to their related variables
  mutate_each(funs(rescale), -group) %>%
  #select which data to plot
  head(3) %>% select(1:10) -> mtcars_radar

#suppresses warnings
options(warn = -1)
ggradar(mtcars_radar)
