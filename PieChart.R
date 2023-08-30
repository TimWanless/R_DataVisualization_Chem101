#' This installs packages
library(ggplot2)
library(tidyverse)
#'
#'Loads data
#'Creates a dataframe
LA_enroll <- data.frame(
  major=c("Education","Psychology","CompSci","Business","Other")
  value=c(31, 19, 16, 14, 10)
)
view(LA_enroll)
head(LA_enroll)
#'
#'This views the diamond data set in ggplot2
view(diamonds)
head(diamonds)
#'
#'This creates a ideal diamond subset
diamonds_ideal <- select(filter(diamonds, cut=="Ideal"),c(carat:price))
view(diamonds_ideal)
#'
#'Bar chart using LA_enroll dataset
#'
ggplot(data = LA_enroll, mapping = aes(x=major, y=value))+
  geom_bar(stat = "identity")
#'
#'This fills the bars of the plot.
ggplot(data = LA_enroll, mapping = aes(x=major, y=value,fill=major))+
  geom_bar(stat = "identity")
#'
#'This creates a stacked bar plot.
#'
ggplot(LA_enroll, aes(x="", y=value, fill=major))+
  geom_bar(stat = "identity")
#'
#'This creates a stacked bar graph that divides cut and clarity.
ggplot(diamonds, aes(x=cut, fill=clarity))+
  geom_bar()
#'
#'This creates a stacked bar plot that shows a proportion.
ggplot(diamonds, aes(x=cut, fill=clarity))+
  geom_bar(position = "fill")
#'
#'This makes a pie chart using the LA enroll data set.
#'
ggplot(LA_enroll, aes(x="", y=value, fill=major))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)
#'
#'This makes a pie chart using the diamond data set.
#'
ggplot(diamonds, aes(x=cut,y=..prop.., fill= cut))+
  geom_bar(width = 1)+
  coord_polar(theta = "x")
#'
#'This adds a blue aesthetic to a pie chart using LA school data.
ggplot(LA_enroll, aes(x="",y=value, fill= major))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)+
  scale_fill_brewer(palette = "Blues")
#'
#'Adds library package.
#'
library(waffle)
#'
#'
waffle(c("Education"=39, "Psychology"=19, "CompSci"=16, "Business"=14, "Other"=10), rows = 10)