#loads libraries
library(ggplot2)
library(tidyverse)
#views data
view(faithful)
attach(faithful)

view(mpg)
attach(mpg)

views(mtcars)
attach(mtcars)

views(diamonds)
attach(diamonds)

#scatter plots

ggplot(data=mpg)+
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(faithful)+
  geom_point(mapping = aes(x=waiting, y=eruptions))

ggplot(faithful, aes(x=waiting, y=eruptions))+
  geom_point()