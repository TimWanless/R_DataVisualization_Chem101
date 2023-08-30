install.packages("tidyverse")

library(tidyverse)

mpg

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class, size = class))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

mtcars
?facet_double
facet
ggplot(mtcars, aes(x = wt, y = mpg, shape = disp)) +
  geom_point()
