library(ggplot2)
library(tidyverse)

physics<- data.frame(
  Number = c(1,2,3,4,5,6,7,8),
  Quiz_avg = c(8.3,10.3,13.0,16.2,15.2, 12.0, 17.0, 17.9)
)

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line()

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line()+
  geom_point(size=4,color="blue")

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line(color="red")+
  geom_point()

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line(arrow=arrow())+
  geom_point()

physics2 <- physics
physics$Number <- as.factor(physics2$Number)

ggplot(physics2, aes(x=Number, y=Quiz_avg, group=1))+
  geom_line()

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line()+
  ylim(0, max(physics$Quiz_avg))
ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line()+
  expand_limits(y=0)

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line()+
  geom_area()

ggplot(physics, aes(x=Number, y=Quiz_avg))+
  geom_line()+
  geom_area(fill="blue", alpha=.2)

physics$HW_avg = c(3.6, 5.5,7.6, 8.1, 6.3, 2.9, 9.9, 9.2)

ggplot(physics)+
  geom_line(aes(x=Number, y=Quiz_avg))+
  geom_line(aes(x=Number, y=HW_avg))

ggplot(physics)+
  geom_line(aes(x=Number, y=Quiz_avg, color="red"))+
  geom_line(aes(x=Number, y=HW_avg, color="blue"))

ggplot(physics)+
  geom_line(aes(x=Number, y=Quiz_avg))+
  geom_line(aes(x=Number, y=HW_avg))

physics$Quiz_perc <- physics$Quiz_avg/20
physics$HW_perc <- physics$HW_avg/10

ggplot(physics)+
  geom_line(aes(x=Number, y=Quiz_perc))+
  geom_line(aes(x=Number, y=HW_perc))

physics$Class = c(1, 1, 1, 1, 1, 1, 1, 1)

physics_class2 <- data.frame(
  Number = c(1, 2, 3, 4, 5, 6, 7, 8),
  Quiz_avg = c(18.2, 15.8, 17.1,17.3, 18.9, 14.0, 19.3, 19.5),
  HW_avg = c(9.8,7.9, 8.5, 8.8, 9.1, 6.9, 9.3, 9.2),
  Class = c(2, 2, 2, 2 ,2,2,2,2)
)

physics_class2$Quiz_perc <- physics_class2$Quiz_avg/20
physics_class2$HW_perc <- physics_class2$HW_avg/10

physics_new <- rbind(physics, physics_class2)

physics_new$Class <- as.factor(physics_new$Class)

view(physics_new)

ggplot(physics_new, aes(x=Number, y=Quiz_perc))+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(physics_new, aes(x=Number, y=Quiz_perc))+
  geom_point()+
  geom_smooth(span=1)
?date
