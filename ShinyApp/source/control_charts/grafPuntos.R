#Function for graphing all points, used for the Shiny App
grafPuntos <- function(table){
library(ggplot2)
n = length(table[,1])

 plot = ggplot(table) + geom_point(mapping = aes(1:n,P1, color = 'P1')) +
  geom_point(mapping = aes(1:n,P2, color = 'P2')) +
  geom_point(mapping = aes(1:n,P3, color = 'P3')) +
  geom_point(mapping = aes(1:n,P4, color = 'P4')) +
  geom_point(mapping = aes(1:n,P5, color = 'P5')) +
  geom_point(mapping = aes(1:n,P6, color = 'P6')) +
  geom_point(mapping = aes(1:n,P7, color = 'P7')) +
  geom_point(mapping = aes(1:n,P8, color = 'P8')) +
  geom_point(mapping = aes(1:n,P9, color = 'P9')) +
  geom_point(mapping = aes(1:n,P10, color = 'P10')) +
  geom_point(mapping = aes(1:n,P11, color = 'P11')) +
  geom_point(mapping = aes(1:n,P12, color = 'P12')) +
  geom_point(mapping = aes(1:n,P13, color = 'P13')) +
  geom_point(mapping = aes(1:n,P14, color = 'P14')) +
  geom_point(mapping = aes(1:n,P15, color = 'P15')) +
  geom_point(mapping = aes(1:n,P16, color = 'P16')) +
  geom_point(mapping = aes(1:n,P17, color = 'P17')) +
  geom_point(mapping = aes(1:n,P18, color = 'P18')) +
  geom_point(mapping = aes(1:n,P19, color = 'P19')) +
  geom_line(mapping = aes(1:n,0.4, color = 'red')) +
  geom_line(mapping = aes(1:n,-0.4, color = 'red')) +
  labs(x = 'Sample', y = 'Milimeters', color = 'Points')
 return(plot)
}