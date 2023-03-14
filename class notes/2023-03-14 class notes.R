#jkoshea class notes
#unit 4 day 1
#2023-03-14


library(tidyverse)

data1 = data.frame(ID= c(1,2), 
                   X1= c("a1", "a2"))
data2 = data.frame(ID= c(2, 3),
                   X2= c("b1", "b2"))
data2


#left_join
data12= left_join(data1, data2)   #can also add a by= after data2 to join by a variable
data12 = data1 %>%
  left_join(data2, by="ID")
data12
#above are the two different ways to do a join- through just one line or by piping

data12 = data1 %>%
  right_join(data2, by="ID")
data12


#inner join
data12=data1 %>%
  inner_join(data2)
data12


#full_join
data12=data1%>%
  full_join(data2)
data12


#semi_join= a filtering join, so result will be smaller, keep IF there is overlap
data12=data1%>%
  semi_join(data2)
data12


#anti_join, when you want to know what is missing 
data12=data1%>%
  anti_join(data2)
data12


#wider vs. longer

survey=data.frame(quadrat_id=c(101, 102, 103, 104), 
                  barnacle_n=c(2, 11, 8, 27), 
                  chiton_n=c(1, 0, 0, 2), 
                  mussel_n=c(0, 1, 1, 4))
survey

long= survey%>%
  pivot_longer(cols=c("barnacle_n", "chiton_n", "mussel_n"), 
               names_to = "taxon", 
               values_to = "counts")
long

wide = long%>%
  pivot_wider(names_from = taxon, 
              values_from = counts)
wide



#class exercise 1.2 - creating plots

ggplot(data=wide, aes(x=quadrat_id, y= barnacle_n, chiton_n, mussel_n, color=taxon))
#fail booooo


#from class, add y=lab 
ggplot(data=wide)+
  geom_point(aes(x=quadrat_id, y=barnacle_n, color="red"))+
  geom_point(aes(x=quadrat_id, y=chiton_n, color="blue"))+
  geom_point(aes(x=quadrat_id, y=mussel_n, color="green"))


ggplot(data=long)



