#Librerias a utilizar
install.packages(c("tidyr","lubridate"),dependencies = TRUE) #limpieza de datos
install.packages("dplyr",dependencies = TRUE)
install.packages("openintro",dependencies = TRUE)
install.packages(c("rpart","aplpack","corrplot","sm"),dependencies = TRUE)
install.packages(c("ggplot2"), dependencies = TRUE)

library(rpart)
library(aplpack)
library(corrplot)
library(sm)

library(dplyr)
library(openintro)
library(tidyr)
library(lubridate)
library(ggplot2)

data("mlb")
head(mlb, 20)

#Seleccionar todos los jugadores en la posicion de pitcher, pero unicamente el nombre y salario

mlb %>%
  filter(position == "Pitcher" ) %>%
  select(player, salary)


#Calcular la cantidad jugadores por posicion, el salario promedio por posicion, 
#el salario maximo y minimo, asi como el total
# dejar los valores en las siguientes variables: cant,mean_salary, min_salary,max_salary,total_salary

mlb %>%
  group_by(position) %>%
  summarise(cant=n(), mean_salary=mean(salary),min_salary=min(salary),max_salary=max(salary),total_salary=sum(salary))


#Calcular la cantidad de jugadores por equipo, el monto total de planilla del equipo y el salario promedio

mlb %>% 
  group_by(team) %>% 
  summarise(cantidad_por_equipo=n(), to=sum(salary), prom=mean(salary))

#Calcular la cantidad de jugadores por equipo y posicion, el monto total de planilla del equipo y el salario promedio

mlb %>% 
  group_by(team,position) %>% 
  summarise(cant=n(), tot=sum(salary), prom=mean(salary))


data("births")
head(births, 20)


#Calcular la diferencia de edad entre padre y madre en una nueva columna dif_parents y agreguelo en el dataframe

mutate(births, dif_parents=f_age - m_age)

births %>% 
  filter(!is.na(visits)) %>% 
  mutate(dif_parents=f_age - m_age)

#Contar el total de nacimientos por cada semana, tambien la cantidad por sexo bebe, tambien por si son fumadores o no

births %>% 
  group_by(weeks, sex_baby, smoke) %>% 
  summarise(n=n())

#Contar la cantidad de nacimiento por edad padre y de la madre
births %>% 
  filter(!is.na(f_age) & !is.na(m_age)) %>% 
  group_by(f_age, m_age) %>% 
  summarise(cantidad_nacimientos=n())


#Genere el total de visitas y el peso promedio para los nacimientos por genero y estado prematuro
births %>% 
  group_by(sex_baby, premature) %>% 
  summarise(visitas=sum(visits),p_p=mean(weight))


#Cuente la cantidad de nacimientos por semana de acuerdo a si son fumadores o no fumadores
births %>% 
  group_by(smoke, weeks) %>%
  summarise(c_n=n())

print(n=100,births %>% 
        group_by(smoke, weeks) %>%
        summarise(c_n=n()))


#Calcular el peso promedio ganado por semana, sexo bebe y por prematuros
births %>% 
  group_by(gained, sex_baby, premature) %>%
  summarise(p_p=mean(gained, na.rm = TRUE))

#--------------------------------------------------------PLOTS---------------------------------------


corrMat <- cor(mtcars)
corrplot(corrMat,method="ellipse")
corrplot(corrMat,method="square")
corrplot(corrMat,method="circle")
corrplot(corrMat,method="pie")


#plot de una variable en relacion a otra
plot(x=mtcars$wt, y=mtcars$hp)


#plot con ajuste de titulos
plot(x=mtcars$wt, y=mtcars$hp,xlab = "Desplazamiento",ylab = "Caballos Fuerza")

#plot con ajuste de titulos y cambio color de los puntos
plot(x=mtcars$mpg, y=mtcars$hp,xlab = "Desplazamiento",ylab = "Caballos Fuerza",col="red")


#******************** Ejemplos con dotchar() **********************
dotchart(mtcars$mpg,labels=row.names(mtcars),cex =.7,
         main="Gas Milage for Car Models",
         xlab="Miles Per Gallon")


x <- mtcars[order(mtcars$mpg),] # sort by mpg
x$cyl <- factor(x$cyl) # it must be a factor
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
         main="Gas Milage for Car Models\ngrouped by cylinder",
         xlab="Miles Per Gallon", gcolor="black", color=x$color) 



#******************** Ejemplos con ggplot2 *****************************

mtcars
str(mtcars)

#Grafico base
plot(mtcars$wt ~ mtcars$mpg)

#Grafico inicial de peso y millas por galon
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() 



#Grafico base
hist(mtcars$wt)

#Grafico de histogram vs ggplot2 geom_histogram
ggplot(mtcars, aes(x=wt)) +
  geom_histogram(bins = 5)

#Colores en ggplot2: https://r-graph-gallery.com/ggplot2-color.html
ggplot(mtcars, aes(x=wt, fill = I("pink"), col = I("steelblue"))) +
  geom_histogram(bins = 5)

ggplot(mtcars, aes(x=wt)) +
  geom_histogram(bins = 5,fill="green",col = "black") +
  ggtitle("Detalle de vehiculos por peso") + 
  xlab("peso") +
  ylab("total")


#  geom_point()
class(mtcars$wt)
class(mtcars$mpg)
class(mtcars$disp)

class(mtcars$am)
table(factor(mtcars$am))


# geom_point 3 variables
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()


#  geom_point() 4 variables
ggplot(mtcars, aes(x = wt, y = mpg, size = hp, color = disp)) +
  geom_point()

# geom_point() 5 variables
ggplot(mtcars, aes(x = wt, y = mpg, size = disp, color = disp, shape = factor(cyl))) +
  geom_point()


ggplot(mtcars,aes(x=mpg,y=qsec,col=factor(cyl),shape=factor(am))) +
  geom_point()

# Agregar un ajuste por (hp/wt) al size
ggplot(mtcars,aes(x=mpg,y=qsec,col=factor(cyl),shape=factor(am),size=(hp/wt))) +
  geom_point()
