library(plumber)
r <- plumb("C:\\Users\\ale23\\OneDrive\\Escritorio\\CUC\\segundoCuatri\\mineria\\scripsR\\api_Stats.R")
r$run(port = 8000)


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rpart","aplpack","corrplot","sm","gapminder","openintro",
              "dplyr","ggplot2","readr","lubridate","tidyr", "forecast",
              "corrplot", "plotly", "rmarkdown","tinytex")
ipak(packages)


employees <- 
  read_csv("C:\\Users\\ale23\\OneDrive\\Escritorio\\CUC\\segundoCuatri\\mineria\\scripsR\\Employees_dataset.csv")

class(employees)
dim(employees)
names(employees)
str(employees)
glimpse(employees)


#verificamos nulos
any(is.na(employees)) #false
summary(employees)

#-----------------------------------------------Corelaciones--------------------------------------------------------
employees_numeric <- select(employees,`MonthlySalary`:`OvertimeHours`)
employees_numeric

corrMat <- cor(employees_numeric)
corrplot(corrMat,method="ellipse")

#Corelaciones encontradas


#Annual Salary --> Monthly Salary
#Overtime Hours --> Monthly Salary
#Overtime Hours --> Annual Salary
#Sick Leaves --> Job Rate

#----------------------------------------------Boxplot------------------------------------------------------------

boxplot(`AnnualSalary` ~ `MonthlySalary`, data = employees_numeric,col = "lightblue")
title("Relacion de salario anual y salario mensual")

boxplot(`OvertimeHours` ~ `MonthlySalary`, data = employees_numeric,col = "lightblue")
title("Relacion de horas extras trabajadas y salario mensual")

boxplot(`OvertimeHours` ~ `AnnualSalary`, data = employees_numeric,col = "lightblue")
title("Relacion de horas extras trabajadas y salario anual")

boxplot(`SickLeaves` ~ `JobRate`, data = employees_numeric,col = "lightblue")
title("Relacion de bajas por enfermedad y tasa de trabajo")

#-----------------------------------------------Algoritmo--------------------------------------------
# Suponiendo que ya tienes tus datos cargados en un data frame llamado 'employee_data'
# Puedes ajustar un modelo de regresión lineal para predecir el salario mensual:

# Ajustar el modelo de regresión lineal múltiple
model_projection <- lm(MonthlySalary ~ Years + Gender, data = employees)

# Resumen del modelo
summary(model_projection)

# Crear una nueva columna para las predicciones
employees$Projected_Salary <- predict(model_projection, employees)

# Visualizar la proyección
ggplot(employees, aes(x = Years, y = Projected_Salary, color = Gender)) +
  geom_point(aes(y = MonthlySalary), alpha = 0.5) +  # Puntos originales
  geom_line() +  # Línea de proyección
  labs(title = "Proyección del Salario Mensual según Años de Experiencia y Género",
       x = "Años de Experiencia",
       y = "Salario Mensual Proyectado (USD)") +
  theme_minimal()


#------------------------------------------------------Hipotesis-------------------------------------------
#¿Existe una diferencia significativa en el salario mensual
#entre empleados de diferentes géneros en la misma posición y departamento?

# Agrupar datos por Género, Posición y Departamento, y calcular la media del salario mensual
salary_summary <- employees %>%
  group_by(Gender, Department) %>%
  summarise(Average_Salary = mean(MonthlySalary, na.rm = TRUE)) %>%
  arrange(Department, Gender)

# Crear gráfico de barras interactivo con plotly
plot <- plot_ly(salary_summary, 
                x = ~Department, 
                y = ~Average_Salary, 
                color = ~Gender, 
                type = 'bar', 
                barmode = 'group',
                text = ~paste("Department: ", Department, "<br>Gender: ", Gender, "<br>Average Salary: $", round(Average_Salary, 2)),
                hoverinfo = 'text') %>%
  layout(title = 'Salario promedio mensual por género, posición y departamento',
         xaxis = list(title = 'Departamento'),
         yaxis = list(title = 'Salario Promedio (USD)'),
         barmode = 'group')

htmlwidgets::saveWidget(plot,"temp_plot.html")
browseURL("temp_plot.html")

#--------------------------------------------------------------------------------------------------------

#¿Cómo afecta el número de horas extras trabajadas al rendimiento (Job Rate)
#de los empleados en diferentes departamentos?


# Agrupar por departamento y calcular estadísticas resumen
dept_performance <- employees %>%
  group_by(Department) %>%
  summarize(
    avg_overtime = mean(OvertimeHours, na.rm = TRUE),
    avg_job_rate = mean(JobRate, na.rm = TRUE)
  )

# Crear un gráfico de dispersión con plotly
fig <- plot_ly(
  data = employees,
  x = ~OvertimeHours,
  y = ~JobRate,
  color = ~Department,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste("Departamento:", Department, "<br>Horas Extras:",OvertimeHours, "<br>Rendimiento:", JobRate)
) %>%
  layout(
    title = "Relación entre Horas Extras y Rendimiento por Departamento",
    xaxis = list(title = "Horas Extras"),
    yaxis = list(title = "Rendimiento (Job Rate)"),
    legend = list(title = "Departamento")
  )
htmlwidgets::saveWidget(fig,"temp_plot.html")
browseURL("temp_plot.html")


#--------------------------------------------------------------------------------------------------------
#¿Cuál es el impacto del número de años de servicio
#en la probabilidad de que un empleado tome licencias no remuneradas?

# Crear una variable binaria para indicar si el empleado tomó licencias no remuneradas
employees_data <- employees %>%
  mutate(Took_Unpaid_Leave = ifelse(UnpaidLeaves > 0, 1, 0))

# Agrupar por años de servicio y calcular la tasa de empleados con licencias no remuneradas
years_leave_prob <- employees_data %>%
  group_by(Years) %>%
  summarize(
    num_employees = n(),
    num_with_leave = sum(Took_Unpaid_Leave),
    leave_prob = num_with_leave / num_employees
  )

# Crear un gráfico de línea con plotly
fig <- plot_ly(
  data = years_leave_prob,
  x = ~Years,
  y = ~leave_prob,
  type = 'scatter',
  mode = 'lines+markers',
  marker = list(size = 8),
  line = list(width = 2),
  text = ~paste("Años de Servicio:", Years, "<br>Probabilidad de Licencia No Remunerada:", round(leave_prob, 2))
) %>%
  layout(
    title = "Impacto de Años de Servicio en la Probabilidad de Tomar Licencias No Remuneradas",
    xaxis = list(title = "Años de Servicio"),
    yaxis = list(title = "Probabilidad de Licencia No Remunerada")
  )

htmlwidgets::saveWidget(fig,"temp_plot.html")
browseURL("temp_plot.html")

#---------------------------------------------------------------------------------------------------
#¿Los empleados en ciertos países  tienen más probabilidades de tomar licencias por enfermedad que en otros?

# Crear una variable binaria para indicar si el empleado tomó licencias por enfermedad
employee_data <- employees %>%
  mutate(Took_Sick_Leave = ifelse(SickLeaves > 0, 1, 0))

# Agrupar por país y calcular la tasa de empleados con licencias por enfermedad
country_sick_leave_prob <- employee_data %>%
  group_by(Country) %>%
  summarize(
    num_employees = n(),
    num_with_sick_leave = sum(Took_Sick_Leave),
    sick_leave_prob = num_with_sick_leave / num_employees
  ) %>%
  arrange(desc(sick_leave_prob))

# Crear un gráfico de barras con plotly
fig_country <- plot_ly(
  data = country_sick_leave_prob,
  x = ~Country,
  y = ~sick_leave_prob,
  type = 'bar',
  text = ~paste("País:", Country, "<br>Probabilidad de Licencia por Enfermedad:", round(sick_leave_prob, 2)),
  marker = list(color = 'rgba(55, 128, 191, 0.7)', line = list(color = 'rgba(55, 128, 191, 1.0)', width = 1))
) %>%
  layout(
    title = "Probabilidad de Licencias por Enfermedad por País",
    xaxis = list(title = "País", tickangle = -45),
    yaxis = list(title = "Probabilidad de Licencia por Enfermedad")
  )

htmlwidgets::saveWidget(fig_country,"temp_plot.html")
browseURL("temp_plot.html")

# Agrupar por centro y calcular la tasa de empleados con licencias por enfermedad
center_sick_leave_prob <- employees %>%
  group_by(Center) %>%
  summarize(
    num_employees = n(),
    num_with_sick_leave = sum(SickLeaves),
    sick_leave_prob = num_with_sick_leave / num_employees
  ) %>%
  arrange(desc(sick_leave_prob))

# Crear un gráfico de barras con plotly
fig_center <- plot_ly(
  data = center_sick_leave_prob,
  x = ~Center,
  y = ~sick_leave_prob,
  type = 'bar',
  text = ~paste("Centro:", Center, "<br>Probabilidad de Licencia por Enfermedad:", round(sick_leave_prob, 2)),
  marker = list(color = 'rgba(219, 64, 82, 0.7)', line = list(color = 'rgba(219, 64, 82, 1.0)', width = 1))
) %>%
  layout(
    title = "Probabilidad de Licencias por Enfermedad por Centro",
    xaxis = list(title = "Centro", tickangle = -45),
    yaxis = list(title = "Probabilidad de Licencia por Enfermedad")
  )


htmlwidgets::saveWidget(fig_center,"temp_plot.html")
browseURL("temp_plot.html")

#---------------------------------------------------------------------------------------------------------

#¿Hay una relación entre el salario anual y la retención de empleados en diferentes departamentos?

# Agrupar por departamento y calcular la tasa de retención y salario anual promedio
dept_retention <- employees %>%
  group_by(Department) %>%
  summarize(
    num_employees = n(),
    avg_annual_salary = mean(AnnualSalary, na.rm = TRUE),
    avg_years = mean(Years, na.rm = TRUE)
  ) %>%
  mutate(
    retention_rate = avg_years / max(avg_years)  # Normalizamos la tasa de retención
  )

# Crear un gráfico de dispersión con plotly para mostrar la relación entre salario y retención
fig <- plot_ly(
  data = dept_retention,
  x = ~avg_annual_salary,
  y = ~retention_rate,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 10),
  text = ~paste("Departamento:", Department, "<br>Salario Anual Promedio:", round(avg_annual_salary, 2), "<br>Tasa de Retención:", round(retention_rate, 2)),
  color = ~Department
) %>%
  layout(
    title = "Relación entre Salario Anual y Tasa de Retención por Departamento",
    xaxis = list(title = "Salario Anual Promedio (USD)"),
    yaxis = list(title = "Tasa de Retención"),
    legend = list(title = "Departamento")
  )


htmlwidgets::saveWidget(fig,"temp_plot.html")
browseURL("temp_plot.html")

