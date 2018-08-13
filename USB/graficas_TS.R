library(dplyr)

Sys.setlocale("LC_ALL", 'en_US.UTF-8')
inai <- read.csv(paste0("/Users/admin/Desktop/BASE_DATOS_INAI/USB/Datos Grande/", "AI_FEDERAL.csv"), encoding = "UTF-8")
variable.names(inai)

vutiles <- c(3,8,19,6)
inai <- inai[,vutiles]

inai$Fecha.de.respuesta <- as.Date(inai$Fecha.de.respuesta, format = "%m/%d/%Y")
inai$Fecha.de.recepción.legal <- as.Date(inai$Fecha.de.recepción.legal , format = "%m/%d/%Y")

class(inai$Fecha.de.recepción.legal)
class(inai$Fecha.de.respuesta)

inai$diferencia <-  inai$Fecha.de.respuesta - inai$Fecha.de.recepción.legal

inai <- inai %>%
  filter(diferencia >= 0)

levels(inai$Sujeto.Obligado)

corregir <- c(14,15,16,17,31,43,44,45,46, 41)
correccion <- c("INEVAL"
               ,"INEGI"
               ,"INAPAM"
               ,"INAI"
               ,"SAGARPA"
               ,"SINDICATO COLEF"
               ,"SINDICATO IMJUVE"
               ,"SCJN",
               "TEPJF", 
               "SECRETARIADO EJECUTIVO, SNSP")

levels(inai$Sujeto.Obligado)[corregir] <- correccion

inai$Texto.de.la.solicitud <- NULL 


write.csv(inai, "inai.csv")

##ggplot(SO, aes(x = edo, y = GA)) +
##  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
  

library(ggplot2)
library(ggthemes)

tiempo_prom <- inai %>%
  group_by(Sujeto.Obligado) %>%
  summarize(promedio = mean(diferencia))

A <- ggplot(inai, aes(x = diferencia, y = Sujeto.Obligado)) + 
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4) +
  geom_point(data = tiempo_prom, aes(x = promedio, y = Sujeto.Obligado), colour = "yellow", size = 3) +
  theme_solarized()
  
A
ggplotly(A)


##
# Graficas por tipo de SO
##



#df.new <- df2 %>% 
#group_by(ExperimentCohort,LetterGrade) %>% 
#summarise (n = n()) %>%
#mutate(freq = n / sum(n))
#

# aes(x = LetterGrade, fill = ExperimentCohort)) +
# geom_histogram(aes(weight = weight), stat = 'count', position = 'dodge')


test <- SO %>% 
  filter(edo == "Guanajuato") %>%
  group_by(ts) %>%
  summarize(mean_GA = mean(GA))

a <-   ggplot(data = test, aes(x = ts, y = mean_GA, fill = ts)) + ggtitle(paste0("Indice GA del estado de ", "Guanajuato")) + 
  labs(x = "Tipo de SO", y = "Indice de GA", colour = "Colour\nlegend") +
  geom_bar(stat="identity", show.legend = FALSE) + 
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
a
ggplotly(a)





