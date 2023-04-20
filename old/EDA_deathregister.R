##Registro di mortalita

#Analisi esplorativa
data = read.csv("deathregister.csv", header = TRUE, sep = ';')
summary(data)
str(data)
#correzione formato delle variabili 
data$dead = as.factor(data$dead)
data$enddate = as.Date(data$enddate, tryFormats = "%Y-%m-%d" )
summary(data)
dim(data)
names(data)
str(data)

#check dati mancanti
sum(is.na(data)) #non ci sono dati mancanti

#duplicati
sum(duplicated(data))
length(unique(data$idnum)) == length(data$idnum) #TRUE - no duplicati

summary(data)

library(ggplot2)
#create df for plot - dead vs alive
n_deaths = sum(data$dead==1)
n_alive = sum(data$dead==0)
df = data.frame(status = c("dead","alive"), count = c(n_deaths,n_alive))
p = ggplot(df, aes(x = status, y= count)) + 
  geom_bar(stat="identity", width = 0.5,color = 'black')  + 
  geom_text(aes(label=count), vjust=1.6, color = 'white') +
  labs(title = "Number of dead and alive (1984 - 1988)") +
  theme_light()
p
#in percentuale 
prop.table(table(data$dead))

#raggruppando per anno
library(dplyr)
data$year = lubridate::year(data$enddate) 
data_year = data %>% group_by(year) %>% summarise(n_alive = sum(dead==0), n_deaths=sum(dead==1)) 
data_year$n_tot = data_year$n_alive + data_year$n_deaths

# Create a stacked barplot
p2 = ggplot(data, aes(x = year, fill = dead)) +
  geom_bar() +
  labs(title="Total number of observations per year (dead vs alive)")+
  scale_fill_brewer(palette="Reds") + theme_minimal()
p2

prop.table(table(data$year))
#circa il 40% delle osservazioni nel 1988

sum(duplicated(data$idnum)) #non ci sono idnum duplicati

