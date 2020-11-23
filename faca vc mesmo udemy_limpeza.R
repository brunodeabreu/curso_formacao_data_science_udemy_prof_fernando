# Faça você mesmo Limpeza

data <- read.csv("limpeza_tratamento/dados/tempo.csv", sep = ";", na.strings = "", stringsAsFactors = T)
head(data)


# Aparecencia
counts <- table(data$Aparencia)
barplot(counts, main="Aparência", xlab = "Aparência")

# Vento
counts <- table(data$Vento)
barplot(counts, main = "Vento", xlab = "Vento")
unique(data$Vento)

# Jogar
counts <- table(data$Jogar)
barplot(counts, main = "Jogar", xlab = "Jogar")
summary(data$Jogar)
unique(data$Jogar)


# Temperatura
summary(data$Temperatura)
boxplot(data$Temperatura, horizontal = T, main = "Temperatura")
hist(data$Temperatura)


# Umidade
summary(data$Umidade)
boxplot(data$Umidade, horizontal = T, main = "Umidade")
hist(data$Umidade)


# Tratamento dos dados

#Aparência : sol, nublado, chuva
#Temperatura: -130 ~ 130 F
#Umidade : 0 ~ 100
#jogar : sim / nao
#Tratar NAs


# Aparência
data[data$Aparencia == "menos",]$Aparencia <- "sol"
data$Aparencia <- factor(data$Aparencia)

#check aparência
data[data$Aparencia == "menos",]
counts <- table(data$Aparencia)
barplot(counts, main="Aparência", xlab = "Aparência")


# temperatura -130 a 130 F
data[data$Temperatura < -130 | data$Temperatura > 130 , ]$Temperatura <- median(data$Temperatura)
summary(data$Temperatura)
boxplot(data$Temperatura, horizontal = T, main = "Temperatura")
hist(data$Temperatura)


# Umidade  0 ~ 100

summary(data$Umidade)
data[data$Umidade < 0 | data$Umidade > 100 | is.na(data$Umidade), ]$Umidade <- median(data$Umidade, na.rm = T)
summary(data$Umidade)
boxplot(data$Umidade, horizontal = T, main = "Umidade")
hist(data$Umidade)



# NAs
sum(is.na(data))

summary(data$Vento)
data[is.na(data$Vento),]$Vento <- "FALSO"
sum(is.na(data))
complete.cases(data)


