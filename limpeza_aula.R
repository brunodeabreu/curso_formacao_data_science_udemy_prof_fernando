# Aula Limpeza de dados

# importando os dados, string como NA e string como fatores
dados <- read.csv('limpeza_tratamento/dados/Churn.csv', sep = ';', na.strings = "", stringsAsFactors = T)
head(dados)
summary(dados)


# Corrigindo nomes de colunas
colnames(dados) <- c('Id', 'Score', 'Estado', 'Genero', 'Idade', 'Patrimonio', 'Saldo', 'Produtos', 
                     'TemCartCredito', 'Ativa', 'Salario', 'Saiu')
head(dados,n = 8)
View(dados)


# Explorar os dados

# Estados
counts <- table(dados$Estado)
barplot(counts, main='Count em Estado', xlab = 'Estados')

# Genero
counts <- table(dados$Genero)
barplot(counts, main = 'Count em Gênero', xlab = 'Gênero')


# Source
summary(dados$Score)
boxplot(dados$Score, horizontal = T, main = 'Score')
hist(dados$Score)


# Idade
summary(dados$Idade)
boxplot(dados$Idade, horizontal = T, main = 'Idade')
hist(dados$Idade)

# Saldo
summary(dados$Saldo)
boxplot(dados$Saldo, horizontal = T, main = 'Saldo')
hist(dados$Saldo)

# Salário
summary(dados$Salario)
boxplot(dados$Salario, horizontal = T, main = 'Salário')
hist(dados$Salario)

# NaN's 
?complete.cases
dados[!complete.cases(dados), ]

# tratando NaN em Salários, replace com a mediana
summary(dados$Salario)
median(dados$Salario, na.rm = T)

dados[is.na(dados$Salario),]$Salario <- median(dados$Salario, na.rm = T)
dados[is.na(dados$Salario),]

# tratando NA e falta de padronizaçao em Gênero
unique(dados$Genero)
summary(dados$Genero)

dados[is.na(dados$Genero) | dados$Genero == 'M', ]$Genero  <- "Masculino"
dados[dados$Genero == 'F' | dados$Genero == 'Fem',  ]$Genero <- "Feminino"
summary(dados$Genero)

# resetando os levels dos fatores para remover F, Fem e M
dados$Genero <- factor(dados$Genero)
summary(dados$Genero)


# Tratando Idade fora do dominio
dados[dados$Idade < 0 | dados$Idade > 120, ]

dados[is.na(dados$Idade), ]  # verificnado se temos idade NA
median(dados$Idade)

dados[dados$Idade < 0 | dados$Idade > 120, ]$Idade <- median(dados$Idade)
dados[dados$Idade < 0 | dados$Idade > 120, ]
summary(dados$Idade)

# Tratando dados duplicados
dados[duplicated(dados$Id), ]
dados <- dados[-c(82), ]
dados[duplicated(dados$Id), ]
View(dados)

# Dados (Estado) fora do dominio
unique(dados$Estado)
summary(dados$Estado)

dados[!dados$Estado %in% c('RS', 'SC', 'PR'), ]$Estado <- "RS"
dados$Estado <- factor(dados$Estado)
summary(dados$Estado)


# Outliers 
# considerando que salario com 2x desvio padrao é outlier
desv <- sd(dados$Salario, na.rm = T)
out <- boxplot(dados$Salario)$out
out

dados[dados$Salario >= 2 * desv, ]$Salario <- median(dados$Salario)
boxplot(dados$Salario)
dados[dados$Salario >= 2 * desv, ]

