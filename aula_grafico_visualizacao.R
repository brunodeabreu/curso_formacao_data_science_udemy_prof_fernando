# Graficos e Visualização 

trees
hist(trees$Height)
hist(trees$Height, main = "Árvores", ylab = "Frequência", xlab = "Altura", col="blue")
hist(trees$Height, 
     main = "Árvores", 
     ylab = "Frequência", 
     xlab = "Altura", 
     col="blue",
     density = 20, 
     breaks = 20)

# Densidade
densidade <- density(trees$Height)
plot(densidade)

# Densidade sobre o histograma
hist(trees$Height, main = NULL, xlab = NULL, ylab = NULL)
par(new = TRUE)  # informa para o R que o prox plot sera sobreposto
plot(densidade)


# Dispersão
plot(trees$Girth, trees$Volume)
plot(trees$Girth, trees$Volume,
     main = "Árvores")

plot(trees$Girth, trees$Volume,
     main = "Árvores", ylab = "Circunferência", xlab = "Volume",
     col = "blue")


plot(trees$Girth, trees$Volume,
     main = "Árvores", ylab = "Circunferência", xlab = "Volume",
     col = "blue",
     pch = 20)


#   muda o tipo do gráfico de dispercao para linha
plot(trees$Girth, trees$Volume,
     main = "Árvores", ylab = "Circunferência", xlab = "Volume",
     col = "blue",
     pch = 20,
     type = "l")

# dispersao com jitter para facilitar os elementos sobrepostos mais facilmente
plot(jitter(trees$Girth), trees$Volume,
     main = "Árvores", ylab = "Circunferência", xlab = "Volume",
     col = "blue")



# Legenda com dimensão categórica

plot(CO2$conc, CO2$uptake, pch = 20, col = CO2$Treatment)
legend("bottomright",    #“bottomright”, “bottom”, “bottomleft”, “left”, “topleft”, “top”, “topright”, “right”, “center”
       legend = c("nonchilled", "chiulled"),
       cex = 1,    # diminuir o tamanho
       fill = c("black", "red")) # como preencher 





# Matriz de dispersão
plot(trees)

# Dividindo a tela

split.screen(figs = c(2,2))
screen(1)
plot(trees$Girth, trees$Height)
screen(2)
plot(trees$Girth, trees$Volume)
screen(3)
plot(trees$Height, trees$Volume)
screen(4)
hist(trees$Volume)
close.screen(all = TRUE)


# Boxplot

boxplot(trees$Volume, main = "Árvore", xlab = "Volume")

boxplot(trees$Volume, main = "Árvore", 
        xlab = "Volume",
        col = "blue",
        horizontal = T)

boxplot(trees$Volume, main = "Árvore", 
        xlab = "Volume",
        col = "blue",
        horizontal = T,
        outline = F)

boxplot(trees$Height, main = "Árvore", 
        xlab = "Altura",
        col = "blue",
        notch = T)

# visualizando os dados do boxplot
boxplot.stats(trees$Height)
boxplot.stats(trees$Height)$stats


boxplot(trees)



# Agregação

# dataset
InsectSprays

# preparando (resumo) os dados com funcao aggregate
spray <- aggregate(. ~ spray, data = InsectSprays, sum)
spray


# Barras
barplot(spray$count, 
        col = gray.colors(6),
        xlab = "Spray", ylab = "Total",
        names.arg = spray$spray   # variavel que vou gerar as barras
        )
box() # gera uma moldura ao redor do gráfico


# Pizza ou Setor
pie(spray$count, laels = spray$spray, 
    main = "Spray, col",
    col = c(1:6))
legend("bottomright", legend = spray$spray, 
       cex = 1,
       fill = c(1:6))



# Tabelas
install.packages("stargazer")
library(stargazer)

# latex
stargazer(iris)

# HTML
stargazer(iris, type = "html")

# Texto
stargazer(iris, type = "text")

# sem sumarizar
stargazer(iris, type = "text", summary = F)





# Laticce
library(lattice)

# boxplot
bwplot(trees$Volume)
bwplot(trees$Volume, main = "Árvores",
       xlab = "Volume")

# histograma
histogram(trees$Volume, main = "Árvores",
          xlab = "Volume", 
          aspect = 1,   # proporcao
          type = "percent", # como calcular as barras :perc, count ou density
          nint=20  # num bins
          )

# Dataset
chickwts

# histograma condicional
histogram(chickwts$weight)

# somente para visualizar
aggregate(chickwts$weight, 
          by=list(chickwts$feed), FUN=sum)

# gerando o histograma condicional por alimentacao
histogram( ~ weight | feed, data = chickwts)



# Gráfico de Dispersao Condicional

xyplot(CO2$conc ~ CO2$uptake)
xyplot(CO2$conc ~ CO2$uptake | CO2$Type)
xyplot(CO2$conc ~ CO2$uptake | CO2$Treatment)



# dotplot

esoph  # Dataset

dotplot(esoph$alcgp ~ esoph$ncontrols, data = esoph)
dotplot(esoph$alcgp ~ esoph$ncontrols |  esoph$tobgp)



# Matriz de dispersao
head(CO2)
splom(~CO2[4:5] | CO2$Type, CO2)


# Densidade condicional
densityplot(CO2$conc)
densityplot(~CO2$conc | CO2$Treatment)
densityplot(~CO2$conc | CO2$Treatment, plot.points=F)



# Grafico 3D
head(OrchardSprays)  # Dataset

cloud(decrease ~ rowpos * colpos, data = OrchardSprays)
cloud(decrease ~ rowpos * colpos, groups = treatment, data = OrchardSprays)



# Level plot
head(trees)  #dataset

levelplot(Girth  ~ Height * Volume, data = trees)

