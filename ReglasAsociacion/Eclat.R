library(readr)
library(arules)


CestaCompra <- read_csv("Documents/GitHub/R_projects/ReglasAsociacion/Market_Basket_Optimisation.csv", 
                        col_names = FALSE)

CestaCompra = read.transactions("Documents/GitHub/R_projects/ReglasAsociacion/Market_Basket_Optimisation.csv",
                                sep = ",", rm.duplicates = TRUE)
summary(CestaCompra)
itemFrequencyPlot(CestaCompra,topN = 50, 
                  ylab = "Frecuencia Compra (Relativa)", col = "blue", main = "Grafico frecuencias de compra")

reglas = eclat(CestaCompra, parameter = list( support = 0.004,minlen = 2))

# visualización
plot(reglas, method = "graph", engine = "htmlwidget")
inspect(sort(reglas, by = "support")[1:10])
