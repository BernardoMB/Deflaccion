paquetes.utilizados <- c("lubridate", "dplyr", "maps", "reshape2",
                         "mapproj", "ggplot2", "scales", "plyr", "knitr",
                         "xtable", "tidyr","reshape2","xtable")
paquetes.instalados <- rownames(installed.packages())
paquetes.por.instalar <- setdiff(paquetes.utilizados, paquetes.instalados)

if(length(paquetes.por.instalar) != 0) install.packages(paquetes.por.instalar,
                                                        repos = "http://cran.us.r-project.org")

lapply(paquetes.utilizados, library, character.only = TRUE)

generar.factores.inflacion <- function(archivo.csv, fecha.valuacion, vector.fechas, vector.montos) {
  archivo.texto <- readLines(archivo.csv)
  linea.inicial <- grep(".*SP1*", archivo.texto)
  
  datos.IPN <- read.csv(archivo.csv, 
                        skip = linea.inicial - 1, 
                        header = TRUE, 
                        stringsAsFactors = FALSE)
  datos.IPN <- datos.IPN[, 1:2]
  datos.IPN$Fecha <- as.Date(datos.IPN$Fecha, format = "%d/%m/%Y")
  
  match1 <- match(vector.fechas,datos.IPN$Fecha)
  valores <-  datos.IPN[match1,]
  
  match2 <- match(fecha.valuacion,datos.IPN$Fecha)
  busca <- datos.IPN$SP1[match2]
  
  
  for(i in 1:length(match1))
  {
    valores[i,2] = ((busca-valores[i,2])/busca)*vector.montos[i]
  }   
  
  return(valores[,2])
}

archivo.texto <- readLines("Consulta_Banxico_pib.csv")
linea.inicial <- grep(".*SR10*", archivo.texto)

datos.pib <- read.csv("Consulta_Banxico_pib.csv", 
                      skip = linea.inicial - 1, 
                      header = TRUE, 
                      stringsAsFactors = FALSE)
datos.pib <- datos.pib[, 1:2]
datos.pib$Fecha <- as.Date(datos.pib$Fecha, format = "%d/%m/%Y")




plot(datos.pib$Fecha,
     generar.factores.inflacion("Consulta_Banxico.csv", 
                                as.Date("2016-02-01"),
                                datos.pib$Fecha, 
                                datos.pib$SR10),
     ylim = c(400000, 13651000),
     type = "o", pch = 19,
     main = "PIB nominal vs real",
     ylab = "PIB nominal / real", 
     xlab = "A??o"
)
lines(datos.pib$Fecha, datos.pib$SR10, type = "o", pch = 19, col = "blue")
legend('topleft', 
       c("nominal", "real"), 
       lty = 1, 
       col = c('black', 'blue'), 
       bty = 'n', 
       cex = .75)