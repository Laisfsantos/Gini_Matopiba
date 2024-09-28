rm(list=ls()) # Limpa todas as variaveis
cat("\f") # Limpa o console

library(pracma)
library(readxl) # Importa biblioteca para ler arquivos .xlsx
library(writexl) # Importa biblioteca para exportar arquivos .xlsx


area_gini2006 <- read_excel("area_gini2006.xlsx")
num_gini2006 <- read_excel("num_gini2006.xlsx")


area_gini <- data.matrix(area_gini2006); 
area_gini <- area_gini[1:nrow(area_gini),2:ncol(area_gini)];
num_gini <- data.matrix(num_gini2006); 
num_gini <- num_gini[1:nrow(num_gini),2:ncol(num_gini)];

## Função para calcular o Gini ###############
GiniTrap <-function(num_gini,area_gini){
X <- cumsum(num_gini/sum(num_gini));
Y <- cumsum(area_gini/sum(area_gini));
G1<-0;
for(k in 2:length(num_gini))
{
  G1 <- G1 + (X[k]-X[k-1])*(Y[k]+Y[k-1]);
}
G <- abs(1-G1);
return(as.numeric(G))
}


## Função para separar o Gini pelas classes de Câmara (1949) ###############
# Dado um vetor de índices de Gini para diferentes municípios, 
# esta função retorna o número de municípios em cada uma das classes
# de concentração fundiária definidas por Câmara (1949).
Gini_Classes <- function(Gini){
  Gini <- round(Gini,3);
  # 4 classes
  CF <- c(0,0.250); # Concentração fraca
  CM <- c(0.251,0.500); # Concentração média
  CFO <- c(0.501,0.900); # Concetração forte
  CA <- c(0.901,1.); # Concentração Absoluta
  nGini_CF<-Gini >= CF[1] & Gini <= CF[2]; nGini_CF<-length(nGini_CF[nGini_CF==TRUE]);
  nGini_CM<-Gini >= CM[1] & Gini <= CM[2]; nGini_CM<-length(nGini_CM[nGini_CM==TRUE]);
  nGini_CFO<-Gini >= CFO[1] & Gini <= CFO[2]; nGini_CFO<-length(nGini_CFO[nGini_CFO==TRUE]);
  nGini_CA<-Gini >= CA[1] & Gini <= CA[2]; nGini_CA<-length(nGini_CA[nGini_CA==TRUE]);
  nGini_Classes <- c(nGini_CF, nGini_CM, nGini_CFO, nGini_CA)
  names(nGini_Classes) <- c("Fraca", "Média", "Forte", "Absoluta")
  
  return(nGini_Classes)
}

#Calcula o Gini
G2006<-0
for(i in 1:337)
{
  G2006[i] <- GiniTrap(num_gini[i,],area_gini[i,])
}

# Gráfico das classes concentração
nGini2006_Classes <- Gini_Classes(G2006);
nGini_Classes <- rbind((nGini2006_Classes));

barplot(nGini_Classes,
        main = "Classes de Concentração de Terra",
        xlab = "Classes",
        ylab = "Número de Municípios",
        col = c("orange","brown"),
        beside = TRUE,
        legend.text = c("2006"),
        args.legend = list(cex=1,x = "topleft")
        )
legend(x=0,y=350, legend = c("2006"), 
       fill=c("orange"))