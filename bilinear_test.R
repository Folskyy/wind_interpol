library(akima)
library(simulariatools)
# cordenads da latitude dos sensores
longitudex <- c(-89.585710,
                -89.537254,
                -96.612942,
                -96.563083,
                -95.192150,
                -87.803883,
                -88.161180,
                -87.393269,
                -100.915360,
                -103.029290,
                -97.570033,
                -99.058779,
                -106.842542)

# cordenadas da longitude dos sensores
latitudey <- c(45.493703,
               46.233910,
               39.110452,
               39.100781,
               39.040431,
               32.541733,
               31.853860,
               32.950461,
               46.769727,
               40.461927,
               33.401231,
               35.410600,
               32.590694)

# Pegando os dados de Neonscience
dados<-read.table("final.txt", header=FALSE, sep=",")
print(dados)

f_x_y <- matrix(nrow = 22320, ncol = 13)
g_x_y <- matrix(nrow = 22320, ncol = 13)

for(q in 1:13){
  f_x_y[,q] <- dados[[q*2 - 1]]
  g_x_y[,q] <- dados[[q*2]]
}
f_x_y

# numero da linhas que será interpolada
n <- 1
# pontos que serão interpolados
x_0 <- -100:-90 
y_0 <- 35:45

# função interpoladora do pacote akima
Field_f <- interp(longitudex, latitudey, f_x_y[n,], x_0, y_0)
Field_g <- interp(longitudex, latitudey, g_x_y[n,], x_0, y_0)
  
# salvando os dados para um arquivo de texto
sink("dadosInterpolados.txt")
  
cat("x,y,u,v")
cat("\n")

for(i in 1:11){
  for(j in 1:11){
    cat(Field_f$x[i])
    cat(",")
    cat(Field_f$y[j])
    cat(",")
    cat(Field_f$z[i,j])
    cat(",")
    cat(Field_g$z[i,j])
    cat("\n")
  }
}
sink()

#plotando os dados e salvando em um arquivo png
Field <- read.table("dadosInterpolados.txt", header=TRUE, sep=",")
png("setas.png", width = 720, height = 720)
vectorField(Field,scale = 0.005, size = 1)
  
dev.off()
