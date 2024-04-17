#Grupo 21
#Angel Reyero Lobo
#Adrián Gómez Lamuedra
#Lucas Martín García



#Importar libreria
library(readr)
library(psych)

#Importar datos
ArchivoDatos <- read_csv("ArchivoDatos.csv")
Altura <- ArchivoDatos$`Mean height` #variable con los datos de las medias de los países


#Crear histograma de frecuencias relativas de la variable Altura
hist(Altura,
     xlim = c(140, 180),
     breaks = 13,
     freq = FALSE,
     main = "Histograma de Altura",
     col = "pink")

#mostrar la función de densidad de la función de distribucion empírica
lines(density(Altura),col="red",lwd=2)

#mostrar la función de densidad de una distribucion normal para comparar
curve(dnorm(x,mean=mean(Altura),sd=sd(Altura)), from=146,to=174, 
      add=TRUE, col="blue", lwd=2)

#Mostrar los cuartiles (25, 50 y 75 )
AlturaOrdenados = sort(Altura)
Q1 = (AlturaOrdenados[50] +AlturaOrdenados[51]) / 2
Q2 = (AlturaOrdenados[100] +AlturaOrdenados[101]) / 2
Q3 = (AlturaOrdenados[150] +AlturaOrdenados[151]) / 2

segments(x0 = Q1, y0 = 0,
         x1 = Q1, y1 = 10,
         col = "black", lwd = 2, lty = 2)
segments(x0 = Q2, y0 = 0,
         x1 = Q2, y1 = 10,
         col = "black", lwd = 2, lty = 2)
segments(x0 = Q3, y0 = 0,
         x1 = Q3, y1 = 10,
         col = "black", lwd = 2, lty = 2)

legend(x = "topright", legend = c("Histograma", "Funcion de Densidad", "Densidad de una Normal", "Cuartiles"),
       fill = c("pink", "red", "blue", "black"), 
       title = "Leyenda", cex = 0.85)



#Crear diagrama de Caja y Bigotes
boxplot(Altura, horizontal = TRUE,
        xlab = "Altura",  # Etiqueta eje X
        main = "Diagrama de Caja y Bigotes de Altura", # Título
        border = "black",  # Color del borde del boxplot
        outpch = 25,       # Símbolo para los outliers
        outbg = "green",   # Color de los datos atípicos
        whiskcol = "blue", # Color de los bigotes
        whisklty = 2,      # Tipo de línea para los bigotes
        lty = 1,
        col = "lightblue") # Tipo de línea (caja y mediana)
#Añadir cruz en la media
segments(x0 = mean(Altura), y0 = 0.97,
         x1 = mean(Altura), y1 = 1.03,
         col = "red", lwd = 1)
segments(x0 = mean(Altura) - 0.27, y0 = 1,
         x1 = mean(Altura) + 0.27, y1 = 1,
         col = "red", lwd = 1)
legend(x = "topright", legend = c("Media"),
       fill = c("red"), 
       title = "Leyenda")

#Boxplot conjunto con las tres variables del archivo de datos
#cargar los datos en un data.frame y tenerlos por columnas
conjuntoDatos <- data.frame(
  "lower 0.95" = ArchivoDatos$`Mean height lower 95% uncertaint`,
  "Mean height" = ArchivoDatos$`Mean height`,  
  "upper 0.95" = ArchivoDatos$`Mean height upper 95% uncertaint`)
#un boxplot con los tres a la vez 
boxplot(conjuntoDatos, col = rainbow(ncol(conjuntoDatos)))

#Crear diagrama de Tallo y Hojas por consola
stem(Altura)

#Crear diagrama de tallo y Hojas por imagen (plot)
plot.new()

out <- capture.output(stem(Altura)) #Captura el diagrama de tallo y Hojas en ingles
diagrama <- c()
diagrama[1] = "El numero decimal empieza en |"
for (i in 4:25) { #se meten los valores en el nuevo vector en castellano y sin espacios
  diagrama[i - 2] = out[i]
}
text(0.2, 1.04, "Diagrama de Tallo y Hojas", adj = c(0, 1), cex = 1.5) #titulo
text(0.22, 0.98, paste(diagrama, collapse = "\n"), adj = c(0, 1), cex = 1) #(agrandar plot si no se muestra completo)


#Obtener funcion de distribucion empírica
ECDF=ecdf(Altura) #Empirical Cumulative Distribution Function
plot(ECDF,col="red",lwd=3,xlab="Altura",ylab="", main = "Funcion de Distribucion Empírica de Altura")
points(Altura,rep(0,200),col="blue",pch=20,cex=1)
segments(Altura,rep(0,200),Altura,sapply(Altura,ECDF),col="blue",lwd=2)


#Crear grafica de función de distribución empírica tipificada
(xbar=mean(Altura))
(desvt=sd(Altura))
datosTipificados=(Altura-xbar)/desvt
head(datosTipificados)
ECDF2=ecdf(datosTipificados)
plot(ECDF2,col="blue",lwd=1,xlab="altura",ylab="", main = "Datos de Altura Tipificados")
curve(pnorm, from=-5,to=5, 
      add=TRUE, col="magenta", lwd=3, lty = 2)
legend(x = "topleft", legend = c("Funcion de Distribución", "Distribución de una Normal(0,1)"),
       fill = c("blue", "magenta"), 
       title = "Leyenda",
       cex = 0.9)



#Estimadores de la media (media muestral) y de la varianza (cuasivarianza muestral y varianza muestral)
#La media muestral y la cuasivarianza muestral son estimadores insesgados de la media y la varianza de 
#la población, sin embargo también calculamos la media muestral ya que tiene menor ECM
#el estimador minimal suficiente para (media poblacional, varianza poblacional) es (media muestral, cuasivarianza)
#Por otra parte el estimador de máxima verosimilitud será media muestral y varianza muestral de la 
#media poblacional y varianza poblacional respectivamente, así por los teroemas de consistencia de EMV tenemos que
#convergen al verdadero valor del parámetro y son asintóticamente eficientes ya que la varianza converge a la CFCR 

media = mean(Altura)
S = sd(Altura)
S2 = var(Altura) #Cuasivarianza muestral
varianza <- function(x) { ((length(Altura)-1)/length(Altura))*var(Altura) }
V = varianza(Altura) #Varianza muestral

plot.new()
text(0.3, 1.04, "Estimadores" , adj = c(0, 1), cex = 1.6) #titulo
text(0.1, 0.90, paste("Media muestral  = ", media) , adj = c(0, 1), cex = 1) 
text(0.1, 0.8, paste("Cuasidesviacion típica  = ", S) , adj = c(0, 1), cex = 1) 
text(0.1, 0.7, paste("Cuasivarianza  = ", S2) , adj = c(0, 1), cex = 1) 
text(0.1, 0.6, paste("Varianza muestral  = ", V) , adj = c(0, 1), cex = 1) 

#Intervalo de confianza de grado 0.95 de la media poblacional con desviación desconocida 
#Se utiliza el Teorema de Fisher para utilizar el método de la cantidad pivotal
alpha = 0.05
t = qt(1 - alpha/2,199) 
n = 200
raizn = sqrt (n)
infM = media - t * S / raizn
supM = media + t * S / raizn

plot.new()
text(0.15, 1.04, "Intervalos de confianza" , adj = c(0, 1), cex = 1.6) #titulo
text(0, 0.90, "El intervalo de confianza de grado 0.95 de la media poblacional es :" , adj = c(0, 1), cex = 1) #titulo
text(0.1, 0.8, paste("[" ,toString(infM) , " , " , toString(supM) , "]") , adj = c(0, 1), cex = 1) 


#Intervalo de confianza de grado 0.95 de la varianza poblacional
#Se utiliza el Teorema de Fisher para utilizar el método de la cantidad pivotal
chi1 = qchisq(1 - alpha/2,199)
chi2 = qchisq(alpha/2,199)
infV = ((n - 1)*S2)/chi1
supV = ((n - 1)*S2)/chi2

text(0, 0.65, "El intervalo de confianza de grado 0.95 de la varianza poblacional es :" , adj = c(0, 1), cex = 1) #titulo
text(0.1, 0.55, paste("[" ,toString(infV) , " , " , toString(supV) , "]") , adj = c(0, 1), cex = 1) 


#Contraste de Hipótesis: 
#H_0 := media Poblacional <= media muestral española (162.02787)
#H_1 := media Poblacional > media muestral española (162.02787)
#Primero calculamos la region crítica:
#RC = { Media muestral >= media española + (S/sqrt(n)) * t_(n-1);0.05}  con nivel de significación 0.05
#Lo obtenemos por el método de Razón de Verosimilitudes
mediaEsp = ArchivoDatos$`Mean height`[which(ArchivoDatos$Country == "Spain")] #media muestral en España
t2 = qt(1-alpha,199)
k = mediaEsp + (S / raizn)*t2
plot.new()
text(0, 1.04, "Contraste de Hipótesis (con nivel de significación 0.05) " , adj = c(0, 1), cex = 1.3) #titulo
text(0, 0.90, "H_0 := media Poblacional <= media muestral española (162.02787)" , adj = c(0, 1), cex = 1)
text(0, 0.80, "H_1 := media Poblacional > media muestral española (162.02787)" , adj = c(0, 1), cex = 1)

if(media >= k){
  text(0, 0.65, paste("media poblacional = ", media) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("media española + (S / sqrt(n)) * t_(n-1);0.95  = ", k) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(media, " >= ", k ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region critica," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
} else{
  text(0, 0.65, paste("media poblacional = ", media) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("media española + (S / sqrt(n)) * t_(n-1);0.95  = ", k) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(media, " < ", k ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region de aceptación," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "no se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
}


#Contraste de hipótesis a través del p-Valor
#test de p-Valor mostrado por consola
#el p-valor es el alfa más pequeño que hace que la muestra observada pertenezca a la región crítica de tamaño alfa,
#por lo que al tener un valor de 0.9999 ( > 0.05) nos quedamos con la hipótesis nula
t.test(Altura, alternative= "greater", mu=mediaEsp,
conf.level=1-alpha)

#test de p-valor mostrado por plot
plot.new()
out <- capture.output(t.test(Altura, alternative= "greater", mu=mediaEsp,
       conf.level=1-alpha)) #obtenemos el test
pStr = out[5] #obtenemos la linea donde esta el p-valor
#Obtenemos el p-valor del resultado del test
j = nchar(pStr)
while(j > 0 && substr(pStr, j , j) != "."){
  j = j - 1
}
j = j - 1
pValor = as.double(substr(pStr, j , nchar(pStr)))

text(0, 1.04, "Contraste de Hipótesis (con nivel de significación 0.05) " , adj = c(0, 1), cex = 1.3) #titulo
text(0, 0.90, "H_0 := media Poblacional <= media muestral española (162.02787)" , adj = c(0, 1), cex = 1)
text(0, 0.80, "H_1 := media Poblacional > media muestral española (162.02787)" , adj = c(0, 1), cex = 1)

if(pValor > alpha){
  text(0, 0.65, paste("p-valor = ", pValor) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("alpha = ", alpha) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(pValor, " > ", alpha ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region aceptación," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "no se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
} else{
  text(0, 0.65, paste("p-valor = ", pValor) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("alpha = ", alpha) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(pValor, " <= ", alpha ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region crítica," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
}



#Contraste de Hipótesis: 
#La segunda variable("Mean height lower 95% uncertaint"), representada en la segunda columna de datos del archivo
#representa el extremo inferior del intervalo de confianza de la media de nivel de significación 0.95 para cada país.

#H_0 := media Poblacional de la segunda variable == extremo inferior del intervalo de confianza de la media de la altura
#H_1 := media Poblacional de la segunda variable != extremo inferior del intervalo de confianza de la media de la altura
#Primero calculamos la region crítica a través del método Razón de Verosimilitudes:
#RC = { |Media muestral de la segunda variable - extremo inferior del intervalo de confianza de la media de la altura| >= (S/sqrt(n)) * t_(n-1);0.025}  con nivel de significación 0.05
#Se trata de un contraste de hipóteisis nula simple frente a la alternativa bilateral
#La región crítica se obtiene a través del método de Razón de Verosimilitudes

mediaLow = mean(ArchivoDatos$`Mean height lower 95% uncertaint`) #media muestral segunda variable
t2 = qt(1 - alpha/2,199)
kd = (S / raizn)*t2
ki = abs(mediaLow - infM)
plot.new()
text(0, 1.04, "Contraste de Hipótesis (con nivel de significación 0.05) " , adj = c(0, 1), cex = 1.3) #titulo
text(0, 0.90, "H_0 := media Poblacional de la segunda variable == extremo inferior del intervalo de confianza de grado 0.95 de la media poblacional de la altura" , adj = c(0, 1), cex = 1)
text(0, 0.80, "H_1 := media Poblacional de la segunda variable != extremo inferior del intervalo de confianza de grado 0.95 de la media poblacional de la altura" , adj = c(0, 1), cex = 1)

if(ki >= kd){
  text(0, 0.65, paste("| Media muestral de la segunda variable - extremo inferior del intervalo |  =  ", ki) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("(S/sqrt(n)) * t_(n-1);0.025  = ", kd) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(ki, " >= ", kd ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region critica," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
} else{
  text(0, 0.65, paste("| Media muestral de la segunda variable - extremo inferior del intervalo |  =  ", ki) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("(S/sqrt(n)) * t_(n-1);0.025  = ", kd) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(ki, " < ", kd ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region de aceptación," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "no se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
}


#Contraste de Hipótesis: 
#La tercera variable("Mean height upper 95% uncertaint"), representada en la tercera columna de datos del archivo
#representa el extremo superior del intervalo de confianza de la media de nivel de significación 0.95 para cada país.

#H_0 := media Poblacional de la tercera variable == extremo superior del intervalo de confianza de la media de la altura
#H_1 := media Poblacional de la tercera variable != extremo superior del intervalo de confianza de la media de la altura
#Primero calculamos la region crítica a través del método Razón de Verosimilitudes:
#RC = { |Media muestral de la tercera variable - extremo superior del intervalo de confianza de la media de la altura| >= (S/sqrt(n)) * t_(n-1);0.025}  con nivel de significación 0.05
#Se trata de un contraste de hipóteisis nula simple frente a la alternativa bilateral
#La región crítica se obtiene a través del método de Razón de Verosimilitudes

mediaUpp = mean(ArchivoDatos$`Mean height upper 95% uncertaint`) #media muestral tercera variable 
t2 = qt(1 - alpha/2,199)
kd2 = (S / raizn)*t2
ki2 = abs(mediaUpp - supM)
plot.new()
text(0, 1.04, "Contraste de Hipótesis (con nivel de significación 0.05) " , adj = c(0, 1), cex = 1.3) #titulo
text(0, 0.90, "H_0 := media Poblacional de la tercera variable == extremo superior del intervalo de confianza de grado 0.95 de la media poblacional de la altura" , adj = c(0, 1), cex = 1)
text(0, 0.80, "H_1 := media Poblacional de la tercera variable != extremo superior del intervalo de confianza de grado 0.95 de la media poblacional de la altura" , adj = c(0, 1), cex = 1)

if(ki2 >= kd2){
  text(0, 0.65, paste("| Media muestral de la tercera variable - extremo superior del intervalo |  =  ", ki2) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("(S/sqrt(n)) * t_(n-1);0.025  = ", kd2) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(ki2, " >= ", kd2 ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region critica," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
} else{
  text(0, 0.65, paste("| Media muestral de la tercera variable - extremo superior del intervalo |  =  ", ki2) , adj = c(0, 1), cex = 1)
  text(0, 0.55, paste("(S/sqrt(n)) * t_(n-1);0.025  = ", kd2) , adj = c(0, 1), cex = 1)
  text(0, 0.45, paste(ki2, " < ", kd2 ) , adj = c(0, 1), cex = 1)
  text(0, 0.35, "Como la m.a.s. pertenece a la region de aceptación," , adj = c(0, 1), cex = 1)
  text(0, 0.25, "no se tiene suficiente evidencia estadística para rechazar la hipótesis nula" , adj = c(0, 1), cex = 1)
}




#Comparacion histogramas, resultado gráfico del contraste de hipótesis

plot.new()
par(mfrow = c(1, 1))
plot(density(Altura),
     main = "Comparacion de datos", lwd = 2)
lines(density(ArchivoDatos$`Mean height lower 95% uncertaint`),
      col = "red",lwd=2)
lines(density(ArchivoDatos$`Mean height upper 95% uncertaint`),
      col = "blue",lwd=2)
legend(x = "topright", legend = c("Altura", "Altura extremo inferior",
                                  "Altura extremo superior", "Intervalo de confianza",
                                  "Media extremo inferior", "Media extremo superior"),
       fill = c("black", "red", "blue", "green", "brown", "lightblue"), 
       title = "Leyenda", cex = 0.8)

#mostrar en el gráfico el intervalo de confianza de grado 0.95 de la media poblacional de la altura
segments(x0 = infM , y0 = -0.0025,
         x1 = infM , y1 = 0.0025,
         col = "green", lwd = 3)
segments(x0 = supM , y0 = -0.0025,
         x1 = supM , y1 = 0.0025,
         col = "green", lwd = 3)
segments(x0 = infM, y0 = 0,
         x1 = supM , y1 = 0,
         col = "green", lwd = 3)

#mostrar en el gráfico la media de la segunda y tercera variable
segments(x0 = mediaLow, y0 = -0.0038,
         x1 = mean(ArchivoDatos$`Mean height lower 95% uncertaint`), y1 = 10,
         col = "brown", lwd = 2, lty = 2)
segments(x0 = mediaUpp, y0 = -0.0037,
         x1 = mean(ArchivoDatos$`Mean height upper 95% uncertaint`), y1 = 10,
         col = "lightblue", lwd = 2, lty = 2)






#Región creíble a través del método bayesiano:

#Para ello tomamos como distribución a priori de la variación una gamma inversa (v0/2, (v0*sigma0^2)/2)
#y para la media condicionada por la varianza una normal (mu0, (sigma^2)/n0).
#Con ello calculamos la distribución a posteriori conjunta que va a ser una 
#normal gamma inversa (mu1, n1, v1, sigma1^2), y calculamos la integral de 0 a infinito de 
#la densidad conjunta a posteriori para hallar la distribución a posteriori de la media que es una 
#t de student descentrada de parámetro de localización mu1 y parámetro de escalada sqrt((sigma1^2)/n1) y 
#v1 grados de libertad.
#Por lo tanto para hallar al región creíble solo hace falta tener en cuenta que 
#(mu - mu1) / sqrt((sigma1^2)/n1) es una t de student con v1 grados de libertad (solo se ha estandarizado)

#Vamos a hacer la distribución a posteriori de la normal 
#Para representar las distribuciones gráficamente hace falta instalar y cargar las siguientes librerías
install.packages("pscl")
library(pscl)
install.packages("invgamma")
library(invgamma)

#distribución a priori(para la media va a ser una normal(mu0, sd=sigma/raiz(n0)))
mu0=mediaEsp #valor inicial de la media 
n0=1 # precision inicial para N[mu0, sd=sigma/raiz(n0)]


v0=10
sigmados0= 17#vamos a poner estos valores iniciales, ya que si utilizásemos otros métodos 
#para aproximarlos implicarían un conocimiento previo de correlación, coeficiente de variación de Pearson...
#varianza apriori(es una gamma inversa)
alpha0=v0/2
beta=v0*sigmados0/2#son los valores iniciales de la distribución a priori de la varianza


#dibujamos la gamma inversa(alpha0, beta)

.x <- seq(qinvgamma(0.001,alpha0,beta), qinvgamma(0.99,alpha0,beta),
          length.out=1000)
plot(.x, dinvgamma(.x, alpha0, beta), xlab="varianza",
     ylab="Density", type="l",
     main=paste("varianza a priori~ gamma_inversa
(",alpha0,",",beta, ")"), col = "red", lwd = 2)
remove(.x)


#a posteriori(la conjunta)
mu1=(n0*mu0+n*media)/(n0+n)
n1=n0+n
v1=v0+n
v1sigma1=v0*sigmados0+(n-1)*S2+(n0*n/(n0+n)*(mu0-media)**2)
sigma1=v1sigma1/v1


#varianza a posteriori(es una gamma inversa(v1/2, v1*sigma1^2/2))
alpha1=v1/2
beta1=v1sigma1/2
.x <- seq(qinvgamma(0.001,alpha1,beta1),
          qinvgamma(0.99,alpha1,beta1),length.out=1000)
plot(.x,dinvgamma(.x,alpha1,beta1), xlab="varianza",
     ylab="Densidad",type="l",
     main=paste("varianza a posteriori~ gamma_inversa
(",alpha1,",",beta1,")"), col = "red", lwd = 2)
remove(.x)



#media a posteriori
escala=sqrt(sigma1/n1)#esto lo hacemos para estandarizar la t de student, ya que tiene de parámetro de escalada 
#(sigma1^2/n1)^1/2
print(paste("a posteriori la variable (mu -",mu1,"/",escala,")sigue
una t de Student con",v1,"grados de libertad"))
.x <-seq(qt(0.01,v1)*escala+mu1,
         qt(0.99,v1)*escala+mu1,length.out=1000)
plot(.x, dt((.x-mu1)/escala,v0), xlab="media",
     ylab="Densidad", type="l",
     main=paste("t de student (",v1,"g.l)"), col = "red", lwd = 2)
#Se puede observar que a parte de escalada está descentrada de parámetro de localización mu1
remove(.x)


#Ahora calculamos la región creíble (estandarizando como ya hemos explicado anteriormente)
t1<-qt(0.025, v1)
extInf<-t1*(sigma1/n1)^0.5+mu1
t2<-qt(0.975, v1)
extSup<-t2*(sigma1/n1)^0.5+mu1
plot.new()
text(0, 1.04, "La región creíble grado 0.95 de la media poblacional es :" , adj = c(0, 1), cex = 1) #titulo
text(0.1, 0.95, paste("[" ,toString(extInf) , " , " , toString(extSup) , "]") , adj = c(0, 1), cex = 1) 


text(0, 0.74, "El intervalo de confianza de grado 0.95 de la media poblacional es :" , adj = c(0, 1), cex = 1) #titulo
text(0.1, 0.65, paste("[" ,toString(infM) , " , " , toString(supM) , "]") , adj = c(0, 1), cex = 1) 

x1 = extSup - extInf
x2 = supM - infM
#Se puede observar que la amplitud de la región creíble es menor que la amplitud del intervalo de 
#confianza ya que utiliza más información al tener en cuenta la distribución a priori (aunque los 
#parámetros a priori se podrían haber estimado de una manera más fidedigna mediante métodos ajenos
#al temario del curso)





#Limpiar proyecto
detach("package:readr", unload = TRUE)
rm(list = ls())
dev.off()
cat("\014")

