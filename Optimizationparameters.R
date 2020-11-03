
#Lectura de Datos

library(ISLR)
library(dplyr)

Carseats = Carseats %>%
  mutate(High = as.factor(ifelse(Sales <= 8, "No", "Yes")))

Datos=Carseats

head(Datos)

colSums(is.na(Datos))

Datos=Datos[,-1]
str(Datos)


boxplot(Datos$CompPrice)

Linf=quantile(Datos$CompPrice, 0.25)-1.5*IQR(Datos$CompPrice)
Lsup=quantile(Datos$CompPrice, 0.75)+1.5*IQR(Datos$CompPrice)
which(Datos$CompPrice<Linf)
which(Datos$CompPrice>Lsup)

Datos<-Datos[-c(which(Datos$CompPrice<Linf),which(Datos$CompPrice>Lsup)),]

boxplot(Datos$Income)
boxplot(Datos$Advertising)
boxplot(Datos$Population)

boxplot(Datos$Price)
Linf=quantile(Datos$Price, 0.25)-1.5*IQR(Datos$Price)
Lsup=quantile(Datos$Price, 0.75)+1.5*IQR(Datos$Price)

which(Datos$Price<Linf)
which(Datos$Price>Lsup)

Datos<-Datos[-c(which(Datos$Price<Linf),which(Datos$Price>Lsup)),]

boxplot(Datos$Age)
boxplot(Datos$Education)

# LOS DATOS ESTAN LIMPIOS :)

# no conviene aplicar CV por el costo computacional
# probar que los datos estan limpios

# Partición en data de entrenamiento y prueba
set.seed(30)
ind<- sample(1:round(nrow(Datos)*0.8))
train <- Datos[ind,]
test <- Datos[-ind,]

########################################################
# ÁRBOLES DE DECISIÓN
########################################################
library(tree)
arbol_clasi <- tree(formula= High ~ ., data = train)

# Validación cruzada para encontrar el número de nodos óptimo
set.seed(80)
cv_arbol <- cv.tree(arbol_clasi, FUN = prune.misclass, K=10)
cv_arbol
plot(cv_arbol$size, cv_arbol$dev, type = "b")
# Size con menor error es 

prune_arbol = prune.misclass(arbol_clasi, best = 7)

#Error de predicción con la data de prueba
tree_pred<-predict(prune_arbol, test, type = "class")
table(tree_pred, test$High)
(31+13)/(31+13+36+40)*100

########################################################
# RANDOM FOREST
########################################################

# FUNCION 1: Optimizacion de los tres parámetros de RF

tuning_RF_npredsizentree <- function(df, y, ntree, size){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del número de predictores evaluados (mtry)
  # el número de árboles construidos (ntree)
  # y el número mínimo de observaciones en los nodos terminales (size)
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = vector con distintos números de árboles
  #   size = vector con distintos números mínimos de observaciones en los nodos terminales
  library(randomForest)
  
  p <- ncol(df) - 1
  oob_error<- matrix(0,ncol=4,nrow=(p*length(size)*length(ntree)))
  colnames(oob_error)=c("Num_Pred","Num_Arboles","Size","OOB_Error")
  cont=0
  
  for (i in 1:p) {
    for (k in 1:length(ntree)){ 
      for (j in 1:length(size)){
        cont=cont+1
        f <- formula(paste(y,"~ ."))
        modrf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree[k], size=size[j], na.action=na.omit)
        oob_error[cont,] <- cbind(i,ntree[k],size[j],modrf$err.rate[ntree[k], 1])
      }
      
    }
  }
  
  resultado <- data.frame(oob_error)
  #plot(resultado[,4], type = "b", main="OOB Error según número de predictores y profundidad", ylab="OOB Error")
  return(resultado)
}


#resul=tuning_RF_npredsizentree(train,"Datos",ntree=c(200,300,500),size=c(100,200,300,500,600))
#resul
#resul[which.min(resul[,4]),]

# FUNCION 2: REPITIENDO - METODO COMPUTACIONAL

rep_tuningRF<- function(r,df, y, ntree, size){
  p <- ncol(df) - 1
  oob=matrix(0,ncol=r,nrow=(p*length(size)*length(ntree)))
  for(i in 1:r){
    res<-tuning_RF_npredsizentree(df, y, ntree, size)
    oob[,i]=res[,4]
  }
  resultado=data.frame(res[,1:3],apply(oob,1,mean))
  plot(resultado[,4], type = "b", main="OOB Error", ylab="OOB Error por parámetros")
  
  for(i in 1:p){
    inc=length(ntree)*length(size)
    abline(v=i*inc, col="Red")
  }
  
  return(resultado)
}

# Optimización de Parámetros RF

set.seed(2021)
RF_OPT=rep_tuningRF(10,train,"High",ntree=c(200,300,500),size=c(100,200,300))
RF_OPT
which.min(RF_OPT[,4])
RF_OPT[which.min(RF_OPT[,4]),]

# Ajustando el modelo RF con parámetros óptimos
set.seed(40)
modelo_RF<-randomForest(formula = High~ ., data=train, mtry=3, ntree=500, size=200, na.action=na.omit)

# Error de predicción con la data de prueba
predicciones <- predict(object = modelo_RF, newdata = test, type = "class")
table(predicciones, test$High)
(11+15)/(120)*100

########################################################
# ADA BOOST
########################################################

tuning_adaboost<-function(df_train, df_test, mfinal_vec, y_name){
  suppressWarnings(library(adabag))
  error<-c()
  for(i in 1:length(mfinal_vec)){
    f <- formula(paste(y_name,"~ ."))
    model = boosting(formula=f, data = df_train, boos = TRUE, mfinal=mfinal_vec[i])
    pred = predict.boosting(model , df_test)
    error[i]<-pred$error*100
  }
  #plot(mfinal_vec,error,type="b")
  return(list(mfinal_vec,error=error))
}

#set.seed(20201)
#tuning_adaboost(train,test,c(50,100,200),"High")

tuning_adab_rep<-function(df_train, df_test, mfinal_vec, y_name ,r){
  
  error<-matrix(0,ncol = r, nrow=length(mfinal_vec))
  for(i in 1:r){
    error[,i]<-tuning_adaboost(df_train, df_test, mfinal_vec, y_name)$error
  }
  
  resultado<-data.frame(cbind(mfinal_vec,apply(error,1,mean)))
  colnames(resultado)[2]="Error_Test"
  plot(resultado[,2], type = "b", main="Error de Clasificación - Test", ylab="% de Error de Clasificación")
  return(resultado)

}

set.seed(20201)
tuning_adab_rep(train,test,c(50,100,200),"High",100)
























