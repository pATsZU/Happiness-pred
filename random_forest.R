# Instalowanie i ładowanie pakietów
#install.packages("randomForest")

library(randomForest)
library(readxl)
# Wczytanie danych z arkusza Excela

dane <- read_excel("C:/Users/patsz/OneDrive/Pulpit/praca magisterska/dane praca/dane.xlsx")
dane <-dane[,-1]# usunięcie sygnatury czasowej

# Parametry Lasu Losowego 
set.seed(2023)
n<- 1000 #ntree
m<- 10# mtry

# Podział danych na zbiór treningowy i testowy
prop <- 0.7
index <- sample(1:nrow(dane), prop * nrow(dane))
train_data <- dane[index, ]
test_data <- dane[-index, ]

# Modele Las losowy

model1<-randomForest(formula = happiness ~ .,data=train_data,ntree= n,mtry=m,do.trace=10, keep.forest =T)
print(model1)

plot(model1, main=NULL)
print("Wspólczynnik determinacji:", quote = F)
print(model1$rsq[n])
# Wagi zmiennych
wz1<- round(importance(model1),2)
waga1<- wz1[order(wz1,decreasing = T),]
print("wagi poszczególnych zmiennych", quote = F)
print(as.data.frame(waga1))

# Błędy modelu
model1_p<-predict(model1, newdata=test_data)
mse_model1<-var(test_data$happiness-model1_p)
print("Blad MSE modelu 1 dla danych testowych:",quote = F)
print(mse_model1)
print("Blad MSE modelu 1 dla danych treningowych:",quote = F)
print(model1$mse[n])

rmse1tren<- sqrt(model1$mse[n])
rmse1test<- sqrt(mse_model1)
print("Blad RMSE modelu 1 dla danych treningowych:",quote = F)
rmse1tren
print("Blad RMSE modelu 1 dla danych testowych:",quote = F)
rmse1test


