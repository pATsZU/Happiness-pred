# Instalowanie i ładowanie pakietu xgboost
#install.packages("xgboost")
#install.packages("mlr")
library(xgboost)
library(data.table)
library(mlr)
library(caret)
library(readxl)
library(yardstick)

set.seed(2023)
dane <- read_excel("C:/Users/patsz/OneDrive/Pulpit/praca magisterska/dane praca/dane.xlsx")

dane <-dane[,-1]#usunięcie sygnatury czasowej

### One-hot encoding
df <- dane
sapply(lapply(df, unique), length)#sprawdzenie poziomów/ilosci unikatow

dummy <- dummyVars(" ~ .", data=df)

final_df <- data.frame(predict(dummy, newdata=df))
dane <- final_df 

#For many machine learning algorithms, using correlated features is not a good idea. It may sometimes make prediction less accurate, 
#and most of the time make interpretation of the model almost impossible. GLM, for instance, assumes that the features are uncorrelated.

#Fortunately, decision tree algorithms (including boosted trees) are very robust to these features. Therefore we have nothing to do to manage this situation
#*https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html

### Podział danych na zbiór treningowy i testowy
prop <- 0.7
index <- sample(1:nrow(dane), prop * nrow(dane))
train <- dane[index, ]
test <- dane[-index, ]
### Zdefiniowanie zmmiennych objaśnianych i objaśniających w zbiorze treningowym
train_x = data.matrix(train[, -107])
train_y = train$happiness

### Zdefiniowanie zmmiennych objaśnianych i objaśniających w zbiorze testowym
test_x = data.matrix(test[, -107])
test_y = test[, 107]

### Definiowanie ostatecznych zbiorów
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

### Definiowanie watchlisty
watchlist = list(train=xgb_train, test=xgb_test)

### Modele XGBoost wraz z wyświetlaniem danych treningowych i testowych 

model = xgb.train(data = xgb_train, max.depth = 6, watchlist=watchlist, nrounds = 100)

### Finalny model

final = xgboost(data = xgb_train, max.depth = 6, nrounds = 7, verbose = 0)
print(final)

### Predykcja
pred_y = predict(final, xgb_test)

### Błędy
rmse=round(RMSE(test_y, pred_y),2)
mae =round(MAE(test_y, pred_y),2) #mae
mse =round(mean((test_y - pred_y)^2),2) #mse
sprintf("Blad MSE modelu 1 dla danych testowych: %s",mse)

sprintf("Blad MAE modelu 1 dla danych testowych: %s",mae)

sprintf("Blad RMSE modelu 1 dla danych testowych: %s",rmse)


### Wagi

importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = final 
)
importance_matrix[1:10]
xgb.plot.importance(importance_matrix[1:10])
# Accuracy
round(pred_y, digits = 0)
test_y <- as.numeric(test_y)
pred_y <- as.numeric(pred_y)
confusionMatrix(as.factor(round(pred_y, digits = 0)),as.factor(test_y))

