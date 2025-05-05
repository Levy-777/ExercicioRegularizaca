#APRENDIZADO SUPERVISIONADO - REGULARIZAÇÃO

##Pacotes que serão utilizados:
install.packages("glmnet")
library(glmnet)

esforco3$altura <- (esforco3$altura/10)
esforco3 <- transform(esforco3, sup = (altura * peso/3600)^ 0.5 )
esforco3 <- transform(esforco3, imc = peso / (altura/100)^2 )


vo2 <- c("vo2fcpico", "vo2pcr", "vo2lan", "vo2rep")


for (i in vo2){
# a função lm() ajusta o modelo de regressão linear multivariaveis
# com a summary() para obter os estimadores de mínimos quadrados
reggae <- lm(esforco3[[i]] ~ idade + peso + sup + imc , data = esforco3)
cat("==========================================================================", i, "==========================================================================")
print(summary(reggae))


#Para extrair as variáveis explicativas e a variável resposta
#X recebe uma matriz das 7 variáveis explicativas data.matrix()
#y recebe variável resposta
y <- esforco3[[i]]
y[is.na(y)] <- mean(y, na.rm = TRUE)
X <- esforco3[ , c("idade","peso","sup" , "imc")]
X <- data.matrix(X)


# Para ajustar o modelo Ridge
# o argumento  alpha = 0 na função cv.glmnet()  indica que estamos fazendo o modelo Ridge
modelo_Ridge = cv.glmnet(X, y, alpha = 0)
modelo_Ridge
cat("==========================================================================", i, "==========================================================================")
print(modelo_Ridge)

#gerando o gráfico dessa regularização
plot(modelo_Ridge, main = paste("modelo ridge",i))
dev.new()

# os coeficientes  são obtidos por meio da função coef ()
coef(modelo_Ridge, s = "lambda.min")
modelo_Ridge$lambda.min
sqrt(modelo_Ridge$cvm[modelo_Ridge$lambda == modelo_Ridge$lambda.min])

#Os valores preditos para os elementos do conjunto de dados e a correspondente raiz quadrada do MSE (RMSE) são obtidos por meio das funções predict( ) e sqrt ( ).
predict(modelo_Ridge, X, s = "lambda.min")

# Para ajustar o modelo Lasso
# o argumento  alpha = 1 na função glmnet()  indica que estamos fazendo Lasso
modelo_Lasso <- cv.glmnet(X, y, alpha = 1) 

#gerando o gráfico dessa regularização
plot(modelo_Lasso, main = paste("modelo lasso",i))
dev.new()

# os coeficientes  são obtidos por meio da função coef ()
coef(modelo_Lasso, s = "lambda.min")
modelo_Lasso$lambda.min
sqrt(modelo_Lasso$cvm[modelo_Lasso$lambda == modelo_Lasso$lambda.min])

# Para ajustar o modelo Elastic Net
# o argumento  alpha = 0,5 na função glmnet()  indica que estamos fazendo Elastic Net
modelo_Elastic <- cv.glmnet(X, y, alpha = 0.5) 

#gerando o gráfico dessa regularização
plot(modelo_Elastic, main = paste("modelo elastic", i))
dev.new()

# os coeficientes  são obtidos por meio da função coef ()
coef(modelo_Elastic, s = "lambda.min")
modelo_Elastic$lambda.min
sqrt(modelo_Elastic$cvm[modelo_Elastic$lambda == modelo_Elastic$lambda.min])
}


