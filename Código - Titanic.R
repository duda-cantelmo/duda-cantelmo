#instalacao dos pacotes ----
install.packages("margins")
install.packages("ROCR")
install.packages("wooldridge")
install.packages("sjPlot")
install.packages("AER")
install.packages("sandwich")
install.packages("lmtest")
install.packages("pROC")
require(pROC)
require(wooldridge)
require(sjPlot)
require(margins)
require(AER)
require(sandwich)
require(lmtest).
#lendo o arquivo ----
getwd()
dados_titanic <- read.csv("titanic.csv", sep = ";")

#graficos----
plot(dados_titanic$sobrev,dados_titanic$tarifa)
table(dados_titanic$sobrev,dados_titanic$tarifa)

#regredindo so com a classe ----
Probit_01 <- glm(formula = sobrev ~ primeira+segunda, family = binomial(link = "probit"),
                 data = dados_titanic)

summary(Probit_01)
Logit_01  <- glm(formula = sobrev ~ primeira+segunda, family = binomial(link = "logit"), 
                 data = dados_titanic)
summary(Logit_01)

#regredindo com tudo
Prob_tudo <- glm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado, family = binomial(link = "probit"),
                 data = dados_titanic)
Logit_tudo <- glm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado, family = binomial(link = "logit"),
                 data = dados_titanic)

summary(Prob_tudo)
summary(Logit_tudo)

#regredindo com interacoes
Prob_more <- glm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira+mulher*segunda, family = binomial(link = "probit"),
                 data = dados_titanic)
Logit_more <- glm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira+mulher*segunda, family = binomial(link = "logit"),
                  data = dados_titanic)

summary(Prob_more)
summary(Logit_more)



#valores preditos----
fitted(Prob_more) ; 
fitted(Logit_more)

#margins ----
margins_probit <- margins(Prob_more)
print(margins_probit)

margins_logit <- margins(Logit_more)
print(margins_logit)

#pseudo r2----
Probit_r2 <- update(Prob_more, formula = . ~ 1)
1 - as.vector(logLik(Prob_more)/logLik(Probit_r2))

Logit_00 <- update(Logit_more, formula = . ~ 1)
1 - as.vector(logLik(Logit_more)/logLik(Logit_00))

# Matriz de confusao ---- 

table(true = dados_titanic$sobrev, pred = round(fitted(Prob_more)))
table(true = dados_titanic$sobrev, pred = round(fitted(Logit_more)))
# Usando uma matriz robusta:
coeftest(Prob_more, vcov = sandwich)
coeftest(Logit_more, vcov = sandwich)

# Pseudo R²?
1 - Prob_more$deviance/Prob_more$null.deviance
1 - Logit_more$deviance/Logit_more$null.deviance 

# Taxa de acerto do modelo: 

table(true = dados_titanic$sobrev, pred = round(fitted(Prob_more)))
table(true = dados_titanic$sobrev, pred = round(fitted(Logit_more)))

summary(fitted(Prob_more))
summary(fitted(Logit_more))

# Efeitos Marginais:
Prob_more|>margins()
Logit_more|>margins()

# Curva ROC
#Cálculo do vetor de observado vs predito:
pred.pro <- pROC::roc(dados_titanic$sobrev,fitted(Prob_more))
pred.log <- pROC::roc(dados_titanic$sobrev,fitted(Logit_more))

par(pty = "s")
plot.roc(pred.pro,
         print.auc = TRUE,
         legacy.axes = TRUE
)
par(new=T)
plot.roc(pred.log,
         print.auc = TRUE,
         legacy.axes = TRUE,
         print.auc.y = .4,
         add = TRUE,
         col=2
)

legend("bottomright", legend=c("Regressão Probit", "Regressão Logit"),
       col= c("black", "red"), lwd=2)

#Teste Wald
wald_test <- anova(Logit_more,Prob_more, test = "Chisq")

print(wald_test)


# Uso do MQO --------------------------------------------------------------

MQO_more  <- lm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira+mulher*segunda, data = dados_titanic)
summary(MQO_more)
# MQO produzindo predições fora do intervalo (0,1):
# perceber que algumas variaveis que antes nao eram significativas, comecaram a ser aqui!
MQO_more|>predict()|>summary()
##########################################################

# Comparando os três modelos 
tab_model(Prob_more,Logit_more,MQO_more,
          dv.labels = c("Probit","Logit","MQO"),
          show.ci = F,
          transform = NULL
)

# Exibindo em odds-ratios-------------------------------------------------------
tab_model(Prob_more,Logit_more,
          show.ci = F)


# Pseudo R² (McFadden)
1 - Prob_more$deviance/Prob_more$null.deviance
1 - Logit_more$deviance/Logit_more$null.deviance 

Prob_irres <- glm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira+mulher*segunda, family = binomial(link = "probit"), data = dados_titanic)
Logit_irres  <- glm(formula = sobrev ~ mulher+primeira+segunda+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira+mulher*segunda, family = binomial(link = "logit"), data = dados_titanic)

summary(Prob_irres)
summary(Logit_irres)

# Pseudo R2
1 - Prob_irres$deviance/Prob_irres$null.deviance
1 - Logit_irres$deviance/Logit_irres$null.deviance 


# Modelo aninhado e Teste de Razão de Verossimilhanca:

Prob_res <- glm(formula = sobrev ~ mulher+primeira+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira, family = binomial(link = "probit"),
                  data = dados_titanic)
summary(Prob_res)

Logit_res <- glm(formula = sobrev ~ mulher+primeira+irmaos+parent+crianca+idoso+acompanhado+mulher*idoso+mulher*crianca+mulher*primeira, family = binomial(link = "logit"),
                data = dados_titanic)
summary(Logit_res)


#  Modelo Irrestrito  - Modelo Restrito     

lrtest(Prob_irres, Prob_res)
#Modelo 2 eh estatisticamente superior ao Modelo 1 em termos de ajuste aos dados
lrtest(Logit_irres, Logit_res)
# Curva ROC:

#grafico
pred.pro <- pROC::roc(dados_titanic$sobrev,fitted(Prob_res))
pred.log <- pROC::roc(dados_titanic$sobrev,fitted(Logit_res))

par(pty = "s")
plot.roc(pred.pro,
         print.auc = TRUE,
         legacy.axes = TRUE
)
par(new=T)
plot.roc(pred.log,
         print.auc = TRUE,
         legacy.axes = TRUE,
         print.auc.y = .4,
         add = TRUE,
         col=2
)

legend("bottomright", legend=c("Regressão Probit", "Regressão Logit"),
       col= c("black", "red"), lwd=2)
#bastante diferenca entre um e outro aqui hein

# Performance: 
table(true = dados_titanic$sobrev, pred = round(fitted(Prob_res)))
table(true = dados_titanic$sobrev, pred = round(fitted(Logit_res)))


# Usando matrizes robustas de heterocedastidade em Modelos de escolha discreta

library(AER)
coeftest(Prob_res, vcov = sandwich)
coeftest(Logit_res, vcov = sandwich)
