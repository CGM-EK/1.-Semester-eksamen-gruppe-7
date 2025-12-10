library(tidyverse)
library(ggplot2)
library(dkstat)
library(pls)
#vi henter data fra Danmarks statistik
forbrugerforv <- dst_meta(table = "FORV1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbrugerforv_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
f.tillid1 <- dst_get_data(table = "FORV1", query = forbrugerforv_meta_filters, lang = "da")
f.tillid1 <- f.tillid1 %>% filter(TID >="2000-01-01")

#Folder dataframe ud på de enkelte spg.
f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#Angiver spgnr. på kolonner
colnames(f.tillid)[2:14] <- c("FTI", "Spg1", "Spg2", "Spg3", "Spg4", "Spg8", "Spg5", "Spg6", "Spg7", "Spg9", "Spg10", "Spg11", "Spg12")

#Erstatter NA-værdier med værdier fra samme periode året før i spg. 10
f.tillid[305,12] <- f.tillid[305-4,12]
f.tillid[306,12] <- f.tillid[306-4,12]
f.tillid[307,12] <- f.tillid[307-4,12]
f.tillid[308,12] <- f.tillid[308-4,12]
f.tillid[309,12] <- f.tillid[309-4,12]
f.tillid[310,12] <- f.tillid[310-4,12]

##########Sætter kolonne 8,9,10 og 12 i negativ - Grundet negative værdier anses som positiv for privatforbruget
f.tillid[,c(8,9,10,12)] <- f.tillid[,c(8,9,10,12)]*-1

#Vi henter data for privatforbruget fra Danmarks statistik
p.forbrugss <- dst_meta(table = "NKN1", lang = "da")

#Vi udvælger variabler vi vil kigge på og opretter et dataset
pforbrug_meta_filters <- list(
  TRANSAKT = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)
p.forbrug1 <- dst_get_data(table = "NKN1", query = pforbrug_meta_filters, lang = "da")

#Vi filtrere dataframe på tid
p.forbrug <- p.forbrug1 %>% filter(TID >="1999-01-01")

#Forbrugertillidens data sættes i kvartaler for at gøre den sammenlignelig med privatforbruget
#kvartalersekvenser opsættes for at sætte forbrugertillid dataframen op
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#Kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerft1 <- f.tillid[c(kvartalseq1),3:ncol(f.tillid)]
kvartalerft2 <- f.tillid[c(kvartalseq2),3:ncol(f.tillid)]
kvartalerft3 <- f.tillid[c(kvartalseq3),3:ncol(f.tillid)]
forbrugertillid <- as.data.frame(c((kvartalerft1+kvartalerft2+kvartalerft3)/3))

#Der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-09-30"),
                 by = "quarter")
f.tillidsammen <- as.data.frame(year)

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]
#Opgave 3
kvartalseq1.1 <- seq(1,309, 3)
kvartalseq2.1 <- seq(2,310, 3)
kvartalseq3.1 <- seq(3,311, 3)

kvartalerft1.1 <- f.tillid[c(kvartalseq1.1),2]
kvartalerft2.1 <- f.tillid[c(kvartalseq2.1),2]
kvartalerft3.1 <- f.tillid[c(kvartalseq3.1),2]

####
colnames(optimalvektor) <- "opt"
f.tillidsammenk4$opt <- optimalvektor
tempf.tillid <- as.data.frame(c((kvartalerft1.1+kvartalerft2.1+kvartalerft3.1)/3))

f.tillidsammenk4 <- data.frame(pfv = f.tillidsammen$pfv,
                                  opt = optimalvektor,
                                  dst = tempf.tillid)
f.tillidsammenk4$YNPFV <- as.factor(ifelse(f.tillidsammenk4[,1] >=0, "1", "0"))
f.tillidsammenk4$YNOPT <- as.factor(ifelse(f.tillidsammenk4[,2] >=0, "1", "0"))
f.tillidsammenk4$YNDST <- as.factor(ifelse(f.tillidsammenk4[,3] >=0, "1", "0"))
f.tillidsammenk4$fittedvalues <- fitted.values(lm.opt)


glm.PFV.DST <- glm(formula = f.tillidsammenk4$YNPFV~f.tillidsammenk4$YNDST, family = "binomial")
summary(glm.PFV.DST)
glmdst <- c(as.numeric(glm.PFV.DST$fitted.values), 0)
f.tillidsammenk4$YNDSTfitted <- as.factor(ifelse(glmdst[-1] >=0.6, "1", "0"))

glm.PFV.OPT <- glm(formula = f.tillidsammenk4$YNPFV~f.tillidsammenk4$OPT, family = "binomial")
summary(glm.PFV.OPT)
glmOPT <- c(as.numeric(glm.PFV.OPT$fitted.values), 0)
f.tillidsammenk4$YNOPTfitted <- as.factor(ifelse(glmOPT[-1] >=0.6, "1", "0"))
table(predicted =f.tillidsammenk4$YNOPTfitted, actual = f.tillidsammenk4$YNPFV)
table(predicted =f.tillidsammenk4$YNDSTfitted, actual = f.tillidsammenk4$YNPFV)

table(f.tillidsammenk4$YNPFV)

40/103*100

estimatedst <- 0.2007
stddst <- 0.3178
predictionglm <- estimatedst+stddst*((f.tillid[310:311,2])/2)
table(predictionglm)
#DST's indikator forudser et fald i privatforbruget i forhold til forrige år

p <- exp(predictionglm)/(1+exp(predictionglm))
#vores validering er megeeet god

####fik chatten til at komme med et eksempel og jeg anvendte det så på vores data
dataframeglm <- data.frame(y= f.tillidsammenk4[1], x1=f.tillidsammenk4[2])
dataframeglm[3] <- as.factor(ifelse(dataframeglm[2] >=0, "1", "0"))
modelt <- glm(YNPFV ~ FTI, data = f.tillidsammenk4, family = binomial)
summary(modelt)

modelopt <- glm(YNPFV ~ opt, data = f.tillidsammenk4, family = binomial)
summary(modelopt)

new_datat <- data.frame(FTI = c(sum(f.tillid[310:311,2])/2))
pred_probst <- predict(modelt, newdata = new_datat, type = "response")
pred_probst
pred_classest <- ifelse(pred_probst > 0.5, 1, 0)
pred_classest
new_datat$pred_probt  <- pred_probst
new_datat$pred_classt <- pred_classest
print(new_datat)

new_datatopt <- data.frame(opt = c(sum(f.tillid[310:311,5])/2+
                             sum(f.tillid[310:311,7])/2+
                             sum(f.tillid[310:311,11])/2+
                              sum(f.tillid[310:311,13])/2+
                              sum(f.tillid[310:311,14])/2)/5)

pred_probst <- predict(modelopt, newdata = new_datatopt, type = "response")
pred_probst
pred_classest <- ifelse(pred_probst > 0.5, 1, 0)
pred_classest
new_datatopt$pred_probt  <- pred_probst
new_datatopt$pred_classt <- pred_classest
print(new_datatopt)

#######roc kurve
####DST
install.packages("pROC")
library(pROC)
pred_probstDST <- predict(modelt, type = "response")
roc_obj <- roc(f.tillidsammenk4$YNPFV, pred_probstOPT)

roc_df <- data.frame(
  tpr = roc_obj$sensitivities,
  fpr = 1-roc_obj$specificities
)
head(pred_probstOPT)
auc(roc_obj)
ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "darkolivegreen4", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(title = "DST.  Kurven viser en god klassifikationsevne, AUC = 0.79",
       x = "False Positive Rate",
       y = "True Positive Rate")

####DST
pred_probstOPT <- predict(modelopt, type = "response")
roc_objOPT <- roc(f.tillidsammenk4$YNPFV, pred_probstOPT)

roc_dfOPT <- data.frame(
  tpr = roc_objOPT$sensitivities,
  fpr = 1-roc_objOPT$specificities
)
head(pred_probstOPT)
auc(roc_objOPT)
ggplot(roc_dfOPT, aes(x = fpr, y = tpr)) +
  geom_line(color = "darkolivegreen4", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(title = "OPT.  Kurven viser en god klassifikationsevne, AUC = 0.82",
       x = "False Positive Rate",
       y = "True Positive Rate")
