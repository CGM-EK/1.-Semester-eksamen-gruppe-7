#loader pakker til brug i opgaven
library(readxl)
library(tidyverse)
library(pROC)

#f.tillid <- read_excel("R/R projekter/Forbrugertillidsindikator2000_2025 OLA2.xlsx")
#vi henter data fra Danmarks statistik
forbrugerforv <- dst_meta(table = "FORV1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbrugerforv_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
f.tillid <- dst_get_data(table = "FORV1", query = forbrugerforv_meta_filters, lang = "da")
f.tillid <- f.tillid %>% filter(TID >="2000-01-01")

f.tillid <- pivot_wider(
  data = f.tillid,
  names_from = INDIKATOR,
  values_from = value)

###########

p.forbrugss <- dst_meta(table = "NKN1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
pforbrug_meta_filters <- list(
  TRANSAKT = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)
p.forbrug1 <- dst_get_data(table = "NKN1", query = pforbrug_meta_filters, lang = "da")
p.forbrug <- p.forbrug1 %>% filter(TID >="1999-01-01")

f.forbrug <- pivot_wider(
  data = p.forbrug,
  names_from = TRANSAKT,
  values_from = value)



########################################################

#oprettelse af faktorvariabel der viser om det kvartalvise årlige vækst er steget eller faldet
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-09-30"),
                 by = "quarter")
dfopg3 <- as.data.frame(year)

P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
dfopg3$pfv <- P.forbrugvaekst[-1]
dfopg3$YN <- as.factor(ifelse(Dfopg3$pfv >=0, "1", "0"))
table(Dfopg3$YN)
# no yes 
# 24 79

#udregning i procent
noprocent <- table(dfopg3$YN)[1]/(table(dfopg3$YN)[1]+table(dfopg3$YN)[2])*100
yesprocent <- 100-noprocent

############################################
#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,309, 3)
kvartalseq2 <- seq(2,310, 3)
kvartalseq3 <- seq(3,311, 3)

#variabler defineres for DI og DST's forbrugertillidsindikatorer i f.tillid
f.tillid$sammenlagtDI <- c((f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                              f.tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4)

f.tillid$sammenlagtDST <- c((f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                               f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`+
                               f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)/5)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerDIft1 <- f.tillid$sammenlagtDI[kvartalseq1]
kvartalerDIft2 <- f.tillid$sammenlagtDI[kvartalseq2]
kvartalerDIft3 <- f.tillid$sammenlagtDI[kvartalseq3]
forbrugertillidDI <- as.data.frame(c((kvartalerDIft1+kvartalerDIft2+kvartalerDIft3)/3))

kvartalerDSTft1 <- f.tillid$sammenlagtDST[kvartalseq1]
kvartalerDSTft2 <- f.tillid$sammenlagtDST[kvartalseq2]
kvartalerDSTft3 <- f.tillid$sammenlagtDST[kvartalseq3]
forbrugertillidDST <- as.data.frame((kvartalerDSTft1+kvartalerDSTft2+kvartalerDSTft3)/3)

#der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-09-30"),
                 by = "quarter")
f.tillidsammen <- as.data.frame(year)

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]
f.tillidsammen$f.tillidDI <- forbrugertillidDI$`c((kvartalerDIft1 + kvartalerDIft2 + kvartalerDIft3)/3)`
f.tillidsammen$f.tillidDST <- forbrugertillidDST$`(kvartalerDSTft1 + kvartalerDSTft2 + kvartalerDSTft3)/3`

#lineære modeller for dst og di's forbrugertillidsindikatorer med fitted values og korrelation
lm.test.di <- lm(f.tillidsammen$pfv~f.tillidsammen$f.tillidDI)
summary(lm.test.di)
fitted.lm.test.di <- lm.test.di$fitted.values
cor(fitted.lm.test.di,f.tillidsammen$pfv)

lm.test.dst <- lm(f.tillidsammen$pfv~f.tillidsammen$f.tillidDST)
summary(lm.test.dst)
fitted.lm.test.dst <- lm.test.dst$fitted.values
cor(fitted.lm.test.dst,f.tillidsammen$pfv)

##
#indsætter de fittede værdier fra forbrugertillidsindikatorerne fra DST og DI i dataframen
dfopg3$DI.fitted <- fitted.lm.test.di
dfopg3$DST.fitted <- fitted.lm.test.dst


dfopg3$YNDI <- as.factor(ifelse(Dfopg3$DI.fitted >=0, "1", "0"))
table(dfopg3$YNDI)
#no  Yes 
#20  83 

#udregning i procent
noprocent1 <- table(dfopg3$YNDI)[1]/(table(dfopg3$YNDI)[1]+table(dfopg3$YNDI)[2])*100
yesprocent1 <- 100-noprocent1

dfopg3$YNDST <- as.factor(ifelse(dfopg3$DST.fitted >=0, "1", "0"))
table(Dfopg3$YNDST)
#no  Yes
#17  86 

#udregning i procent
noprocent1 <- table(dfopg3$YNDST)[1]/(table(dfopg3$YNDST)[1]+table(dfopg3$YNDST)[2])*100
yesprocent1 <- 100-noprocent1

glm.DIPFV <- glm(formula = dfopg3$YN~dfopg3$YNDI, family = "binomial")
summary(glm.DIPFV)

pred_probsDI <- predict(glm.DIPFV, type = "response")
threshold <- 0.5
pred_classDI <- ifelse(pred_probsDI > threshold,1, 0)
table(predicted =pred_classDI, actual = dfopg3$YN)

glm.DSTPFV <- glm(formula = dfopg3$YN~dfopg3$YNDST, family = "binomial")
summary(glm.DSTPFV)

pred_probsDST <- predict(glm.DSTPFV, type = "response")
pred_classDST <- ifelse(pred_probsDST > threshold,1, 0)
table(predicted =pred_classDST, actual = dfopg3$YN)

#ROC 

roc_obj <- roc(dfopg3$YN, pred_probsDI)

roc_df <- data.frame(
  tpr = roc_obj$sensitivities,
  fpr = 1 - roc_obj$specificities
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(title = "Kurven viser en god klassifikationsevne",
       x = "False Positive Rate",
       y = "True Positive Rate")

auc(roc_obj)

#Forudsigelse

summary(glm.DIPFV)
-0.8473     0.4879

diynpredict <- -0.8473+0.4879*sum((f.tillid$sammenlagtDI[310:311])/2)

summary(glm.DSTPFV)
-0.6061     0.5075

dstynpredict <- -0.6061+0.5075*sum((f.tillid$sammenlagtDI[310:311])/2)
jan <- (sum(f.tillidsammen$f.tillidDI[102:103]))/2
janlaverballade <- c(f.tillidsammen$f.tillidDI, jan)
pred_probsDI <- predict(glm.DIPFV,newdata = as.data.frame(janlaverballade), type = "response")


