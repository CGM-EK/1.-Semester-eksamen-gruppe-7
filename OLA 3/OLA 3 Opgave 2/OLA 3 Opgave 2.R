#### PCR.fit ####
install.packages("pls")
library(pls)

pcr.fit <- pcr(f.tillidsammen$pfv ~ forbrugertillid$Spg1+
                 forbrugertillid$Spg2+
                 forbrugertillid$Spg3+
                 forbrugertillid$Spg4+
                 forbrugertillid$Spg5+
                 forbrugertillid$Spg6+
                 forbrugertillid$Spg7+
                 forbrugertillid$Spg8+
                 forbrugertillid$Spg9+
                 forbrugertillid$Spg10+
                 forbrugertillid$Spg11+
                 forbrugertillid$Spg12,
               validation = "CV", scale = T)
summary(pcr.fit)

loadings.pcr.fit <- pcr.fit$loadings

validationplot(pcr.fit, val.type = "MSEP")

w.indicators1 <- as.data.frame(loadings.pcr.fit[1:12, 2]^2)
sum(w.indicators1)

w.indicators1$spg <- c("Spg1", "Spg2", "Spg3", "Spg4", "Spg5", "Spg6", "Spg7", "Spg8", "Spg9", "Spg10", "Spg11", "Spg12")
w.indicators1$spg <- factor(w.indicators1$spg,
                            levels = paste0("Spg", 1:12))

christian <- data.frame(spg1=forbrugertillid$Spg1*as.numeric(w.indicators1[1,1]),
                           spg2=forbrugertillid$Spg2*as.numeric(w.indicators1[2,1]),
                           spg3=forbrugertillid$Spg3*as.numeric(w.indicators1[3,1]),
                           spg4=forbrugertillid$Spg4*as.numeric(w.indicators1[4,1]),
                           spg5=forbrugertillid$Spg5*as.numeric(w.indicators1[5,1]),
                           spg6=forbrugertillid$Spg6*as.numeric(w.indicators1[6,1]),
                           spg7=forbrugertillid$Spg7*as.numeric(w.indicators1[7,1]),
                           spg8=forbrugertillid$Spg8*as.numeric(w.indicators1[8,1]),
                           spg9=forbrugertillid$Spg9*as.numeric(w.indicators1[9,1]),
                           spg10=forbrugertillid$Spg10*as.numeric(w.indicators1[10,1]),
                           spg11=forbrugertillid$Spg11*as.numeric(w.indicators1[11,1]),
                           spg12=forbrugertillid$Spg12*as.numeric(w.indicators1[12,1]), 
                           pfv=f.tillidsammen$pfv)

lmvægt <- lm(data=christian, pfv ~ spg1+spg2+spg3+spg4+spg5+spg6+spg7+spg8+spg9+spg10+spg11+spg12)
summary(lmvægt)
summarise(lmvægt)




ggplot(data = w.indicators1, aes(x = spg[1:12], y = w.indicators1[,1]))+
  geom_bar(fill = "darkolivegreen4", stat = "identity")+
  ylab("Vægt")+xlab("Spørgsmål")+ labs(title = "Vægtning af spørgsmål")+
  theme_minimal()

# pcr.fit på spg for den optimale indikator fra opg. 1
pcr.fit.opt <- pcr(f.tillidsammen$pfv ~
                     forbrugertillid$Spg3+
                     forbrugertillid$Spg8+
                     forbrugertillid$Spg9+
                     forbrugertillid$Spg11+
                     forbrugertillid$Spg12,
                   validation = "CV", scale = T)
summary(pcr.fit.opt)

loadings.pcr.fit.opt <- pcr.fit.opt$loadings

validationplot(pcr.fit.opt, val.type = "MSEP")

w.indicators1opt <- as.data.frame(loadings.pcr.fit.opt[1:5, 2]^2)
sum(w.indicators1opt)

w.indicators1opt$spg <- c("Spg3", "Spg8", "Spg9", "Spg11", "Spg12")
w.indicators1opt$spg <- factor(w.indicators1opt$spg,
                               levels = paste0("Spg", 1:12))

ggplot(data = w.indicators1opt, aes(x = spg[1:5], y = w.indicators1opt[,1]))+
  geom_bar(fill = "darkolivegreen4", stat = "identity")+
  ylab("Vægt")+xlab("Spørgsmål")+ labs(title = "Vægtning af spørgsmål på den optimale indikator")+
  theme_minimal()

#pfv
optimalvektor <- as.data.frame(forbrugertillid$Spg3*as.numeric(w.indicators1opt[1,1])+
                                 forbrugertillid$Spg8*as.numeric(w.indicators1opt[2,1])+
                                 forbrugertillid$Spg9*as.numeric(w.indicators1opt[3,1])+
                                 forbrugertillid$Spg11*as.numeric(w.indicators1opt[4,1])+
                                 forbrugertillid$Spg12*as.numeric(w.indicators1opt[5,1]))

lm.opt.vægt <- lm(f.tillidsammen$pfv ~ optimalvektor[,1])
summary(lm.opt.vægt)

spgsss <- (sum(f.tillid$Spg3[310:311])/2*as.numeric(w.indicators1opt[1,1]))+
             (sum(f.tillid$Spg8[310:311])/2*as.numeric(w.indicators1opt[2,1]))+
             (sum(f.tillid$Spg9[310:311])/2*as.numeric(w.indicators1opt[3,1]))+
             (sum(f.tillid$Spg11[310:311])/2*as.numeric(w.indicators1opt[4,1]))+
             (sum(f.tillid$Spg12[310:311])/2*as.numeric(w.indicators1opt[5,1]))

k4opt <- optimalvektor[99,1]
#definerer koefficienter for den lm
stdopt <- 1.2646
estimateopt <- -8.4992


#forudsigelse for k4
pfvforudsigelsek4opt <- estimateopt+stdopt*spgsss

allspg <- (sum(f.tillid$Spg1[310:311])/2*as.numeric(w.indicators1[1,1]))+
  (sum(f.tillid$Spg2[310:311])/2*as.numeric(w.indicators1[2,1]))+
  (sum(f.tillid$Spg3[310:311])/2*as.numeric(w.indicators1[3,1]))+
  (sum(f.tillid$Spg4[310:311])/2*as.numeric(w.indicators1[4,1]))+
  (sum(f.tillid$Spg5[310:311])/2*as.numeric(w.indicators1[5,1]))+
  (sum(f.tillid$Spg6[310:311])/2*as.numeric(w.indicators1[6,1]))+
  (sum(f.tillid$Spg7[310:311])/2*as.numeric(w.indicators1[7,1]))+
  (sum(f.tillid$Spg8[310:311])/2*as.numeric(w.indicators1[8,1]))+
  (sum(f.tillid$Spg9[310:311])/2*as.numeric(w.indicators1[9,1]))+
  (sum(f.tillid$Spg10[310:311])/2*as.numeric(w.indicators1[10,1]))+
  (sum(f.tillid$Spg11[310:311])/2*as.numeric(w.indicators1[11,1]))+
  (sum(f.tillid$Spg12[310:311])/2*as.numeric(w.indicators1[12,1]))

std1 <- 4.99266
est1 <- 5.04872

pfvforudsigelsek4 <- est1+std1*allspg





# Opgave 2.4
pfv2015 <- sum(p.forbrug[65:68,5])
pfv2016 <- sum(p.forbrug[69:72,5])
dif <- (pfv2016/pfv2015*100)-100
# PFV faktiskevægt fra 2015 til 2016 = 3.28%
