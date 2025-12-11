
newhomes <- read.csv("C:/Users/viggo/Downloads/newhomes.csv")

#for at rense data skal vi starte med at fjerne alle observationer hvor der mangler værdier der gør boligen identificerbar

#fjerner alle NA. Fjernede 200 observationer 11900 til 11700
newhomestest <- na.omit(newhomes)

#der forekommer mange "%" i zip og de observationer hvor der forekommer fjernes derfor også
newhomestest2 <- newhomestest[!grepl("%", newhomestest$zip), ]

#4 boliger har ikke oplyst kvm og de fjernes også
newhomestest3 <- newhomestest2[!grepl("-", newhomestest2$kvm), ]

#mange 200 observationer indeholder ejerudgift i deres kvm, dette fjernes også
newhomestest4 <- newhomestest3[!grepl("Ejerudg", newhomestest3$kvm), ]

#mange boliger har ikke oplyst grundstørrelse og de fjernes også
newhomestest5 <- newhomestest4[!grepl("-", newhomestest4$grund), ]

#der er en enkelt bolig uden oplyst antal værelser
newhomestest6 <- newhomestest5[!grepl("-", newhomestest5$vaer), ]

#da vi gerne vil kigge på villaer sorterer vi alle ikke-villaer fra
newhomesclean <- newhomestest6[grepl("^Villa$", newhomestest6$type), ]

#######################

#vi vil nu gerne have fat i de kolonner der anses for relevante i vores undersøgelse

#kolonnen for postnumre har flere informationer end postnummer men da vi gerne 
#vil kigge på postnumre sorterer vi de andre informationer fra som bynavn og postnummernavn
zip <- gsub("[^0-9]","", newhomesclean$zip)
unique(zip)

#udtrykket energimærke fjernes fra observationen da den er overflødig
energimærke <- sub(".*Energimærke ", "", newhomesclean$energi)
unique(energimærke)

#for at gøre prisen, kvm, ejerudgiften og grundstørrelsen numerisk fjernes alt
#undtagen tal dette ødelægger ikke tallene da der kun er heltal i observationerne
pris <- as.numeric(gsub("[^0-9]","", newhomesclean$pris))
kvm <- as.numeric(gsub("[^0-9]","", newhomesclean$kvm))
ejerudg <- as.numeric(gsub("[^0-9]","", newhomesclean$ejerudg))
grundstr <- as.numeric(gsub("[^0-9]","", newhomesclean$grund))

#lignende metode gør vi for antal værelser og opførelsesår dog ikke som numerisk
vaer <- gsub("[^0-9]","", newhomesclean$vaer)
alder <- gsub("[^0-9]","", newhomesclean$alder)

#vi vil også have en ny variabel som er pris/kvm
kvmpris <- pris/kvm

#det hele sættes sammen til en ny ren dataframe
nhcl <- data.frame(
  zip,
  energimærke,
  pris,
  kvm,
  kvmpris,
  ejerudg,
  grundstr,
  vaer,
  alder=2024-as.numeric(alder)
)
nhcl$alder <- 2024-nhcl$alder

########################################

lmpriskvmpris <- lm(data = nhcl, kvmpris~pris)
summary(lmpriskvmpris)
cor(nhcl$kvmpris,lmpriskvmpris$fitted.value)

lmpriskvmkvm <- lm(data = nhcl, kvmpris~kvm)
summary(lmpriskvmkvm)
cor(nhcl$kvmpris,lmpriskvmkvm$fitted.value)

lmpriskvmejerudg <- lm(data = nhcl, kvmpris~ejerudg)
summary(lmpriskvmejerudg)
cor(nhcl$kvmpris,lmpriskvmejerudg$fitted.value)

lmpriskvmgrundstr <- lm(data = nhcl, kvmpris~grundstr)
summary(lmpriskvmgrundstr)
cor(nhcl$kvmpris,lmpriskvmgrundstr$fitted.value)

lmpriskvmald <- lm(data=nhcl, kvmpris~alder)
summary(lmpriskvmald)
cor(nhcl$kvmpris,lmpriskvmald$fitted.value)

#for at gøre energimærket sammenligneligt bliver vi nødt til at gøre det til en faktor
#der laves en vektor med rangeringen af energinumre
energy_order <- c("G", "F", "E", "D", "C", "B", "A", "A2010", "A2015")

#den laves om til en faktorvariabel med levels defineret ud fra rækkefølgen i vektoren
nhcl$energi_faktor <- factor(nhcl$energimærke, levels = energy_order, ordered = TRUE)
library(ordinal)
lmpriskvm <- clm(data = nhcl, energi_faktor~log(kvmpris))
summary(lmpriskvm)