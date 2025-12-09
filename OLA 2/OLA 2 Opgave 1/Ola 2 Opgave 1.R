#pakker loades til brug i opgaven
library(danstat)
library(mapDK)
library(tidyverse)
library(dkstat)

install.packages(
  "dkstat",
  repos = c(
    ropengov = "https://ropengov.r-universe.dev",
    getOption("repos")
  )
)


#Vi henter data fra Danmarks statistik
dkbefolkningpost <- dst_meta(table = "POSTNR2", lang = "da")

#Vi udvælger variabler vi vil kigge på og opretter et dataset
dkbefolk_meta_filters <- list(
  PNR20 = "*",
  Tid = "2025"
)
befolkningsdata <- dst_get_data(table = "POSTNR2", query = dkbefolk_meta_filters, lang = "da")

#Klargøre en ny df til cleaning
befolkningsdatacl <- befolkningsdata


#Opretter kolonne til postnummer baseret på PNR20 kolonnen
befolkningsdatacl$mzip <- sub(".* - ", "", befolkningsdata$PNR20) #fjerner alt før "-"
befolkningsdatacl$mzip <- as.numeric(gsub("[^0-9]","", befolkningsdatacl$mzip)) #fjerner alt på nær tal

#Opretter kolonne til bynavn baseret på PNR20 kolonnen
befolkningsdatacl$by <- sub(".* - ", "", befolkningsdata$PNR20) #fjerner alt før "-"
befolkningsdatacl$by <- gsub("[-^0-9]","", befolkningsdatacl$by) #fjerner alle tal
befolkningsdatacl$by <- gsub(" ","", befolkningsdatacl$by) #fjerner mellemrum før udtryk

#Fjerner "Hele landet" fra dataframen
befolkningsdatacl <- befolkningsdatacl %>% filter(by!="Helelandet")

#vi fjerner alle observationer hvor der ikke er nogle beboere
befolkningsdatacl <- as.data.frame(subset(befolkningsdatacl, befolkningsdatacl$value != 0))

############################################
#vi indlæser datafil for boliger

url <- "https://raw.githubusercontent.com/CGM-EK/1.-Semester-eksamen-gruppe-7/main/OLA%202/OLA%202%20Opgave%201/boligcl2.rds"

boligcl2 <- readRDS(url(url))

#laver et nyt dataframe til at arbejde i
boligclean <- boligcl2

#opretter ny prisvariabel i kr. med kun tal
boligclean$prisny <- as.numeric(gsub("[^0-9]","", boligclean$pris))

#opretter en ny variabel med kvm/pris
boligclean$kvmpris <- boligclean$prisny/boligclean$kvm2

#oprettter et dataframe med totale befolkningstal pr by
combinedby <- befolkningsdatacl %>% 
  group_by(by) %>% 
  summarise(total=sum(value),.groups = "drop")

#vi merger for at få befolkningstal for byerne ind i dataframet
mergedboligtemp <- left_join(befolkningsdatacl, combinedby, by = "by")

#vi merger vores to dataset på postnummer
mergedbolig <- inner_join(mergedboligtemp, boligclean, by = "mzip")

#vi laver en ny dataframe uden dublikater ud fra boligID
mergedclean <- mergedbolig %>% distinct(mergedbolig$bolig_id, .keep_all = TRUE)

#vi undersøger befolkningstalet fra de forskellige byer
summary(mergedclean$total)

#vi definerer vores breaks og labels til den nye bykategorivariabel
mybreaks=c(160000,40000,10000, 2500, 1000,0)
mylabs=c("Landsby (<1.000)","Lille by (1.001-2.500)", "Almindelig by (2.501-10.000)", "Større by (10.001-40.000)", "Storby (40.001+)")

#den nye kategorivariabel defineres
mergedclean$Bykategori <- cut(mergedclean$total, labels = mylabs, breaks = mybreaks)

#vi laver så den endelige dataframe hvor vi har vores by, pris, kvmpris, og bykategori
boliger1.4 <- mergedclean[c(5,8,24,26)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
combinedkvm <- boliger1.4 %>% 
  group_by(Bykategori) %>% 
  summarise(kvmpris=mean(kvmpris),.groups = "drop")

#Tjekker antal observationer i hver kategori
table(boliger1.4$Bykategori)

#kategori_n defineres ud fra ovenstående table
kategori_n <- c(table(boliger1.4$Bykategori)[1],table(boliger1.4$Bykategori)[2],table(boliger1.4$Bykategori)[3],table(boliger1.4$Bykategori)[4],table(boliger1.4$Bykategori)[5])

#Kategori tilføjes til combinedkvm
combinedkvm$n <- kategori_n

#vi ploter den gennemsnitlige kvmpris for bykategorierne
ggplot(data = combinedkvm, aes(x=Bykategori, y=kvmpris, fill = Bykategori))+
  geom_bar(stat = "identity")+ 
  geom_text(aes(label = paste0("n = ", n)),
            vjust = -0.5,
            size = 3) +
  theme_minimal()+ 
  theme(legend.position = "none") +
  labs(title = "Kvadratmeterpriser stiger med indbyggertal", caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/")+ 
  ylab("Gennemsnitlig kvadratmeterpris i Kr.")+ 
  xlab("Bykategori baseret på indbyggertal")


#############################
mergedcleanvilla <- mergedclean %>% filter(type=="Villa")
boliger1.4 <- mergedcleanvilla[c(5,8,11,24,26)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
combinedkvmpris <- boliger1.4 %>% 
  group_by(Bykategori) %>% 
  summarise(kvmpris=mean(kvmpris),.groups = "drop")

#vi ploter den gennemsnitlige kvmpris for bykategorierne
ggplot(data = combinedkvmpris, aes(x=Bykategori, y=kvmpris, fill = Bykategori))+
  geom_bar(stat = "identity")+ 
  geom_text(aes(label = paste0("n = ", format(round(kvmpris), big.mark = ".",))),
            vjust = -0.5,
            size = 3) +
  theme_minimal()+ 
  theme(legend.position = "none") +
  labs(title = "Kvadratmeterpriser stiger med indbyggertal", caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/")+ 
  ylab("Gennemsnitlig kvadratmeterpris i Kr.")+ 
  xlab("Bykategori baseret på indbyggertal")

#Load mapDK
library(devtools)
devtools::install_github("sebastianbarfort/mapDK")
library(mapDK)

postnr_mean <- mergedcleanvilla %>% 
  group_by(mzip) %>% 
  summarise(
    mean_kvmpris = mean(kvmpris, na.rm = TRUE),
    n = n()
  )
#Heatmap over kvmpris på kommuner
mapDK(
  values      = "mean_kvmpris",  # kolonnen med gennemsnitspris
  id          = "mzip",        # din postnummer-kolonne
  data        = postnr_mean,
  detail      = "zip",           # kortniveau = postnumre
  guide.label = "Gns. pris (kr/m²)",
  map.title = "De højeste kvmpriser ses omkring østkysten af nordsjælland og københavn",
  map.fill = "green",
  map.colour = "black"
)+
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    name = "Gns. pris\n(kr/m²)",
    na.value = "grey95"
  )

#Opretter en kolonne med kommunenavn
mergedcleanvilla$kommune <- word(mergedcleanvilla$PNR20, 3)

#PLOT ÅRHUS

#Opretter dataframe for villadata i århus
aarhus <- mergedcleanvilla %>% filter(kommune=="Århus")
#Cleaner pris kolonne 
aarhus$pris <- as.numeric(gsub("[^0-9]","", aarhus$pris))

#laver summary som bruges til at definere breaks
summary(aarhus$pris)

#definerer breaks
prisbreaksaa <- c(summary(aarhus$pris)[6],summary(aarhus$pris)[5],summary(aarhus$pris)[4],summary(aarhus$pris)[3],summary(aarhus$pris)[2],0)
#navngiver breaks
prislabs=c("Meget billig", "Billig", "Almindelig pris", "Dyrt", "Meget dyrt")
#inputter breaks i aarhus dataframe
aarhus$priskategori <- cut(aarhus$pris, labels = prislabs, breaks = prisbreaksaa)
#laver en mindre dataframe som bruges til plots
aarhussmall <- aarhus[c(5,8,24,28)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
table(aarhussmall$priskategori)

#tjekker antal observationer i hver kategori
table(boliger1.4$Bykategori)
prislabsaa=c("Meget billig (<3,5 mil. kr.)", "Billig (3,5-4,8 mil. kr.)", "Almindelig pris (4,8-5,5 mil. kr.)", "Dyrt (5,5-6,5 mil. kr.)", "Meget dyrt (+6,5 mil. kr.)")

priskategoriaa <- c(table(aarhussmall$priskategori)[1],table(aarhussmall$priskategori)[2],table(aarhussmall$priskategori)[3],table(aarhussmall$priskategori)[4],table(aarhussmall$priskategori)[5])

aadf <- data.frame(labels = factor(prislabs,levels = prislabs), n = priskategoriaa, priskategori = factor(prislabsaa, levels = prislabsaa))

#vi ploter den gennemsnitlige kvmpris for bykategorierne
ggplot(data = aadf, aes(x=labels, y=n, fill = priskategori))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/sum(n[1:5])*100,2),"%")), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Midterpriser er sjældne: Århusianske villaer fordeler sig i yderkategorierne",
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Antal observationer",
       x = "Priskategori")

#PLOT KØBENHAVN

#Opretter dataframe for villadata i København
københavn <- mergedcleanvilla %>% filter(kommune=="København")

#Cleaner pris kolonne
københavn$pris <- as.numeric(gsub("[^0-9]","", københavn$pris))

#laver summary som bruges til at lave breaks
summary(københavn$pris)

prisbreaks <- c(summary(københavn$pris)[6],summary(københavn$pris)[5],summary(københavn$pris)[4],summary(københavn$pris)[3],summary(københavn$pris)[2],0)
prislabs=c("Meget billig", "Billig", "Almindelig pris", "Dyrt", "Meget dyrt")
københavn$priskategori <- cut(københavn$pris, labels = prislabs, breaks = prisbreaks)
københavnsmall <- københavn[c(5,8,24,28)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
table(københavnsmall$priskategori)

#tjekker antal observationer i hver kategori
table(boliger1.4$Bykategori)
prislabskbh=c("Meget billig (<5.5mil. kr.)", "Billig (5,5-7,7 mil. kr.)", "Almindelig pris (7.7-10 mil. kr.)", "Dyrt (10-11,1 mil. kr.)", "Meget dyrt (+11,1 mil. kr.)")
priskategorikbh <- c(table(københavnsmall$priskategori)[1],table(københavnsmall$priskategori)[2],table(københavnsmall$priskategori)[3],table(københavnsmall$priskategori)[4],table(københavnsmall$priskategori)[5])


kbhdf <- data.frame(priskategori = factor(prislabskbh, levels = prislabskbh), n = priskategorikbh, labels = factor(prislabs, levels = prislabs))


ggplot(data = kbhdf, aes(x=labels, y=n, fill = priskategori))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/sum(n[1:5])*100,2),"%")),vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Københavnske villaer fordeler sig primært i yderkategorierne af pris",
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Antal observationer",
       x = "Priskategori")

#PLOT ODENSE
odense <- mergedcleanvilla %>% filter(kommune=="Odense")
odense$pris <- as.numeric(gsub("[^0-9]","", odense$pris))


summary(odense$pris)

prisbreaksod <- c(summary(odense$pris)[6],summary(odense$pris)[5],summary(odense$pris)[4],summary(odense$pris)[3],summary(odense$pris)[2],0)
prislabs=c("Meget billig", "Billig", "Almindelig pris", "Dyrt", "Meget dyrt")
odense$priskategori <- cut(odense$pris, labels = prislabs, breaks = prisbreaksod)
odensesmall <- odense[c(5,8,24,28)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
table(odensesmall$priskategori)

prislabsod=c("Meget billig (>2.2 mil. kr.)", "Billig (2.2-3 mil. kr.)", "Almindelig pris (3-3.6 mil. kr.)", "Dyrt (3.6-4.3 mil. kr.)", "Meget dyrt (+4.3 mil. kr.)")

priskategoriod <- c(table(odensesmall$priskategori)[1],table(odensesmall$priskategori)[2],table(odensesmall$priskategori)[3],table(odensesmall$priskategori)[4],table(odensesmall$priskategori)[5])

oddf <- data.frame(labels = factor(prislabs,levels = prislabs), n = priskategoriod, priskategori = factor(prislabsod, levels = prislabsod))

#PLOT ODENSE
ggplot(data = oddf, aes(x=labels, y=n, fill = priskategori))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/sum(n[1:5])*100,2),"%")), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Midterpriser fylder langt mindre på villamarkedet i Odense",
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Antal observatioenr",
       x = "Priskategori")


