aarhus <- mergedcleanvilla %>% filter(kommune=="Århus")
aarhus$pris <- as.numeric(gsub("[^0-9]","", aarhus$pris))


summary(aarhus$pris)

prisbreaksaa <- c(42000000,6695000,5874128,4995000,3595000,1249000)
prislabs=c("Meget billig", "Billig", "Almindelig pris", "Dyrt", "Meget dyrt")
aarhus$priskategori <- cut(aarhus$pris, labels = prislabs, breaks = prisbreaksaa)
aarhussmall <- aarhus[c(5,8,24,29)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
table(aarhussmall$priskategori)

#tjekker antal observationer i hver kategori
table(boliger1.4$Bykategori)
prislabsaa=c("Meget billig (>1.25mil. kr.)", "Billig (1.25-3.5 mil. kr.)", "Almindelig pris (3.5-5 mil. kr.)", "Dyrt (5-6 mil. kr.)", "Meget dyrt (+7 mil. kr.)")

priskategoriaa <- c(202,210,116,73,200)

aadf <- data.frame(labels = factor(prislabs,levels = prislabs), n = priskategoriaa, priskategori = factor(prislabsaa, levels = prislabsaa))

#vi ploter den gennemsnitlige kvmpris for bykategorierne
ggplot(data = aadf, aes(x=labels, y=n, fill = priskategori))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/sum(n[1:5])*100,2),"%")), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Århus",
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Priskategori",
       x = "%")
