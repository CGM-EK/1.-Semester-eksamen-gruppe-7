odense <- mergedcleanvilla %>% filter(kommune=="Odense")
odense$pris <- as.numeric(gsub("[^0-9]","", odense$pris))


summary(odense$pris)

prisbreaksod <- c(16500000,4295000,3604170,2935000,2195000,495000 )
prislabs=c("Meget billig", "Billig", "Almindelig pris", "Dyrt", "Meget dyrt")
odense$priskategori <- cut(odense$pris, labels = prislabs, breaks = prisbreaksod)
odensesmall <- odense[c(5,8,24,29)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
table(odensesmall$priskategori)

prislabsod=c("Meget billig (>2.2 mil. kr.)", "Billig (2.2-3 mil. kr.)", "Almindelig pris (3-3.6 mil. kr.)", "Dyrt (3.6-4.3 mil. kr.)", "Meget dyrt (+4.3 mil. kr.)")

priskategoriod <- c(149,138,82,65,141)

oddf <- data.frame(labels = factor(prislabs,levels = prislabs), n = priskategoriod, priskategori = factor(prislabsod, levels = prislabsod))

#vi ploter den gennemsnitlige kvmpris for bykategorierne
ggplot(data = oddf, aes(x=labels, y=n, fill = priskategori))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/sum(n[1:5])*100,2),"%")), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Odense",
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Priskategori",
       x = "%")
