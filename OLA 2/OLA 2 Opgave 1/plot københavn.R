københavn <- mergedcleanvilla %>% filter(kommune=="København")
københavn$pris <- as.numeric(gsub("[^0-9]","", københavn$pris))


summary(københavn$pris)

prisbreaks <- c(65000000,11187500,10002419,7724000,5595000,1995000,0)
prislabs=c("Meget billig", "Billig", "Almindelig pris", "Dyrt", "Meget dyrt")
københavn$priskategori <- cut(københavn$pris, labels = prislabs, breaks = prisbreaks)
københavnsmall <- københavn[c(5,8,24,29)]

#vi laver en ny dataframe der indeholder den gennemsnitlige kvmpris for bykategorien
table(københavnsmall$priskategori)

#tjekker antal observationer i hver kategori
table(boliger1.4$Bykategori)
prislabskbh=c("Meget billig (>2mil. kr.)", "Billig (2-5.5 mil. kr.)", "Almindelig pris (5.5-7.7 mil. kr.)", "Dyrt (7.7-10 mil. kr.)", "Meget dyrt (+10 mil. kr.)")
priskategorikbh <- c(109,107,88,20,109)


kbhdf <- data.frame(priskategori = factor(prislabskbh, levels = prislabskbh), n = priskategorikbh, labels = factor(prislabs, levels = prislabs))


ggplot(data = kbhdf, aes(x=labels, y=n, fill = priskategori))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/sum(n[1:5])*100,2),"%")),vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "København",
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Priskategori",
       x = "%")

