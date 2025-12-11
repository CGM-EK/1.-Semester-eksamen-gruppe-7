###############plot for information og kommunikation
inf_kom <- forbrugsdata %>% filter(FORMAAAL == "CPJ Information og kommunikation")
inf_kom <- inf_kom[35:59,]
inf_kom$year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2024-01-01"),
                 by = "year")
ggplot(data=inf_kom, aes(x=year, y=value, color = "CPJ Information og kommunikation"))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Forbruget på information og kommunikation er steget eksponensielt siden år 2000",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = inf_kom$year[seq(1,25, by = 1)],
               labels = format(inf_kom$year[seq(1,25, by = 1)], "%Y"))

###############plot for køb af køretøjer
køretøj <- forbrugsdata %>% filter(FORMAAAL == "CPH Køb af køretøjer")
køretøj <- køretøj[35:59,]
køretøj$year <- seq.Date(from = as.Date("2000-01-01"),
                         to = as.Date("2024-01-01"),
                         by = "year")
ggplot(data=køretøj, aes(x=year, y=value, color = "CPH Køb af køretøjer"))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Generelt falder forbruget på køretøjer med krisetider men stiger hurtigt igen",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = køretøj$year[seq(1,25, by = 1)],
               labels = format(køretøj$year[seq(1,25, by = 1)], "%Y"))
###############plot for medicin
medicin <- forbrugsdata %>% filter(FORMAAAL == "CPG Medicin, lægeudgifter o.l.")
medicin <- medicin[35:59,]
medicin$year <- seq.Date(from = as.Date("2000-01-01"),
                         to = as.Date("2024-01-01"),
                         by = "year")
ggplot(data=medicin, aes(x=year, y=value, color = "CPG Medicin, lægeudgifter o.l."))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Forbruget på medicin er steget eksponensielt fra 2000 til 2024 og ser ikke ud til at blive voldsomt påvirket af krisetider",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = medicin$year[seq(1,25, by = 1)],
               labels = format(medicin$year[seq(1,25, by = 1)], "%Y"))

###############plot for beklædning
beklæd <- forbrugsdata %>% filter(FORMAAAL == "CPC Beklædning og fodtøj")
beklæd <- beklæd[35:59,]
beklæd$year <- seq.Date(from = as.Date("2000-01-01"),
                         to = as.Date("2024-01-01"),
                         by = "year")
ggplot(data=beklæd, aes(x=year, y=value, color = "CPC Beklædning og fodtøj"))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Forbruget på beklædning er fordoblet over de sidste 24 år, der kan dog ses et dyk efter inflation",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = beklæd$year[seq(1,25, by = 1)],
               labels = format(beklæd$year[seq(1,25, by = 1)], "%Y"))
##############plot for forsikring og finansielle tjenester
forsik <- forbrugsdata %>% filter(FORMAAAL == "CPN Forsikring og finansielle tjenester")
forsik <- forsik[35:59,]
forsik$year <- seq.Date(from = as.Date("2000-01-01"),
                        to = as.Date("2024-01-01"),
                        by = "year")
ggplot(data=forsik, aes(x=year, y=value, color = "CPN Forsikring og finansielle tjenester"))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Efter voldsom fremgang faldt forbruget på finansielle tjenester drastisk efter finanskrisen og har haft stille fremgang efterfølgende",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = forsik$year[seq(1,25, by = 1)],
               labels = format(forsik$year[seq(1,25, by = 1)], "%Y"))
##############plot for boligudstyr
boligudstyr <- forbrugsdata %>% filter(FORMAAAL == "CPF Boligudstyr, husholdningstjenester mv.")
boligudstyr <- boligudstyr[35:59,]
boligudstyr$year <- seq.Date(from = as.Date("2000-01-01"),
                        to = as.Date("2024-01-01"),
                        by = "year")
ggplot(data=boligudstyr, aes(x=year, y=value, color = "CPF Boligudstyr, husholdningstjenester mv."))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Forbruget på boligudstyr og husholdningstejenster har haft fremgang, peakede under corona men faldt voldsomt efter inflation",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = boligudstyr$year[seq(1,25, by = 1)],
               labels = format(boligudstyr$year[seq(1,25, by = 1)], "%Y"))

##############plot for boligbenyttelse
bolig <- forbrugsdata %>% filter(FORMAAAL == "CPD Boligbenyttelse")
bolig <- bolig[35:59,]
bolig$year <- seq.Date(from = as.Date("2000-01-01"),
                             to = as.Date("2024-01-01"),
                             by = "year")
ggplot(data=bolig, aes(x=year, y=value, color = "CPD Boligbenyttelse"))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Forbruget på boligbenyttelse har haft en konstant vækst over de sidste 24 år",
       x = "årstal",
       y = "Kr brugt",
       color = "Kategori",
       caption = "Kilde:https://www.statistikbanken.dk/NAHC021")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(breaks = bolig$year[seq(1,25, by = 1)],
               labels = format(bolig$year[seq(1,25, by = 1)], "%Y"))
