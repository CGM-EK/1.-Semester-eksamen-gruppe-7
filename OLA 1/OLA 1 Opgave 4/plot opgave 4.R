library(tidyverse)
library(ggplot2)
library(dkstat)
#vi henter data fra Danmarks statistik
forbruggrupper <- dst_meta(table = "FU02", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbruggrupperfilter <- list(
  KONSUMGRP =  c("02.1.1.1 Spiritus og likør",                                           
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
"02.1.2.3 Hedvin",
"02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
"02.1.3.1 Pilsnerøl, guldøl",
"02.1.3.2 Andre alkoholholdige øl",
"02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
"02.1.3.4 Øl-baserede drikkevarer",
"02.2.0.1 Cigaretter",
"02.2.0.2 Cigarer o.l.",
"02.2.0.3 Andre tobaksvarer",
"02.3.0.0 Euforiserende stoffer"),
  PRISENHED = "Faste priser",
  Tid = "*"
)
alkohol <- dst_get_data(table = "FU02", query = forbruggrupperfilter, lang = "da")

alkohol$KONSUMGRP <- gsub("[^A-ZÆØÅa-zæøå ]","",alkohol$KONSUMGRP)

alkohol$TID <- gsub("-.*","",alkohol$TID)

alkoholplot <- as.data.frame(alkohol %>% pivot_wider(names_from = "KONSUMGRP", values_from = "value"))

#Spiritus og likør     

ggplot(alkoholplot, aes(x=TID, y =`  Spiritus og likør`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på Spiritus og likør steg og faldt med finanskrisen og har siden fundet vej tilbage",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Alkoholiske læskedrikke
ggplot(alkoholplot, aes(x=TID, y =`  Alkoholiske læskedrikke`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på alkoholiske læskedrikke steg og faldt med finanskrisen og har siden fundet vej tilbage",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Vin af druer
ggplot(alkoholplot, aes(x=TID, y =`  Vin af druer`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på vin af druer stiger og falder voldsomt og havde højdepunkt i 2009",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Vin af andre frugter
ggplot(alkoholplot, aes(x=TID, y =`  Vin af andre frugter`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på vin af andre frugter peakede i 2011 og er siden faldet voldsomt",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Hedvin
ggplot(alkoholplot, aes(x=TID, y =`  Hedvin`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Hedvin har sine op og nedture og peakede i start 2000",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Vinbaserede drikkevarer og alkoholfri vin
ggplot(alkoholplot, aes(x=TID, y =`  Vinbaserede drikkevarer og alkoholfri vin`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Vinbaserede drikkevarer og alkoholfri vin havde peak 2013 og har ikke været stort siden",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Pilsnerøl, guldøl
ggplot(alkoholplot, aes(x=TID, y =`  Pilsnerøl guldøl`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på pilsner og guldøl har været dalene siden sit peak i slut 90'erne",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Andre alkoholholdige øl
ggplot(alkoholplot, aes(x=TID, y =`  Andre alkoholholdige øl`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på alkolholdige øl peakede i 2016 hvor det steg volsomt og siden faldet",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Øl med lavt alkoholindhold og alkoholfri øl
ggplot(alkoholplot, aes(x=TID, y =`  Øl med lavt alkoholindhold og alkoholfri øl`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Øl med lavt alkoholindhold og alkoholfri øl er blevet mere populært de seneste år",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Øl-baserede drikkevarer
ggplot(alkoholplot, aes(x=TID, y =`  Ølbaserede drikkevarer`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Øl-baserede drikkevarer peakede lige inden corona og er siden faldet",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Cigaretter
ggplot(alkoholplot, aes(x=TID, y =`  Cigaretter`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på cigaretter har været stille dalene siden finanskrisen",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Cigarer o.l.
ggplot(alkoholplot, aes(x=TID, y =`  Cigarer ol`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på cigarer og lignende har være dalene siden 90'erne",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Andre tobaksvarer
ggplot(alkoholplot, aes(x=TID, y =`  Andre tobaksvarer`))+
  geom_point(size = 2)+
  theme_minimal()+
  labs(title = "Forbruget på andre tobaksvarer er også faldet fra over 2 mia til under 250mio",
       x = "Årstal",
       y = "Forbrug i mio. kr")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


###########opgave 4.2

corr <- round(cor(alkoholplot %>% select(where(is.numeric)), use = "pairwise.complete.obs"), 2)
corr_long <- corr %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

ggplot(corr_long, aes(var1, var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = correlation), size = 3) +
  scale_fill_gradient2(low = "yellow", high = "darkolivegreen4", mid = "darkolivegreen3", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
