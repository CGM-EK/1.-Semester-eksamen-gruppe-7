
library(tidyverse)

#hiver kolonnen ud for lånemuligheder og ændrer dem til 3 variabler istedet for 5
ju <- df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`
ju[v == "Dårlig"] <- "Dårlige"
nyemuligheder <- case_when(
  v %in% c("Meget dårlige", "Dårlige") ~ "Dårlige(n=447)",
  v %in% c("Neutrale") ~ "Neutrale(n=800)",
  v %in% c("Gode", "Meget gode") ~ "Gode(n=3186)",
  TRUE ~ NA_character_
)
dflol <- df
dflol$lånmuligheder <- nyemuligheder
#fjerner naværdierne fra kolonnen eks der hvor der stod "ved ikke" før
df_lol <- dflol[!is.na(dflol$lånmuligheder), ]
table(dflol$lånmuligheder)
#Definerer rækkefølgen
levels_orderny <- c(
  "Dårlige(n=447)",
  "Neutrale(n=800)",
  "Gode(n=3186)"
)

#Laver til faktor
dflol$lånmuligheder <- factor(nyemuligheder, levels = levels_orderny, ordered = TRUE)
dflol$n <- n(dflol$lånemuligheder)
df2 <- dflol %>%
  group_by(lånmuligheder) %>%        # gruppér efter den kategori du vil tælle
  mutate(n = n()) %>%           # n() giver antal rækker i hver gruppe
  ungroup()
df2$fixeren <- 1
df2 <- df2 %>% filter(!is.na(lånemuligheder))
ggplot(data = df2, aes(x=lånemuligheder,y = fixeren,
                          fill = lånemuligheder))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(n/4433*100,1),"%")), vjust = -0.5, size = 3, color = "white")+
  theme_minimal()+
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    title = "Størstedelen af udspurgte virksomheder ser favorabelt på deres lånemuligheder",
    y = "Antal",
    x = "Svarkategori"
  )
table(df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)
table(df_clean$lånemuligheder)

dfuvi <- df[!grepl("Ved", df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`), ]

################3

summary(df$`Antal ansatte Cvr-nr.`)

mybreaks=c(10000, 500, 100,50, 20, 0)
mylabs=c("1-20 ansatte", "21-50 ansatte", "51-100 ansatte", "101-500 ansatte", "Over 500 ansatte")

#den nye kategorivariabel defineres
ansattekategori <- cut(df$`Antal ansatte Cvr-nr.`, labels = mylabs, breaks = mybreaks)

ansattekategori
table(ansattekategori)
df$ansattekategori <- ansattekategori

#### vi prøver at indekse talene

table(df$lånemuligheder)
jonathan <- subset(df,df$lånemuligheder=="Dårlige")
jannik <- subset(df,df$lånemuligheder=="Neutrale")
jakob <- subset(df,df$lånemuligheder=="Gode")
jurl <- as.vector(table(jakob$ansattekategori))
gode <- as.numeric(jurl/nrow(jakob)*100)
kurl <- as.vector(table(jonathan$ansattekategori))
dårlige <- as.numeric(jonathan/ncol(kurl)*100)
qurl <- as.vector(table(jannik$ansattekategori))
neutrale <- as.numeric(qurl/nrow(jannik)*100)
dfplot

kasper <- subset(df,df$ansattekategori=="1-20 ansatte")
kris <- subset(df,df$ansattekategori=="21-50 ansatte")
kristian <- subset(df,df$ansattekategori=="51-100 ansatte")
krabbe <- subset(df,df$ansattekategori=="101-500 ansatte")
krumme <- subset(df,df$ansattekategori=="over 500 ansatte")
nurl <- as.vector(table(jakob$ansattekategori))
gode <- as.numeric(jurl/nrow(jakob)*100)
burl <- as.vector(table(jonathan$ansattekategori))
dårlige <- as.numeric(kurl/nrow(jonathan)*100)
xurl <- as.vector(table(jannik$ansattekategori))
neutrale <- as.numeric(qurl/nrow(jannik)*100)
dfplot <- data.frame(cbind(gode,neutrale,dårlige, mylabs))


df_plot <- dfplot %>%
  pivot_longer(
    cols = c(gode, neutrale, dårlige),
    names_to = "kategori",
    values_to = "værdi"
  )
df_plot$værdi <- as.numeric(df_plot$værdi)
levels_orderigen <- c(
  "1-20 ansatte",
  "21-50 ansatte",
  "51-100 ansatte",
  "101-500 ansatte",
  "Over 500 ansatte"
)

#Lav til faktor
df_plot$mylabs <- factor(df_plot$mylabs, levels = levels_orderigen, ordered = TRUE)

nyelevels <- c(
  "dårlige",
  "neutrale",
  "gode"
)
df_plot$kategori <- factor(df_plot$kategori, levels = nyelevels, ordered = TRUE)

ggplot(df_plot, aes(x = mylabs, y = værdi, fill = kategori)) +
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,100))+
  labs(title = "Flest af de større virksomheder vurderes at de har gode lånemuligheder",
       x= "indeks for lånemuligheder",
       y= "ansattekategori")+
  theme_minimal()

##
df_indeks <- df_plot %>%
  group_by(mylabs) %>%
  mutate(
    total = sum(as.numeric(værdi)),
    andel = as.numeric(værdi) / total * 100
  )
df_indeks$kategori <- factor(df_indeks$kategori, levels = nyelevels)
df_indeks$mylabs <- factor(df_indeks$mylabs, levels = levels_orderigen)

library(viridis)

ggplot(df_indeks, aes(x = mylabs, y = andel, fill = kategori)) +
  geom_col(position = "dodge")+
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(title = "Flest af de større virksomheder vurderes at de har gode lånemuligheder",
       x= "indeks for lånemuligheder",
       y= "ansattekategori")+
  theme_minimal()
