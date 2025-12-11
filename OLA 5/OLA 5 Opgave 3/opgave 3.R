library(tidyverse)
library(ordinal)
library(readxl)

#loader dataset til opgaven
regnskaber_industri_transport_byg_5_25000_ansatte_anonym <- read_excel("~/R projekter/OLAer igen/OLA5/regnskaber_industri_transport_byg_5_25000_ansatte_anonym.xlsx")
View(regnskaber_industri_transport_byg_5_25000_ansatte_anonym)

#laver en lidt mere bearbejdlig dataframe til brug i opgaven ud fra datasættet
df <- regnskaber_industri_transport_byg_5_25000_ansatte_anonym[c(1,2,5,6,20,21,22,23,24,38,39,40,41,42,50,51,52,53,54,56,57,58,59,60,188,189,190,191,192,194,195,196,197,198,206,207,208,209,210,212,213,214,215,216,218,219,220,221,222,224,225,226,227,228)]

#tjekker de forskellige besvarelser i spørgeskemaet
unique(df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

# Kopiér kolonnen til et kort navn
v <- df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`

#retter stavefejl / variationer
v[v == "Dårlig"] <- "Dårlige"
x_new <- case_when(
  v %in% c("Meget dårlige", "Dårlige") ~ "Dårlige",
  v %in% c("Neutrale") ~ "Neutrale",
  v %in% c("Gode", "Meget gode") ~ "Gode",
  TRUE ~ NA_character_
)
unique(x_new)

# 2. Definér rækkefølgen
levels_order <- c(
  "Dårlige",
  "Neutrale",
  "Gode"
)

# 3. Lav til faktor
df$lånemuligheder <- factor(x_new, levels = levels_order, ordered = TRUE)
df_clean <- df[!is.na(df$lånemuligheder), ]
levels(df_clean$lånemuligheder)
unique(df_clean$lånemuligheder)

###############opgave 2####################
modfuld <- clm(df$lånemuligheder ~ log(df$`Balance 2020 (1.000 kr)`)+df$`Afkastningsgrad 2020 (%)` + 
                 df$`Likviditetsgrad 2020 (%)` + df$`Soliditetsgrad 2020 (%)` + 
                 df$`Egenkapital forrentning 2020 (%)`, data = df, link = "logit")
summary(modfuld)

##balance
mod1 <- clm(df_clean$lånemuligheder ~ log(df_clean$`Balance 2020 (1.000 kr)`), data = df, link = "logit")
summary(mod1)
##afkastningsgrad
mod2 <- clm(df_clean$lånemuligheder ~ df_clean$`Afkastningsgrad 2020 (%)`, data = df, link = "logit")
summary(mod2)
##likviditetsgrad
mod3 <- clm(df_clean$lånemuligheder ~ df_clean$`Likviditetsgrad 2020 (%)`, data = df, link = "logit")
summary(mod3)
##soliditetsgrad
mod4 <- clm(df_clean$lånemuligheder ~ df_clean$`Soliditetsgrad 2020 (%)`, data = df, link = "logit")
summary(mod4)
##egenkaptials forrentning
mod5 <- clm(df_clean$lånemuligheder ~ df_clean$`Egenkapital forrentning 2020 (%)`, data = df, link = "logit")
summary(mod5)
##bruttofortjeneste
mod6 <- clm(df_clean$lånemuligheder ~ log(df_clean$`Bruttofortjeneste 2020 (1.000 kr)`), data = df, link = "logit")
summary(mod6)
##primært resultat
mod7 <- clm(df_clean$lånemuligheder ~ log(df_clean$`Primært Resultat 2020 (1.000 kr)`), data = df, link = "logit")
summary(mod7)
##kortfristet gæld
mod8 <- clm(df_clean$lånemuligheder ~ log(df_clean$`Kortfristet gæld 2020 (1.000 kr)`), data = df, link = "logit")
summary(mod8)




################opgave 3#####################
#se plot scripts