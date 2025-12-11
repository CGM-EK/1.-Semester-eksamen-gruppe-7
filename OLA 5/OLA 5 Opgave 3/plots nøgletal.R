
##############Egenkapitalens forrentning

#pivot longer på egenkapital, den søger efter kolonner der indeholder "egenkap"
#Den tager værdierne over i en ny kolonne der hedder værdi og årene får også sin egen kolonne ud fra værdien
df_longek <- df_clean %>%
  pivot_longer(
    cols = starts_with("egenkap"),
    names_to = "år",
    values_to = "værdi"
  )

#renser den nye år-kolonne for alt andet end bogstaver så der kun er årstallet
df_longek$år <- as.numeric(gsub("[^0-9]", "", df_longek$år))

#grupperer på år og lånemuligheder og retunerer gennemsnitsværdierne for dem der har svaret de forskellige lånemuligheder
df_meanek <- df_longek %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop")

ggplot(df_meanek, aes(x = år, y = mean_værdi, fill = lånemuligheder)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    x = "Vurdering af lånemuligheder",
    y = "Gennemsnitlig egenkapitals forretning",
    title = paste("Det kan tyde på at virksomheder at virksomheder der ikke har lige så gode lånemuligheder",
                  "er bedere til at realisere deres egen kapitals potentiale", sep = "\n")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

############afkastningsgrad
#pivot longer på afkastningsgrad, den søger efter kolonner der indeholder "afkast"
#Den tager værdierne over i en ny kolonne der hedder værdi og årene får også sin egen kolonne ud fra værdien
df_longafk <- df_clean %>%
  pivot_longer(
    cols = starts_with("Afkast"),
    names_to = "år",
    values_to = "værdi"
  )

#renser den nye år-kolonne for alt andet end bogstaver så der kun er årstallet
df_longafk$år <- as.numeric(gsub("[^0-9]", "", df_longafk$år))

#grupperer på år og lånemuligheder og retunerer gennemsnitsværdierne for dem der har svaret de forskellige lånemuligheder
df_meanafk <- df_longafk %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop")

ggplot(df_meanafk, aes(x = år, y = mean_værdi, fill = lånemuligheder)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    x = "Vurdering af lånemuligheder",
    y = "Gennemsnitlig afkastningsgrad",
    title = "Der er tilsyndeladende ikke nogen sammenhæng mellem afkastningsgrad og vurdering af lånemuligheder"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(5,20, by = 5), limits = c(0,20))
theme(axis.text.x = element_text(angle = 30, hjust = 1))

############likviditetsgrad
#pivot longer på likviditetsgrad, den søger efter kolonner der indeholder "likvi"
#Den tager værdierne over i en ny kolonne der hedder værdi og årene får også sin egen kolonne ud fra værdien
df_longlik <- df_clean %>%
  pivot_longer(
    cols = starts_with("likvi"),     # eller: cols = c("år2021","år2022","år2023","år2024")
    names_to = "år",
    values_to = "værdi"
  )

#renser den nye år-kolonne for alt andet end bogstaver så der kun er årstallet
df_longlik$år <- as.numeric(gsub("[^0-9]", "", df_longlik$år))

#grupperer på år og lånemuligheder og retunerer gennemsnitsværdierne for dem der har svaret de forskellige lånemuligheder
df_meanlik <- df_longlik %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop")



ggplot(data = df_meanlik, aes(x=år,y=mean_værdi,fill=lånemuligheder))+
  geom_col(position = "dodge", width = 0.8)+
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,225), breaks = seq(0,225, by = 25))+
  labs(
    title = paste("Virksomheder der har svaret positivt omkring deres lånemuligheder har været dem med højest likvidtetsgrad",
                  "fra 2016 til 2020hvor dem med bedst likviditetsgrad har givet udtryk for dårlige lånemuligheder", sep = "\n"),
    x = "Vurdering af lånemuligheder",
    y = "Gennemsnitlige likviditetsgrad"
  )

#############soliditetsgrad
#pivot longer på soliditetsgrad, den søger efter kolonner der indeholder "soliditet"
#Den tager værdierne over i en ny kolonne der hedder værdi og årene får også sin egen kolonne ud fra værdien
df_longsol <- df_clean %>%
  pivot_longer(
    cols = starts_with("soliditet"),     # eller: cols = c("år2021","år2022","år2023","år2024")
    names_to = "år",
    values_to = "værdi"
  )

#renser den nye år-kolonne for alt andet end bogstaver så der kun er årstallet
df_longsol$år <- as.numeric(gsub("[^0-9]", "", df_longsol$år))

#grupperer på år og lånemuligheder og retunerer gennemsnitsværdierne for dem der har svaret de forskellige lånemuligheder
df_meansol <- df_longsol %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop")



ggplot(data = df_meansol, aes(x=år,y=mean_værdi,fill=lånemuligheder))+
  geom_col(position = "dodge", width = 0.8)+
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  theme_minimal()+
  labs(
    title = paste("Der er en tydelig sammenhæng mellem virksomheders virksomhedens mulighed for at optage",
                  "lån og deres soliditetsgrad", sep = "\n"),
    x = "Vurdering af lånemuligheder",
    y = "Gennemsnitlige soliditetsgrad"
  )

##########balancen
df_longbal <- df_clean %>%
  pivot_longer(
    cols = starts_with("balance"),
    names_to = "år",
    values_to = "værdi"
  )

#lortet driller så vi laver vores egen årkolonne
år = c(rep(2016:2020, each = 3))
dfetst <- filter(df_longbal, df_longbal$lånemuligheder=="Dårlige" )
mean(dfetst$værdi,na.rm = TRUE)
df_meanbal <- df_longbal %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop")

df_meanbal$year <- år

ggplot(df_meanbal, aes(x = year, y = log(mean_værdi), fill = lånemuligheder)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    x = "Vurdering af lånemuligheder",
    y = "logaritmiske værdi af gennemsnitlige balance",
    title = "Det ser tydeligt ud til at virksomheder med en højere balance også har bedere lånemuligheder"
  ) +
  theme_minimal()

#bruttofortjeneste
df_longbrut <- df_clean %>%
  pivot_longer(
    cols = starts_with("Brutto"),
    names_to = "år",
    values_to = "værdi"
  )

#lortet driller så vi laver vores egen årkolonne
dfetst <- filter(df_longbrut, df_longbrut$lånemuligheder=="Dårlige" )
mean(dfetst$værdi,na.rm = TRUE)
df_meanbrut <- df_longbrut %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop", n=n())

df_meanbrut$year <- år

ggplot(df_meanbrut, aes(x = year, y = log(mean_værdi), fill = lånemuligheder)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    x = "Vurdering af lånemuligheder",
    y = "logaritmiske værdi af gennemsnitlige bruttofortjeneste",
    title = "Det ser tydeligt ud til at virksomheder med en højere bruttofortjeneste også har bedere lånemuligheder"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

##primært resultat
df_longprim <- df_clean %>%
  pivot_longer(
    cols = starts_with("Primær"),
    names_to = "år",
    values_to = "værdi"
  )

#lortet driller så vi laver vores egen årkolonne
dfetst <- filter(df_longprim, df_longprim$lånemuligheder=="Dårlige" )
mean(dfetst$værdi,na.rm = TRUE)
df_meanprim <- df_longprim %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop", n=n())

df_meanbrut$year <- år

ggplot(df_meanprim, aes(x = år, y = log(mean_værdi), fill = lånemuligheder)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    x = "Vurdering af lånemuligheder",
    y = "logaritmiske værdi af gennemsnitlige primært resultat",
    title = "Det ser tydeligt ud til at virksomheder med et højere primæt resultat også har bedere lånemuligheder"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#kortfristet gæld
df_longkort <- df_clean %>%
  pivot_longer(
    cols = starts_with("Kortfrist"),
    names_to = "år",
    values_to = "værdi"
  )

#lortet driller så vi laver vores egen årkolonne
dfetst <- filter(df_longkort, df_longkort$lånemuligheder=="Dårlige" )
mean(dfetst$værdi,na.rm = TRUE)
df_meankort <- df_longkort %>%
  group_by(år, lånemuligheder) %>%
  summarise(mean_værdi = mean(værdi, na.rm = TRUE), .groups = "drop", n=n())

df_meanbrut$year <- år

ggplot(df_meankort, aes(x = år, y = log(mean_værdi), fill = lånemuligheder)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_viridis(discrete = TRUE, option = "e", begin = 0.2, end = 0.8)+
  labs(
    x = "Vurdering af lånemuligheder",
    y = "logaritmiske værdi af gennemsnitlige kortfristet gæld",
    title = "Det ser tydeligt ud til at virksomheder med en højere kortfristet gæld også har bedere lånemuligheder"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
