unique(bildf2$makemodels)
bildf2$modelny <- gsub("^([A-Za-zÆØÅæøå]+\\s+[A-Za-zÆØÅæøå]+).*", "\\1", bildf2$makemodels)

unique(bildf2$modelny2)

bildf2$modelny2 <- gsub("^([A-Za-zÆØÅæøå]+).*", "\\1", bildf2$makemodels)

bildf2$prisny <- gsub("[^0-9]","",bildf2$prices)



plotdf <- bildf2 %>% 
  group_by(modelny2) %>%
  summarise(gennemsnitspris = mean(as.numeric(prisny), na.rm = TRUE), n=n())

plotdf2 <- plotdf %>% arrange(gennemsnitspris)

colnames(plotdf2) <- c("Bilmærke", "gennemsnitspris", "n")

int <- mean(plotdf2$gennemsnitspris)

ggplot(data=plotdf2[12:21,],aes(y=gennemsnitspris, x = factor(Bilmærke, levels = Bilmærke), fill = Bilmærke))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Hymer og mercedes dominerer tydeligt når det kommer til priser på autocampere", 
       caption = "Kilde:Bilbasen", 
       x = "Bilmærke", 
       y = "Gennemsnitspris",
       subtitle = "Alle biler fordelt på mærke")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = seq(0,2000000, by = 200000), limits = c(0,1800000))+
  geom_hline(yintercept = int)

#############nye biler  
nyebiler <- bildf2 %>% filter(Kilometertal=="(ny bil)")
nyebilerdf <- nyebiler %>% 
  group_by(modelny2) %>%
  summarise(gennemsnitspris = mean(as.numeric(prisny), na.rm = TRUE), n=n())

nyebilerdf <- nyebilerdf %>% arrange(gennemsnitspris)

colnames(nyebilerdf) <- c("Bilmærke", "gennemsnitspris", "n")
intny <- mean(nyebilerdf$gennemsnitspris)

ggplot(data=nyebilerdf,aes(y=gennemsnitspris, x = factor(Bilmærke, levels = Bilmærke), fill = Bilmærke))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Når det kommer til nye biler er det Hymer der har de højeste priser",
       caption = "Kilde:Bilbasen", 
       x = "Bilmærke", 
       y = "Gennemsnitspris",
       subtitle = "Nye Biler fordelt på mærke")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = seq(0,2000000, by = 200000), limits = c(0,1800000))+
  geom_hline(yintercept = intny)


#############brugte biler
brugtebiler <- bildf2 %>% filter(Kilometertal!="(ny bil)")

brugtebilerdf <- brugtebiler %>% 
  group_by(modelny2) %>%
  summarise(gennemsnitspris = mean(as.numeric(prisny), na.rm = TRUE), n=n())

brugtebilerdf <- brugtebilerdf %>% arrange(gennemsnitspris)

colnames(brugtebilerdf) <- c("Bilmærke", "gennemsnitspris", "n")
intbrugt <- mean(brugtebilerdf$gennemsnitspris)

ggplot(data=brugtebilerdf[10:19,],aes(y=gennemsnitspris, x = factor(Bilmærke, levels = Bilmærke), fill = Bilmærke))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  theme_minimal()+
  labs(title = "Ved brugte biler er det dog mercedes der har de højeste priser på autocampere", 
       caption = "Kilde:Bilbasen", 
       x = "Bilmærke", 
       y = "Gennemsnitspris",
       subtitle = "Brugte biler fordelt på mærke")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = seq(0,2000000, by = 200000), limits = c(0,1800000))+
  geom_hline(yintercept = intbrugt)
