unique(bildf2$forhandler)

forhandlere <- bildf2 %>% 
  group_by(forhandler) %>%
  summarise(gennemsnitspris = mean(as.numeric(prisny), na.rm = TRUE), n=n())

forhandlere2 <- forhandlere %>% arrange(n)

forhand <- forhandlere2[36:43,]
forhand <- forhand %>% arrange(gennemsnitspris)
intfor <- mean(forhand$gennemsnitspris)
forhand$forhandler <- gsub(" - en del af Semler Mobility", "", forhand$forhandler)

ggplot(data=forhand,aes(y=gennemsnitspris, x = factor(forhandler, levels = forhandler), fill = forhandler))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = seq(0,1200000, by = 200000), limits = c(0,1200000))+
  geom_hline(yintercept = intfor)+
  labs(title = "Camperservice ApS, Volswagen Risskov, Jysk Caravan Center og KOBE Leasing A/S har de højeste gennemsnitlige priser", 
       caption = "Kilde:Bilbasen", 
       x = "Forhandler", 
       y = "Gennemsnitspris",
       subtitle = "Priser fordelt på bilforhandlere")


################camperservice ApS

camperservice <- bildf2 %>% filter(forhandler=="Camperservice ApS")

camperser <- camperservice %>% 
  group_by(modelny2) %>%
  summarise(n=n())
colnames(camperser) <- c("Bilmærke", "n")

ggplot(data=camperser,aes(y=n, x = factor(Bilmærke, levels = Bilmærke), fill = Bilmærke))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Camperservice ApS forhandler primært med Hymer og Carado, hvilket forklarer den højere gennemsnitspris", 
       caption = "Kilde:Bilbasen", 
       x = "Bilmærke", 
       y = "Antal biler til salg",
       subtitle = "Biler til salg af Camperservice ApS fordelt på bilmærke")

###############Jysk caravan center
jyskc <- bildf2 %>% filter(forhandler=="Jysk Caravan Center")

jysk <- jyskc %>% 
  group_by(modelny2) %>%
  summarise(n=n())
colnames(jysk) <- c("Bilmærke", "n")

ggplot(data=jysk,aes(y=n, x = factor(Bilmærke, levels = Bilmærke), fill = Bilmærke))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n), vjust = -0.5, size = 3)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = paste("Jysk Caravan Center forhandler kun med Ahorn, Benimar og Hobby, som alle har gennemsnitspriser", "\nhøjere end gennemsnittet for alle mærker"), 
       caption = "Kilde:Bilbasen", 
       x = "Bilmærke", 
       y = "Antal biler til salg",
       subtitle = "Biler til salg af Jysk Caravan Center fordelt på bilmærke")
