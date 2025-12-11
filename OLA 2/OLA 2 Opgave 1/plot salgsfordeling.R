jason <- c(rep(1, times = 23922))
mergedcleanvilla$counter <- jason

plotting <- mergedcleanvilla %>% 
  group_by(Bykategori) %>% 
  summarise(value=sum(counter),.groups = "drop")

jagurd <- c("Landsby", "Lille by", "Almindelig by", "Større by", "Storby")   
plotting$labels <- factor(jagurd, levels = jagurd)

ggplot(data = plotting, aes(x=labels, y=value, fill = Bykategori))+
  geom_bar(stat = "identity")+ 
  geom_text(aes(label = paste0("procent = ", round(value/sum(value[1:5])*100),"%")),
            vjust = -0.5,
            size = 3) +
  theme_minimal()+
  labs(title = "De fleste boliger til salg ligger i almindelige og større byer", 
       caption = "Kilde:https://www.statistikbanken.dk/POSTNR2\nKilde:https://www.boligsiden.dk/",
       y = "Antal boliger til salg",
       x = "Bykategori")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  

        
