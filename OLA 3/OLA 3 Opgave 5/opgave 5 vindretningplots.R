#################anholt
dfan <- expand.grid(
  x = seq(0, pi, pi/12),
  y = 0
)

# Tilføj vindretning
dfan$winddir <- as.numeric(anholt_flip$value[1:nrow(df)])


dfan$u <- -sin(dfan$winddir * pi / 180)
dfan$v <- -cos(dfan$winddir * pi / 180)


ggplot(dfan, aes(x = x, y = 0, u = u, v = v)) +
  geom_quiver(color = "darkolivegreen4") +
  coord_equal()+
  theme_minimal()+
  labs(title= "Vinden over Anholt bevægede sig i nordvestgående retning", y = "", x = "Dato")+
  scale_x_continuous(breaks = seq(0,3, length.out = 8), labels = c("21. november", "20. november",
                                                           "19. november", "18. november", 
                                                           "17. november", "16. november",
                                                           "15. november", "14. november"))+
  scale_y_continuous(labels = NULL)

#############aarhus
dfaa <- expand.grid(
  x = seq(0, pi, pi/12),
  y = 0
)

# Tilføj vindretning
dfaa$winddir <- as.numeric(aarhus_flip$value[1:nrow(df)])

# ✅ KORREKT omregning fra kompasgrader til matematiske vektorer:
dfaa$u <- -sin(dfaa$winddir * pi / 180)
dfaa$v <- -cos(dfaa$winddir * pi / 180)
install.packages("rnaturalearthdata")

ggplot(dfaa, aes(x = x, y = 0, u = u, v = v)) +
  geom_quiver(color = "darkolivegreen4") +
  coord_equal()+
  theme_minimal()+
  labs(title= "Vinden over Aarhus bevægede sig i nordvestgående retning", y = "", x = "Dato")+
  scale_x_continuous(breaks = seq(0,3, length.out = 8), labels = c("21. november", "20. november",
                                                                   "19. november", "18. november", 
                                                                   "17. november", "16. november",
                                                                   "15. november", "14. november"))+
  scale_y_continuous(labels = NULL)
