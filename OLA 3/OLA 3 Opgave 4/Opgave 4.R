
y <- f.tillidsammen$pfv
x_df <- as.list(totalcordf)

r2_results <- list()

for (name in names(x_df)) {
  x <- x_df[[name]]
  r2_values <- numeric(25)
  
  for (i in 1:25) {
    lmtemp <- lm(y[-i] ~ x[-i])
    r2_values[i] <- summary(lmtemp)$r.squared
  }
  
  r2_results[[name]] <- r2_values
}

r2df <- data.frame(r2_results)
rddft <- t(r2df)

####PLOT forfra####
tester <- data.frame("kombination af spørgsmål"=row.names(rddft))
for(i in 1:25){
  tester[,i+1] <- cbind(rank(-rddft[,i]))
}
colnames(tester) <- c(103:(102-24))

testerorder <- tester[order(tester$`102`),]
plotdf <- testerorder[1:5,1:25]

plotdfyes <- plotdf %>% 
  pivot_longer(cols = -1,
               names_to = "komb",
               values_to = "rangering")

ggplot(plotdfyes, aes(x = as.numeric(komb), y = rangering, group = `103`, colour=`103`)) +
  geom_line(size=1.2) +
  scale_colour_brewer(palette = "Dark2")+
  scale_y_reverse()+
  geom_point() +
  theme_minimal() +
    scale_x_reverse()+
  labs(x = "Antal kvartaler tilbage(kvartaler fjernes forfra)", y = "Rangering", title = "1. pladsen holder sin rangeringen stabilt igennem testen")


##################
plotdf1t <- c(plotdft[,1], plotdft[,2], plotdft[,3], plotdft[,4], plotdft[,5])

plotdffinal <- data.frame(value=as.numeric(plotdf1t), spg=spgvec)

kurt <- plotdffinal %>%
  mutate(spg = factor(spg, levels = unique(spg)))

kurt$tid <- c(rep(1:24, times=5))
ggplot(kurt, aes(x = tid, y = value, group = spg, colour=spg)) +
  geom_line(size=1.2) +
  scale_colour_brewer(palette = "Dark2")+
  scale_y_reverse()+
  geom_point() +
  theme_minimal() +
  labs(x = "Antal kvartaler fjernet (forfra)", y = "Rangering", title = "1. pladsen holder sin rangeringen stabilt igennem testen")
##################

####PLOT forfra######

r2_resultsb <- list()

for (name in names(x_df)) {
  x <- x_df[[name]]
  r2_values <- numeric(25)
  
  for (i in 1:25) {
    lmtemp <- lm(y[i:103] ~ x[i:103])
    r2_values[i] <- summary(lmtemp)$r.squared
  }
  
  r2_resultsb[[name]] <- r2_values
}

r2bdf <- data.frame(r2_resultsb)

r2bdft <- t(r2bdf)
tester2 <- data.frame("kombination af spørgsmål"=row.names(r2bdft))
for(i in 1:25){
  tester2[,i+1] <- cbind(rank(-r2bdft[,i]))}
colnames(tester2) <- c(103:(102-24))

testerorder2 <- tester2[order(tester2$`102`),]
plotdf2 <- testerorder2[1:5,1:25]

plotdfyes2 <- plotdf2 %>% 
  pivot_longer(cols = -1,
               names_to = "komb",
               values_to = "rangering")

ggplot(plotdfyes2, aes(x = as.numeric(komb), y = rangering, group = `103`, colour=`103`)) +
  geom_line(size=1.2) +
  scale_colour_brewer(palette = "Dark2")+
  scale_y_reverse()+
  geom_point() +
  theme_minimal() +
  scale_x_reverse()+
  labs(x = "Antal kvartaler tilbage(kvartaler fjernes forfra)", y = "Rangering", title = "1. pladsen holder sin rangeringen stabilt igennem testen")
################

#undersøgelse af hvilke indikatorer der ligger i top 5 nu

jakob <- forbrugertillid[25:103,]
for (i in 1:12) {
  # generate all combinations of columns of size i
  Comblist <- combn(cols, i, simplify = FALSE)
  
  # compute correlations for each combination
  cordf <- lapply(Comblist, function(vars) {
    combo_mean <- rowMeans(jakob[, vars, drop = FALSE])
  })
  
  # convert to a data frame with names
  cordf_df <- as.data.frame(cordf)
  
  #tilføjer navne til cordf
  colnames(cordf_df)[1:ncol(cordf_df)] = sapply(Comblist, paste, collapse = " + ")
  
  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("stabdf", i), cordf_df)
}
totalstabdf <- cbind(stabdf1, stabdf2,stabdf3,stabdf4,stabdf5,stabdf6,stabdf7,stabdf8,stabdf9,stabdf10,stabdf11,stabdf12)
tempstabdf = as.data.frame(matrix(data=NA,  nrow = 2, ncol = ncol(totalstabdf)))
for (i in 1:ncol(totalstabdf)){
  lm.spgcomb <- lm(f.tillidsammen$pfv[25:103] ~ totalstabdf[,i])
  fit.temp <- as.data.frame(fitted.values(lm.spgcomb))
  Cor1 <- cor(f.tillidsammen$pfv[25:103], fit.temp)
  R2 <- summary(lm.spgcomb)$r.squared
  tempstabdf[1,i] <- R2
  tempstabdf[2,i] <- Cor1
}
colnames(tempstabdf) <- colnames(totalstabdf)
rsquaredstabdf <- t(tempstabdf)
#Navngiver kolonnenavne
colnames(rsquaredstabdf) <- c("R2", "COR")

#Opretter dataframe til top 5 bedste combinationer
Top5stabcombinationer <- rsquaredstabdf[order(-rsquaredstabdf[,2]), ]
Top10stabcombinationer <- as.data.frame(Top5stabcombinationer[1:10,1:2])
