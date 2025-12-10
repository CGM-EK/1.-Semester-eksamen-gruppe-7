############### LOAD CIRKEL-FILER FRA GITHUB ###############

# Liste over alle circjet-filer der skal hentes
filer <- c("circjet","circjet2","circjet3","circjet4","circjet5","circjet6","circjet7")

# Tom dataframe som alle circjet-fly bliver appended til
allecircfly <- data.frame()

# Loop der downloader hver .rds-fil og tilføjer dem til én samlet dataframe
for (name in filer) {
  url_full <- paste0(
    "https://raw.githubusercontent.com/CGM-EK/1.-Semester-eksamen-gruppe-7/main/OLA%204/OLA%204%20Opgave%203/",
    name,
    ".rds"
  )
  
  df <- readRDS(url(url_full))  # læser RDS fil fra GitHub
  df$source <- name             # tilføjer filnavn som identifikator
  allecircfly <- rbind(allecircfly, df)  # binder filen på master dataframen
}


############### LOADER HJÆLPEFUNKTIONER (Util.R) ###############

url <- "https://raw.githubusercontent.com/CGM-EK/1.-Semester-eksamen-gruppe-7/main/OLA%204/OLA%204%20Opgave%203/util.R"
source(url)   # loader getToken() funktionen


############### API SETUP ###############

# Base URL til OpenSky API’et
baseurl <- "https://opensky-network.org/api"

# Endpoints til states og tracks
endpointS <- "/states/all"
endpointT <- "/tracks/all"

# Samlede forespørgsels-URL'er
queryS <- paste0(baseurl, endpointS)
queryT <- paste0(baseurl, endpointT)


############### DEFINERER NORDSØ-OMRÅDE ###############

lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954

# URL til at hente alle fly over Nordsøen
fullurl <- paste0(baseurl, endpointS, "?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)


############### HENTER "STATES" FOR NORDSØEN ###############

token <- getToken()  # genererer nyt token
res <- httr::GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
rescontent <- httr::content(res, as = "text")
resretval <- jsonlite::fromJSON(rescontent)

# Konverterer listen til dataframe
statedfpr <- as.data.frame(resretval$states)


############### 3.1 BARPLOT OVER LANDE ###############

# Rydder op i landet "Kingdom of the Netherlands"
statedfpr$V3 <- gsub("Kingdom of the Netherlands", "Netherlands", statedfpr$V3)

# Plotter fordeling af fly efter hjemland
ggplot(data=statedfpr, aes(x=V3, fill=V3)) +
  geom_bar() +
  labs(
    title="Overvægt af britiske fly over Nordsøen",
    x="Lande",
    y="Antal fly",
    caption=paste0(baseurl,endpointS)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(breaks = seq(0,100,by=5))


############### 3.2 TRACKS FOR UDVALGTE FLY ###############

# Eksempel fly (lige linje)
icaoF <- "4952cf"
begin <- 1763474494
end   <- 1763474495

flyurl <- paste0(
  baseurl, endpointT,
  "?icao24=", icaoF,
  "&begin=", begin,
  "&end=", end
)

# Henter flyets track
resfly <- GET(flyurl, add_headers(Authorization = paste("Bearer", token)))
icaol <- as.data.frame(fromJSON(content(resfly, as="text")))

# Plotter flyets rute
plot(x = icaol$path.2, y = icaol$path.3)


# Fly 2 — et cirkel-fly
plot(x = circjet2$lat[150:295], y = circjet2$lng[150:295])


############### 3.3 LOOPER GENNEM ALLE FLY I NORDSØOMRÅDET ###############

alle_fly <- list()  # liste til at gemme alle track-data
icaoliste <- as.list(statedfpr$V1)  # alle ICAO koder fra states-data

# Henter tracks for hver enkelt ICAO – med error handling
for (i in seq_along(icaoliste)) {
  tryCatch({
    icaoting <- paste0(
      baseurl, endpointT, "?icao24=", icaoliste[i],
      "&begin=", statedfpr[i,4],
      "&end=", statedfpr[i,5]
    )
    
    resfly3 <- GET(icaoting, add_headers(Authorization = paste("Bearer", token)))
    bingbang <- as.data.frame(fromJSON(content(resfly3, as="text")))
    bingbang$icao_id <- icaoliste[i]  # tilføjer ICAO-kode
    alle_fly[[length(alle_fly) + 1]] <- bingbang  # gemmer i liste
    
  }, error=function(e){
    message("Fejl ved iteration ", i, ": ", e$message)
  })
}

# Samler alle track-dataframes til én stor dataframe
samlet_flydata <- do.call(rbind, alle_fly)


############### 3.3 LINEÆR MODEL FOR HVER FLYRUTE ###############

r2values <- list()

# Beregner R² for hver flyrute (bruges til at finde cirkel-fly)
for(i in 1:length(alle_fly)){
  lm_temp <- lm(alle_fly[[i]][["path.2"]] ~ alle_fly[[i]][["path.3"]])
  r2values[i] <- summary(lm_temp)$r.squared
}


############### 3.4 CIRKEL-FLY DETEKTION ###############

# En rute med lav R² ≈ ikke-lineær → ofte cirkulær bevægelse
lmhubba <- lm(circjet2$lat[150:295] ~ circjet2$lng[150:295])
summary(lmhubba)

# R² dataframe
r2flydf <- as.data.frame(r2values)
f2flydf <- data.frame(t(r2flydf))


############### DUPLIKAT-SPOTTING I FLYDATA ###############

samlet_flydata$latlong <- paste0(
  samlet_flydata$path.2, "_",
  samlet_flydata$path.3, "_",
  samlet_flydata$icao24
)

duplicates <- samlet_flydata[duplicated(samlet_flydata$latlong), ]


############### SIMPLE OUTLIER-DETEKTION PÅ HØJDE ###############

threshold <- 300   # højde-afvigelse
window <- 20       # hvor langt frem i tiden sammenlignes

resultater <- list()
counter <- 1
counterstrike <- 1

x <- samlet_flydata$path.5  # højdeværdier

for (i in 1:(length(x)-window)) {
  counterstrike <- counterstrike + 1
  
  # Finder store hop i højde → mulig manøvre
  if (any(abs(x[i] - x[(i+1):(i+window)]) > threshold)) {
    resultater[[counter]] <- list(
      samlet_flydata$icao24[counterstrike]
    )
    counter <- counter + 1
  }
}

# Unikke cirkel-fly baseret på højde-outliers
cirkelmus <- as.data.frame(resultater)
transcirkelmus <- as.data.frame(t(cirkelmus))
uniktranscirkelmus <- unique(transcirkelmus$V1)


############### SAMMENFØR CIRKEL & ALLE FLYDATA ###############

testdf <- bind_rows(allecircfly, samlet_flydata)


############### R²-BASERET CIRKELDETEKTION FOR CIRCJET-FILER ###############

sources <- unique(allecircfly$source)
r2valuescirc <- numeric(length(sources))

for (i in seq_along(sources)) {
  df_sub <- allecircfly %>% filter(source == sources[i])
  
  lm_temp <- lm(lat ~ lng, data = df_sub)
  r2valuescirc[i] <- summary(lm_temp)$r.squared
}

circjetmus <- data.frame(
  ICAO = unique(allecircfly$icao24),
  r2valuescirc = r2valuescirc
)

circjetmus$algoryyytm <- circjetmus$ICAO %in% ICAO
