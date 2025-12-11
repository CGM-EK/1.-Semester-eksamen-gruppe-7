
install.packages("restatapi")
library(restatapi)
install.packages("eurostat")
library(eurostat)

toc<-get_eurostat_toc()

mySearches<-function(searchstr) {
  searchstr="consumption"
  tmpdf<-toc %>% filter(str_detect(title,searchstr)) %>%
    filter(type=="dataset") %>%
    select(title,code)
  return(tmpdf)
}
list <- mySearches("expenditure")

dfconsum <- get_eurostat_data("namq_10_fcs",
                              filters = list(
                                geo="*",
                                na_item="P31_S14",
                                s_adj="SCA",
                                unit="CLV20_MEUR"),
                              date_filter=">1999",
                              label=TRUE,
                              name=FALSE)

pivotland <- pivot_wider(
  data = dfconsum,
  names_from = geo,
  values_from = values
)
landyear <- seq.Date(from = as.Date("2000-01-01"),
                     to = as.Date("2025-09-30"),
                     by = "quarter")

landpfv <- as.data.frame(landyear)

names(landpfv)[names(landpfv) == "lmyear"] <- "year"
unique(dfconsum$geo)
landpfv$PfvAT <- c(0, diff(log(pivotland$Austria),lag=4)*100)[-1]
landpfv$PfvBA <- c(0, diff(log(pivotland$`Bosnia and Herzegovina`),lag=4)*100)[-1]
landpfv$PfvBE <- c(0, diff(log(pivotland$Belgium),lag=4)*100)[-1]
landpfv$PfvBG <- c(0, diff(log(pivotland$Bulgaria),lag=4)*100)[-1]
landpfv$PfvCH <- c(0, diff(log(pivotland$Switzerland),lag=4)*100)[-1]
landpfv$PfvCY <- c(0, diff(log(pivotland$Cyprus),lag=4)*100)[-1]
landpfv$PfvCZ <- c(0, diff(log(pivotland$Czechia),lag=4)*100)[-1]
landpfv$PfvGE <- c(0, diff(log(pivotland$Germany),lag=4)*100)[-1]
landpfv$PfvDK <- c(0, diff(log(pivotland$Denmark),lag=4)*100)[-1]
landpfv$PfvEE <- c(0, diff(log(pivotland$Estonia),lag=4)*100)[-1]
landpfv$PfvGR <- c(0, diff(log(pivotland$Greece),lag=4)*100)[-1]
landpfv$PfvES <- c(0, diff(log(pivotland$Spain),lag=4)*100)[-1]
landpfv$PfvFI <- c(0, diff(log(pivotland$Finland),lag=4)*100)[-1]
landpfv$PfvFR <- c(0, diff(log(pivotland$France),lag=4)*100)[-1]
landpfv$PfvHR <- c(0, diff(log(pivotland$Croatia),lag=4)*100)[-1]
landpfv$PfvHU <- c(0, diff(log(pivotland$Hungary),lag=4)*100)[-1]
landpfv$PfvIE <- c(0, diff(log(pivotland$Ireland),lag=4)*100)[-1]
landpfv$PfvIS <- c(0, diff(log(pivotland$Iceland),lag=4)*100)[-1]
landpfv$PfvIT <- c(0, diff(log(pivotland$Italy),lag=4)*100)[-1]
landpfv$PfvLT <- c(0, diff(log(pivotland$Lithuania),lag=4)*100)[-1]
landpfv$PfvLU <- c(0, diff(log(pivotland$Luxembourg),lag=4)*100)[-1]
landpfv$PfvLV <- c(0, diff(log(pivotland$Latvia),lag=4)*100)[-1]
landpfv$PfvME <- c(0, diff(log(pivotland$Montenegro),lag=4)*100)[-1]
landpfv$PfvMT <- c(0, diff(log(pivotland$Malta),lag=4)*100)[-1]
landpfv$PfvNL <- c(0, diff(log(pivotland$Netherlands),lag=4)*100)[-1]
landpfv$PfvNO <- c(0, diff(log(pivotland$Norway),lag=4)*100)[-1]
landpfv$PfvPL <- c(0, diff(log(pivotland$Poland),lag=4)*100)[-1]
landpfv$PfvPT <- c(0, diff(log(pivotland$Portugal),lag=4)*100)[-1]
landpfv$PfvRO <- c(0, diff(log(pivotland$Romania),lag=4)*100)[-1]
landpfv$PfvRS <- c(0, diff(log(pivotland$Serbia),lag=4)*100)[-1]
landpfv$PfvSE <- c(0, diff(log(pivotland$Sweden),lag=4)*100)[-1]
landpfv$PfvSI <- c(0, diff(log(pivotland$Slovenia),lag=4)*100)[-1]
landpfv$PfvSK <- c(0, diff(log(pivotland$Slovakia),lag=4)*100)[-1]

mean(landpfv$PfvAT)
mean(landpfv$PfvBA)
mean(landpfv$PfvBE)
mean(landpfv$PfvBG)
mean(landpfv$PfvCH)
mean(landpfv$PfvCY)
mean(landpfv$PfvCZ)
mean(landpfv$PfvGE)
mean(landpfv$PfvDK)
mean(landpfv$PfvEE)
mean(landpfv$PfvGR)
mean(landpfv$PfvES)
mean(landpfv$PfvFI)
mean(landpfv$PfvFR)
mean(landpfv$PfvHR)
mean(landpfv$PfvHU)
mean(landpfv$PfvIE)
mean(landpfv$PfvIS)
mean(landpfv$PfvIT)
mean(landpfv$PfvLT)
mean(landpfv$PfvLU)
mean(landpfv$PfvLV)
mean(landpfv$PfvME)
mean(landpfv$PfvMT)
mean(landpfv$PfvNL)
mean(landpfv$PfvNO)
mean(landpfv$PfvPL)
mean(landpfv$PfvPT)
mean(landpfv$PfvRO)
mean(landpfv$PfvRS)
mean(landpfv$PfvSE)
mean(landpfv$PfvSI)
mean(landpfv$PfvSK)

     

#81-89

landpfvu <-  as.data.frame(landpfv[-c(81:89),])

mean(landpfvu$PfvAT)
mean(landpfvu$PfvBA)
mean(landpfvu$PfvBE)
mean(landpfvu$PfvBG)
mean(landpfvu$PfvCH)
mean(landpfvu$PfvCY)
mean(landpfvu$PfvCZ)
mean(landpfvu$PfvGE)
mean(landpfvu$PfvDK)
mean(landpfvu$PfvEE)
mean(landpfvu$PfvGR)
mean(landpfvu$PfvES)
mean(landpfvu$PfvFI)
mean(landpfvu$PfvFR)
mean(landpfvu$PfvHR)
mean(landpfvu$PfvHU)
mean(landpfvu$PfvIE)
mean(landpfvu$PfvIS)
mean(landpfvu$PfvIT)
mean(landpfvu$PfvLT)
mean(landpfvu$PfvLU)
mean(landpfvu$PfvLV)
mean(landpfvu$PfvME)
mean(landpfvu$PfvMT)
mean(landpfvu$PfvNL)
mean(landpfvu$PfvNO)
mean(landpfvu$PfvPL)
mean(landpfvu$PfvPT)
mean(landpfvu$PfvRO)
mean(landpfvu$PfvRS)
mean(landpfvu$PfvSE)
mean(landpfvu$PfvSI)
mean(landpfvu$PfvSK)


landpfvm <-  as.data.frame(landpfv[c(81:89),])

mean(landpfvm$PfvAT)
mean(landpfvm$PfvBA)
mean(landpfvm$PfvBE)
mean(landpfvm$PfvBG)
mean(landpfvm$PfvCH)
mean(landpfvm$PfvCY)
mean(landpfvm$PfvCZ)
mean(landpfvm$PfvGE)
mean(landpfvm$PfvDK)
mean(landpfvm$PfvEE)
mean(landpfvm$PfvGR)
mean(landpfvm$PfvES)
mean(landpfvm$PfvFI)
mean(landpfvm$PfvFR)
mean(landpfvm$PfvHR)
mean(landpfvm$PfvHU)
mean(landpfvm$PfvIE)
mean(landpfvm$PfvIS)
mean(landpfvm$PfvIT)
mean(landpfvm$PfvLT)
mean(landpfvm$PfvLU)
mean(landpfvm$PfvLV)
mean(landpfvm$PfvME)
mean(landpfvm$PfvMT)
mean(landpfvm$PfvNL)
mean(landpfvm$PfvNO)
mean(landpfvm$PfvPL)
mean(landpfvm$PfvPT)
mean(landpfvm$PfvRO)
mean(landpfvm$PfvRS)
mean(landpfvm$PfvSE)
mean(landpfvm$PfvSI)
mean(landpfvm$PfvSK)

