nom_hp <- function(){

  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(x12)
  library(tis)
  
  setwd("/href/scratch3/m1dbl01/Personal/Zillow")
  
  download.file("http://files.zillowstatic.com/research/public/Metro/Metro_Zhvi_AllHomes.csv", destfile = "zhvi.csv")
  
  data <- read_csv("zhvi.csv")
  data <- data %>%
    filter(RegionID == 102001) %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    separate(name, sep = "-", into = c("year", "month")) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date)
  
  enddate <- month.abb[as.numeric(last(data$month))]
  
  zhvi <- tis(data$value, start = min(data$date), frequency = 12)
  
  # set the path of the x12a binary, if this isn't done, then x12 will use the x13as binary
  # located in /opt/R/R-arc-3.4.1/library/x13binary/bin
  x12path("/opt/x12arima/0.3/bin/x12a")
  
  seasadj <- x12(new("x12Single", ts = as.ts(zhvi)))
  
  zhvi_adju <- tis(as.numeric(seasadj@x12Output@d11), start = min(data$date), frequency = 12)
  zhvi_norm <- window(zhvi_adju, start = jul("2007-04-04"), end = jul("2007-04-04"))[1]
  
  zhvi_plot <- data.frame(value = as.numeric(100*zhvi_adju/zhvi_norm), date = data$date)
  
  
  ggplot(zhvi_plot, aes(x = date, y = value)) +
    geom_line() +
    ggtitle("Nominal Prices of Existing Homes", "Index Normalized to 2007 Peak = 100") +
    xlab("") + 
    ylab("") +
    labs(caption = "Note: Seasonally Adjusted") +
    annotate("text", x=max(zhvi_plot$date), y=zhvi_plot$value[zhvi_plot$date==max(zhvi_plot$date)], label = enddate, hjust = 0, size=3) +
    theme_bw() 
}
