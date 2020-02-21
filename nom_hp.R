nom_hp <- function(){
  download.file("http://files.zillowstatic.com/research/public/Metro/Metro_Zhvi_AllHomes.csv", destfile = "zhvi.csv")
  
  data <- read_csv("zhvi.csv")
  data <- data %>%
    filter(RegionID == 102001) %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    separate(name, sep = "-", into = c("year", "month")) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date)
  
  enddate <- month.abb[as.numeric(last(data$month))]
  
  data$norm <- data$value[data$date == as.Date("2007-04-01")]
  
  data$adj <- data$value/data$norm
  
  ggplot(data, aes(x = date, y = adj)) +
    geom_line(color = "red") +
    ggtitle("Nominal Prices of Existing Homes", "Index Normalized to 2007 Peak = 100") +
    xlab("") + 
    ylab("") +
    scale_y_continuous(position = "right") +
    annotate("text", x=max(data$date)+90, y=data$adj[data$date==max(data$date)], label = enddate, hjust = 0, size=3) +
    theme_bw()
}
