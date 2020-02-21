hp_growth <- function(){
  data <- read_csv("zhvi.csv")
  data <- data %>%
    filter(RegionID == 102001) %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    separate(name, sep = "-", into = c("year", "month")) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date) %>%
    mutate(growth = 1200*(value - lag(value))/lag(value)) %>%
    filter(year(date) >= 2011)
  enddate <- month.abb[as.numeric(last(month(data$date)))]
  
  ggplot(data, aes(x = date, y = growth)) +
    geom_line(color = "red") +
    geom_line(aes(y = 0)) +
    ggtitle("Growth of Nominal Prices of Existing Homes", "Percent change, annual rate") +
    xlab("") + 
    ylab("") +
    scale_y_continuous(position = "right") +
    annotate("text", x=max(data$date)+60, y=data$growth[data$date==max(data$date)], label = enddate, hjust=0, size=3) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0))
}
