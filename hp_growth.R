hp_growth <- function(){
  library(plotly)
  data <- read_csv("zhvi_adju.csv") %>%
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
    labs(caption = "Note: Seasonally Adjusted") +
    annotate("text", x=max(data$date)+60, y=data$growth[data$date==max(data$date)], label = enddate, hjust=0, size=3) +
    theme_bw()
}