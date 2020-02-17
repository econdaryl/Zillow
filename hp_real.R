download.file("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current", "cpi.csv")

hp_real <- function(){
  data <- read_csv("zhvi_adju.csv")
  enddate <- month.abb[as.numeric(last(month(data$date)))]
  
  cpi <- read_tsv("cpi.csv") %>%
    mutate(date = as.Date(paste0(year, "-", substring(period, 2, 3), "-01"))) %>%
    filter(series_id == "CUUR0000SA0",
           period != "M13",
           date <= max(data$date)) %>%
    select(year, period, value)
  
  data <- data %>%
    filter(year(date) >= 1997) %>%
    mutate(real = value/cpi$value)
  
  zl_norm <- data %>%
    filter(year(date) < 2010,
           real == max(real)) %>%
    select(real) %>%
    as.numeric()
  
  data <- data %>%
    mutate(real = 100*real/zl_norm)
  
  ggplot(data, aes(x = date, y = real)) +
    geom_line(color = "red") +
    ggtitle("Real Prices of Existing Homes", "Indexes normalized to 2007 Peak = 100") +
    xlab("") + 
    ylab("") +
    scale_y_continuous(position = "right") +
    labs(caption = "Note: Seasonally Adjusted. CPI obtained from Bureau of Labor Statistics.") +
    annotate("text", x=max(data$date)+90, y=data$real[data$date==max(data$date)], label = enddate, hjust=0, size=3) +
    theme_bw()
}
