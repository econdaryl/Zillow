hp_real <- function(){
  download.file("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current", "cpi.csv")
  data <- read_csv("zhvi.csv") %>%
    filter(RegionID == 102001) %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    separate(name, sep = "-", into = c("year", "month")) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    arrange(date)
  enddate <- month.abb[as.numeric(last(month(data$date)))]
  
  cpi <- read_tsv("cpi.csv") %>%
    mutate(date = as.Date(paste0(year, "-", substring(period, 2, 3), "-01"))) %>%
    filter(series_id == "CUUR0000SA0",
           period != "M13",
           date <= max(data$date),
           date >= min(data$date)) %>%
    select(year, period, value)
  
  data <- data %>%
    filter(year(date) >= 2000) %>%
    mutate(real = value/cpi$value)
  
  zl_norm <- data %>%
    filter(year(date) < 2010) %>%
    filter(real == max(real)) %>%
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
    labs(caption = "Source: Zillow; for CPI, BLS") +
    annotate("text", x=max(data$date)+90, y=data$real[data$date==max(data$date)], label = enddate, hjust=0, size=3) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0))
}
