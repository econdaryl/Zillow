timeonmarket <- function(){
  download.file("http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv", "cross.csv")
  cross <- read_csv("cross.csv")
  
  cross <- cross %>%
    mutate(MetroName_Zillow = ifelse(MetroName_Zillow=="Miami, FL", "Miami-Fort Lauderdale, FL", MetroName_Zillow),
           MetroName_Zillow = ifelse(MetroName_Zillow=="Los Angeles, CA", "Los Angeles-Long Beach-Anaheim, CA", MetroName_Zillow),
           CBSA = CBSACode) %>%
    drop_na(CBSA, MetroName_Zillow) %>%
    group_by(CBSA, MetroName_Zillow) %>%
    summarise(n()) %>%
    select(CBSA, MetroName_Zillow)
  
  download.file("http://files.zillowstatic.com/research/public/Metro/DaysOnZillow_Msa.csv", "daysonzillow.csv")
  daysonzillow <- read_csv("daysonzillow.csv")
  
  ptiles <- daysonzillow %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    group_by(name) %>%
    mutate(p10 = quantile(value, .1, na.rm = TRUE),
           p25 = quantile(value, .25,na.rm = TRUE),
           p50 = quantile(value, .5, na.rm = TRUE),
           p75 = quantile(value, .75,na.rm = TRUE),
           p90 = quantile(value, .9, na.rm = TRUE)) %>%
    select(name, p10, p25, p50, p75, p90) %>%
    distinct()
  
  days <- daysonzillow %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    select(RegionName, name, value) %>%
    left_join(cross, by=c("RegionName" = "MetroName_Zillow")) %>%
    mutate(CBSA = ifelse(RegionName == "United States", 0, CBSA)) %>%
    drop_na(CBSA) %>%
    select(name, value, CBSA) %>%
    pivot_wider(id_cols = name, names_from = CBSA, values_from = value) %>%
    select(name, `0`, `38060`, `33100`, `31080`) %>%
    full_join(ptiles, by = "name") %>%
    mutate(date = as.Date(paste0(name, "-01")))
  
  us <- tis(days$`0`, start = min(days$date), freq = 12)
  pho <- tis(days$`38060`, start = min(days$date), freq = 12)
  mia <- tis(days$`33100`, start = min(days$date), freq = 12)
  la <- tis(days$`31080`, start = min(days$date), freq = 12)
  p10 <- tis(days$p10, start = min(days$date), freq = 12)
  p25 <- tis(days$p25, start = min(days$date), freq = 12)
  p50 <- tis(days$p50, start = min(days$date), freq = 12)
  p75 <- tis(days$p75, start = min(days$date), freq = 12)
  p90 <- tis(days$p90, start = min(days$date), freq = 12)
  
  us_adj <- x12(new("x12Single", ts = as.ts(us)))
  us_adj <- tis(as.numeric(us_adj@x12Output@d11), start = min(days$date), frequency = 12)
  pho_adj <- x12(new("x12Single", ts = as.ts(pho)))
  pho_adj <- tis(as.numeric(pho_adj@x12Output@d11), start = min(days$date), frequency = 12)
  mia_adj <- x12(new("x12Single", ts = as.ts(mia)))
  mia_adj <- tis(as.numeric(mia_adj@x12Output@d11), start = min(days$date), frequency = 12)
  la_adj <- x12(new("x12Single", ts = as.ts(la)))
  la_adj <- tis(as.numeric(la_adj@x12Output@d11), start = min(days$date), frequency = 12)
  p10_adj <- x12(new("x12Single", ts = as.ts(p10)))
  p10_adj <- tis(as.numeric(p10_adj@x12Output@d11), start = min(days$date), frequency = 12)
  p25_adj <- x12(new("x12Single", ts = as.ts(p25)))
  p25_adj <- tis(as.numeric(p25_adj@x12Output@d11), start = min(days$date), frequency = 12)
  p50_adj <- x12(new("x12Single", ts = as.ts(p50)))
  p50_adj <- tis(as.numeric(p50_adj@x12Output@d11), start = min(days$date), frequency = 12)
  p75_adj <- x12(new("x12Single", ts = as.ts(p75)))
  p75_adj <- tis(as.numeric(p75_adj@x12Output@d11), start = min(days$date), frequency = 12)
  p90_adj <- x12(new("x12Single", ts = as.ts(p90)))
  p90_adj <- tis(as.numeric(p90_adj@x12Output@d11), start = min(days$date), frequency = 12)
  
  system("rm Rout*")
  system("rm -r gra_Rout/")
  
  data <- data.frame(us_adj = us_adj, pho_adj = pho_adj, mia_adj = mia_adj,
                     la_adj = la_adj, p10_adj = p10_adj, p25_adj = p25_adj,
                     p50_adj = p50_adj, p75_adj = p75_adj, p90_adj = p90_adj, date = days$date)
  
  ggplot(data, aes(x = date)) +
    ggtitle("Time on the Market") +
    geom_ribbon(aes(ymin = p10_adj, ymax = p90_adj), fill = "skyblue", alpha = .7) +
    geom_ribbon(aes(ymin = p25_adj, ymax = p75_adj), fill = "steelblue", alpha = .7) +
    geom_line(aes(y = us_adj)) +
    geom_line(aes(y = pho_adj), col = "navyblue") +
    geom_line(aes(y = mia_adj), col = "goldenrod") +
    geom_line(aes(y = la_adj), col = "purple") +
    geom_line(aes(y = p50_adj), col = "forestgreen") +
    geom_segment(aes(x = as.Date("2010-05-01"), y = 165, xend = as.Date("2010-08-01"), yend = 175), size = .1) +
    geom_segment(aes(x = as.Date("2012-07-01"), y = 130, xend = as.Date("2012-09-01"), yend = 165), size = .1) +
    annotate("text", x = as.Date("2010-09-01"), y = 175, label = "P10-P90", size = 2, hjust=0, color = "skyblue") +
    annotate("text", x = as.Date("2012-10-01"), y = 165, label = "P25-P75", size = 2, hjust=0, color = "steelblue") +
    annotate("text", x = as.Date("2010-02-01"), y = 185, label = "MIA", size = 2, hjust = 0, color = "goldenrod") +
    annotate("text", x = as.Date("2012-01-01"), y = 82,  label = "PHO", size = 2, hjust = 0, color = "navyblue") +
    annotate("text", x = as.Date("2010-09-01"), y = 92,  label = "LA",  size = 2, hjust = 0, color = "purple") +
    annotate("text", x = as.Date("2010-01-01"), y = 140, label = "US",  size = 2, hjust = 0) +
    annotate("text", x = as.Date("2010-07-01"), y = 130, label = "Median", size = 2, hjust = 0, color = "forestgreen") +
    xlab("") + 
    ylab("") +
    labs(caption = "Note: Seasonally Adjusted") +
    scale_y_continuous(position = "right") +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0))
}