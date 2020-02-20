supply <- function(){
  download.file("http://files.zillowstatic.com/research/public/Metro/Sale_Counts_Msa.csv", "Sale_Counts_Msa.csv")
  download.file("http://files.zillowstatic.com/research/public/Metro/MonthlyListings_NSA_AllHomes_Metro.csv", "inventory.csv")
  
  sales <- read_csv("Sale_Counts_Msa.csv")
  cross <- suppressWarnings(read_csv("cross.csv"))
  invent <- read_csv("inventory.csv")
  
  cross <- cross %>%
    mutate(MetroName_Zillow = ifelse(MetroName_Zillow=="Miami, FL", "Miami-Fort Lauderdale, FL", MetroName_Zillow),
           MetroName_Zillow = ifelse(MetroName_Zillow=="Los Angeles, CA", "Los Angeles-Long Beach-Anaheim, CA", MetroName_Zillow),
           CBSA = CBSACode) %>%
    drop_na(CBSA, MetroName_Zillow) %>%
    group_by(CBSA, MetroName_Zillow) %>%
    summarise(n()) %>%
    select(CBSA, MetroName_Zillow)
  
  invent <- invent %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    mutate(inv = value) %>%
    select(RegionName, name, inv)
  
  ptiles <- sales %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    select(RegionName, name, value) %>%
    full_join(invent, by = c("RegionName", "name")) %>%
    mutate(supply = inv/value) %>% # Homes for sale divided by homes sold
    group_by(name) %>%
    mutate(p10 = quantile(supply, .1, na.rm = TRUE),
           p25 = quantile(supply, .25,na.rm = TRUE),
           p50 = quantile(supply, .5, na.rm = TRUE),
           p75 = quantile(supply, .75,na.rm = TRUE),
           p90 = quantile(supply, .9, na.rm = TRUE)) %>%
    select(name, p10, p25, p50, p75, p90) %>%
    distinct() %>%
    drop_na(p10)
  
  supply <- sales %>%
    pivot_longer(cols = matches("[0-9]{4}-[0-9]{2}")) %>%
    select(RegionName, name, value) %>%
    full_join(invent, by = c("RegionName", "name")) %>%
    left_join(cross, by = c("RegionName" = "MetroName_Zillow")) %>%
    mutate(supply = inv/value,
           CBSA = ifelse(RegionName == "United States", 0, CBSA)) %>%
    filter(CBSA %in% c(0, 38060, 33100, 31080)) %>% # Phoenix, Miami, and LA
    pivot_wider(id_cols = name, names_from = CBSA, values_from = supply) %>%
    inner_join(ptiles, by = "name") %>%
    mutate(date = as.Date(paste0(name, "-01")))
  
  us <- tis(supply$`0`, start = min(supply$date), freq = 12)
  pho <- tis(supply$`38060`, start = min(supply$date), freq = 12)
  mia <- tis(supply$`33100`, start = min(supply$date), freq = 12)
  la <- tis(supply$`31080`, start = min(supply$date), freq = 12)
  p10 <- tis(supply$p10, start = min(supply$date), freq = 12)
  p25 <- tis(supply$p25, start = min(supply$date), freq = 12)
  p50 <- tis(supply$p50, start = min(supply$date), freq = 12)
  p75 <- tis(supply$p75, start = min(supply$date), freq = 12)
  p90 <- tis(supply$p90, start = min(supply$date), freq = 12)
  
  us_adj <- x12(new("x12Single", ts = as.ts(us)))
  us_adj <- tis(as.numeric(us_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  pho_adj <- x12(new("x12Single", ts = as.ts(pho)))
  pho_adj <- tis(as.numeric(pho_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  mia_adj <- x12(new("x12Single", ts = as.ts(mia)))
  mia_adj <- tis(as.numeric(mia_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  la_adj <- x12(new("x12Single", ts = as.ts(la)))
  la_adj <- tis(as.numeric(la_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  p10_adj <- x12(new("x12Single", ts = as.ts(p10)))
  p10_adj <- tis(as.numeric(p10_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  p25_adj <- x12(new("x12Single", ts = as.ts(p25)))
  p25_adj <- tis(as.numeric(p25_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  p50_adj <- x12(new("x12Single", ts = as.ts(p50)))
  p50_adj <- tis(as.numeric(p50_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  p75_adj <- x12(new("x12Single", ts = as.ts(p75)))
  p75_adj <- tis(as.numeric(p75_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  p90_adj <- x12(new("x12Single", ts = as.ts(p90)))
  p90_adj <- tis(as.numeric(p90_adj@x12Output@d11), start = min(supply$date), frequency = 12)
  
  system("rm Rout*")
  system("rm -r gra_Rout/")
  
  data <- data.frame(us_adj = us_adj, pho_adj = pho_adj, mia_adj = mia_adj,
                     la_adj = la_adj, p10_adj = p10_adj, p25_adj = p25_adj,
                     p50_adj = p50_adj, p75_adj = p75_adj, p90_adj = p90_adj, date = supply$date)
  
  
  ggplot(data, aes(x = date)) +
    ggtitle("Inventory of Homes for Sale") +
    geom_ribbon(aes(ymin = p10_adj, ymax = p90_adj), fill = "skyblue", alpha = .7) +
    geom_ribbon(aes(ymin = p25_adj, ymax = p75_adj), fill = "steelblue", alpha = .7) +
    geom_line(aes(y = us_adj)) +
    geom_line(aes(y = pho_adj), col = "navyblue") +
    geom_line(aes(y = mia_adj), col = "goldenrod") +
    geom_line(aes(y = la_adj), col = "purple") +
    geom_line(aes(y = p50_adj), col = "forestgreen") +
    geom_segment(aes(x = as.Date("2014-09-01"), y = 14, xend = as.Date("2014-11-01"), yend = 15.4), size = .1) +
    geom_segment(aes(x = as.Date("2015-07-01"), y = 7.5, xend = as.Date("2016-01-01"), yend = 12), size = .1) +
    annotate("text", x = as.Date("2014-12-01"), y = 15.4, label = "P10-P90", size = 2, hjust=0, color = "skyblue") +
    annotate("text", x = as.Date("2016-02-01"), y = 12, label = "P25-P75", size = 2, hjust=0, color = "steelblue") +
    annotate("text", x = as.Date("2013-02-01"), y = 5, label = "MIA", size = 2, hjust = 0, color = "goldenrod") +
    annotate("text", x = as.Date("2013-02-01"), y = 1.75,  label = "PHO", size = 2, hjust = 0, color = "navyblue") +
    annotate("text", x = as.Date("2013-05-01"), y = 3.25,  label = "LA",  size = 2, hjust = 0, color = "purple") +
    annotate("text", x = as.Date("2013-10-01"), y = 5.95, label = "US",  size = 2, hjust = 0) +
    annotate("text", x = as.Date("2013-02-01"), y = 7.75, label = "Median", size = 2, hjust = 0, color = "forestgreen") +
    xlab("") + 
    ylab("") +
    labs(caption = "Note: Seasonally Adjusted. Supply is homes for sale divided by homes sold.") +
    scale_y_continuous(position = "right") +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0))
}
