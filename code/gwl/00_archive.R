library(tidyverse)

m    <- read_csv("~/Desktop/periodic_gwl_bulkdatadownload/measurements.csv")
perf <- read_csv("~/Desktop/periodic_gwl_bulkdatadownload/perforations.csv")
sta  <- read_csv("~/Desktop/periodic_gwl_bulkdatadownload/stations.csv")

# join
m <- left_join(m, perf, by = "SITE_CODE") %>% 
  left_join(sta, by = "SITE_CODE")

# sites to find
swn <- c("07N05E18C001M",  
         "07N05E26P002M",  
         "07N05E29D001M",  
         "07N05E36A001M",  
         "07N06E08H001M",  
         "07N06E12A001M",  
         "07N06E14Q001M",  
         "07N06E20J001M",  
         "07N06E22R002M",  
         "08N04E36L001M",  
         "08N05E21H002M",  
         "08N06E17H001M",  
         "08N06E20R001M",  
         "08N06E26K001M",  
         "08N06E27H002M",  
         "08N06E27N001M",  
         "08N06E30C001M",  
         "08N06E31F001M", 
         "08N06E34R001M", 
         "08N07E02N001M", 
         "08N07E14C001M", 
         "08N07E31J001M", 
         "08N07E33E001M", 
         "09N06E33R001M", 
         "09N07E02G001M", 
         "09N07E02N001M", 
         "10N08E29J001M")

# visualize
m %>% 
  filter(SWN %in% swn) %>%
  ggplot(aes(MSMT_DATE, WSE, color = SWN)) +
  geom_line()

# subset to the site codes
temp <- m %>% filter(SWN %in% swn) 
n <- unique(temp$SWN)

# write csvs
for(i in 1:length(n)){
  temp %>% 
    filter(SWN == n[i]) %>% 
    write_csv(paste0(n[i], ".csv"))
}

write_csv(temp, "all_gwls.csv")
