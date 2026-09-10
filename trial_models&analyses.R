# =============================================================
# NCHA-IIIb (Spring 2026, UTK)
#
# Checking the validity of my tidying and cleanup by
# running analyses that can be compared to the UTK
# executive summary and the NCHA general summary. 
# Substance use (4 types), first-gen student 
# outcomes, and substance use x belonging, 
# loneliness, and wellbeing correlation and regression.
# I will also create visualizations to aid in explaining
# the data.
#
#
# ------- 1. Substance use among UTK students ------- 
# Looking at alcohol, binge drinking, nicotine, and cannabis usage at UTK.

library(tidyverse)
dat <- readRDS("data/ncha_clean.rds")

nic <- grep("^R?N3Q23[A-K]$", names(dat), value = TRUE)

dat <- dat %>% mutate(
  alcohol  = as.integer(N3Q25A %in% 2:4),
  binge    = if_else(is.na(N3Q28), 0L, as.integer(N3Q28 >= 2)),
  cannabis = as.integer(N3Q24 %in% 2:4),
  nicotine = as.integer(rowSums(dat[nic] == 1, na.rm = TRUE) > 0)
)

dat %>%
  summarise(across(c(alcohol, binge, cannabis, nicotine),
                   ~ 100 * mean(., na.rm = TRUE)))

prev <- tibble(
  substance = c("Alcohol", "Binge drinking", "Cannabis", "Nicotine"),
  pct       = c(69.8, 28.2, 21.5, 23.6)
)

# My UTK stats (past 3 months; binge drinking is 
# past 2 weeks): 69.8% reported using alchohol,
# 28.2% reported binge drinking, 23.6% reported
# using nicotine products, and 21.5% reported
# using cannabis.
#
# NCHA UTK stats (past 3 months; binge drinking is 
# past 2 weeks): 69.2% reported using alchohol,
# 28.3% reported binge drinking, 23.9% reported
# using nicotine products, and 23.9% reported
# using cannabis.
#
# NCHA reference group stats (past 3 months; binge drinking 
# is past 2 weeks): 56.2% reported using alchohol,
# 17.4% reported binge drinking, 18.2% reported
# using nicotine products, and 22.9% reported
# using cannabis.
#
# ------- 2. Visualizing the substance use data -------
# Now, I will build a simple visualization of my
# UTK data for easy comparison between sources.

ggplot(prev, aes(x = reorder(substance, pct), y = pct)) +
  geom_col(fill = "orange", width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.2) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  labs(title = "Substance use among UTK students",
       subtitle = "Past 3 months; binge drinking is past 2 weeks",
       x = NULL, y = "% of students") +
  theme_minimal()