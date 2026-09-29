#==============CLASS YEAR X THE SIX SCALES=============
#
# Do the different scale scores differ by year in school?
#
# Built so I can drop in the 2020 and 2023 files later without
# rewriting anything.
#
# One thing to be clear about up front: these are three separate
# groups of students, not the same students followed over time.

library(tidyverse)
library(psych)

# ---- 1. Load the years ---------------------------------------
# Add a line per survey year. The name on the left is what shows
# up in the tables and on the chart.

files <- c(
  "2026" = "2026 data/ncha_clean_2026.rds"
  #"2023" = "2023 data/ncha_clean_2023.rds",
  #"2020" = "2020 data/ncha_clean_2020.rds"
)

scales <- c(flourishing = "Wellbeing (8-56)",
            k6          = "Distress (0-24)",
            loneliness  = "Loneliness (3-9)",
            belonging   = "Belonging (4-24)",
            safety      = "Safety (4-16)",
            cdrisc2     = "Resilience (0-8)")

load_year <- function(path, yr) {
  d <- readRDS(path)
  need <- c(names(scales), "class_yr")
  miss <- setdiff(need, names(d))
  if (length(miss))
    stop(yr, " is missing: ", paste(miss, collapse = ", "))
  d %>%
    select(all_of(need)) %>%
    mutate(year = yr)
}

dat <- imap(files, ~ load_year(.x, .y)) %>% bind_rows()

dat$year <- factor(dat$year, levels = names(files))

cat("Years loaded:", paste(names(files), collapse = ", "), "\n")
print(table(dat$year, useNA = "ifany"))


# ---- 2. Short labels and small groups ------------------------
# The real class year labels are far too long to sit under a bar
# ("Master's (MA, MS, MFA, MBA, MPP, MPA, MPH, etc)"). Short names
# instead. Anything unmatched keeps its original text, so a
# wording change in a future export can't blank a group.
#
# Then drop any year-by-class-year cell under 20 students. An
# average built from 4 people is not an average.

short <- function(x) {
  case_when(
    grepl("^1st",   x) ~ "1st yr",
    grepl("^2nd",   x) ~ "2nd yr",
    grepl("^3rd",   x) ~ "3rd yr",
    grepl("^4th",   x) ~ "4th yr",
    grepl("^5th",   x) ~ "5th yr+",
    grepl("Master", x) ~ "Master's",
    grepl("Doctor", x) ~ "Doctoral",
    TRUE               ~ as.character(x)
  )
}

lev <- c("1st yr", "2nd yr", "3rd yr", "4th yr", "5th yr+",
         "Master's", "Doctoral")

dat <- dat %>%
  filter(!is.na(class_yr)) %>%
  mutate(cy = short(class_yr),
         cy = factor(cy, levels = c(lev, setdiff(unique(cy), lev))))

keep <- dat %>%
  summarise(n = n(), .by = c(year, cy)) %>%
  filter(n >= 20)

cat("\nGroups kept (n >= 20):\n")
print(as.data.frame(keep), row.names = FALSE)

dat <- dat %>%
  semi_join(keep, by = c("year", "cy")) %>%
  droplevels()


# ---- 3. The numbers ------------------------------------------
# Mean and SD for every scale, in every class year, in every
# survey year. This is the table everything else summarizes.

long <- dat %>%
  pivot_longer(all_of(names(scales)),
               names_to = "scale", values_to = "score") %>%
  filter(!is.na(score)) %>%
  mutate(scale = factor(scales[scale], levels = scales))

tab <- long %>%
  summarise(n    = n(),
            mean = mean(score),
            sd   = sd(score),
            se   = sd(score) / sqrt(n()),
            .by  = c(scale, year, cy)) %>%
  arrange(scale, year, cy)

cat("\n===== Scores by class year =====\n\n")
tab %>%
  mutate(across(c(mean, sd, se), ~ round(., 2))) %>%
  print(n = Inf)

write.csv(tab, "2026 out/classyr_scales.csv", row.names = FALSE)


# ---- 4. Is the spread real? ----------------------------------
# One model per scale.
#
# With a single survey year the question is just "do the class
# years differ." Once I add 2020 and 2023 the model also asks
# whether that pattern changed between years.

multi <- nlevels(dat$year) > 1

cat("\n===== Models =====\n")
cat(if (multi) "score ~ class year * survey year\n\n"
    else       "score ~ class year\n\n")

fits <- lapply(levels(long$scale), function(s) {
  
  d <- long %>% filter(scale == s)
  
  f <- if (multi) score ~ cy * year else score ~ cy
  m <- lm(f, data = d)
  a <- anova(m)
  
  out <- data.frame(
    scale   = s,
    term    = rownames(a)[-nrow(a)],
    F       = a$`F value`[-nrow(a)],
    p       = a$`Pr(>F)`[-nrow(a)],
    n       = nobs(m),
    row.names = NULL
  )
  out$r2 <- summary(m)$r.squared
  out
}) %>% bind_rows()

fits$p_adj <- p.adjust(fits$p, "BH")

fits %>%
  mutate(across(c(F, r2), ~ round(., 3)),
         across(c(p, p_adj), ~ round(., 4))) %>%
  print(row.names = FALSE)

write.csv(fits, "2026 out/classyr_scales_models.csv", row.names = FALSE)

cat("\nterm 'cy'      = do the class years differ\n")
if (multi) {
  cat("term 'year'    = did the campus shift overall between surveys\n")
  cat("term 'cy:year' = did the class year pattern itself change\n")
}
cat("\nr2 is how much of the score each model explains. These will\n")
cat("be small. Class year is one of many things driving a score.\n")


# ---- 5. The chart --------------------------------------------
# One panel per scale, free y axis because a 3-9 scale and a 8-56
# scale can't share one.
#
# With a single survey year this is a line across class years.
# With three it becomes one line per year.

f <- ggplot(tab, aes(cy, mean, group = year, color = year)) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.15, linewidth = 0.6, alpha = 0.7) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  facet_wrap(~ scale, scales = "free_y") +
  scale_color_manual(values = c("#B3352E", "#4C72B0", "#8C8C8C"),
                     name = NULL) +
  labs(title = "Scores by year in school (2026)",
       subtitle = "Bars are 95% confidence intervals.",
       x = NULL, y = "Mean score",
       caption = "NCHA-III | UTK") +
  theme_minimal(base_size = 13) +
  theme(legend.position = if (multi) "top" else "none",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey30", size = 9),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(face = "bold"))

print(f)
ggsave("2026 out/fig_classyr_scales.png", f, width = 11, height = 7, dpi = 300)
