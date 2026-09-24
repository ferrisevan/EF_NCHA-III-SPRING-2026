#==============ALCOHOL AND BINGE DRINKING BY CLASS YEAR=============
#
#Does drinking change as students move through school? Freshman to senior to grad.
#
# No models, no controls. Just the percentage in each year.

library(tidyverse)

dat <- readRDS("data/ncha_clean_2026.rds")

# ---- 1. Mark who drinks ---------------------------------------
# Same two definitions I've used all along. Alcohol is any use in
# the last 3 months. Binge is at least one episode in the last 2
# weeks, and a blank there means the student was skipped because
# they don't drink, so it counts as no.

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  stopifnot(length(h) == 1)
  h
}

q25 <- col1("^R?N3Q25A$")    # alcohol, how recently
q28 <- col1("^R?N3Q28$")     # binge episodes, last 2 weeks

dat <- dat %>%
  mutate(
    alcohol = as.integer(.data[[q25]] %in% 2:4),
    binge   = if_else(is.na(.data[[q28]]), 0L, as.integer(.data[[q28]] >= 2))
  )

# ---- 2. Drop the tiny groups ----------------------------------
# "Not seeking a degree" and "Other" have a handful of students
# each.

tab <- dat %>%
  filter(!is.na(class_yr)) %>%
  summarise(n       = n(),
            alcohol = 100 * mean(alcohol),
            binge   = 100 * mean(binge),
            .by     = class_yr) %>%
  filter(n >= 30) %>%
  arrange(class_yr)

cat("===== Drinking by class year =====\n\n")
print(as.data.frame(tab %>% mutate(across(c(alcohol, binge), ~ round(., 1)))),
      row.names = FALSE)

# ---- 3. Is the spread real? -----------------------------------
# Chi-square asks are these percentages further apart
# than you'd expect from random variation?

d <- dat %>% filter(class_yr %in% tab$class_yr) %>% droplevels()

cat("\n-- Any alcohol --\n")
print(chisq.test(table(d$class_yr, d$alcohol)))

cat("\n-- Binge drinking --\n")
print(chisq.test(table(d$class_yr, d$binge)))

# ---- 4. The chart ---------------------------------------------
# Two bars per year, side by side.

short <- function(x) {
  case_when(
    grepl("^1st",      x) ~ "1st yr",
    grepl("^2nd",      x) ~ "2nd yr",
    grepl("^3rd",      x) ~ "3rd yr",
    grepl("^4th",      x) ~ "4th yr",
    grepl("^5th",      x) ~ "5th yr+",
    grepl("Master",    x) ~ "Master's",
    grepl("Doctor",    x) ~ "Doctoral",
    TRUE                  ~ as.character(x)
  )
}

plot_dat <- tab %>%
  pivot_longer(c(alcohol, binge), names_to = "measure", values_to = "pct") %>%
  mutate(measure = recode(measure,
                          alcohol = "Any alcohol (3 mo)",
                          binge   = "Binge drinking (2 wks)"),
         label   = paste0(short(class_yr), "\n(n = ", n, ")"),
         label   = factor(label, levels = unique(label)))

f <- ggplot(plot_dat, aes(label, pct, fill = measure)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_dodge(0.75), vjust = -0.4,
            size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("Any alcohol (3 mo)"     = "red3",
                               "Binge drinking (2 wks)" = "grey"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, max(plot_dat$pct) * 1.2),
                     expand = c(0, 0)) +
  labs(title = "Drinking by year in school",
       x = NULL, y = "% of students",
       caption = "NCHA-IIIb | UTK | Spring 2026") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 10, lineheight = 0.9))

print(f)
ggsave("out/fig_class_yr_alcohol.png", f, width = 9, height = 5.5, dpi = 300)

# ---- 5. Reading it --------------------------------------------
# Campus overall is 69.8% alcohol and 28.3% binge. Years above
# those lines are pulling the campus number up.
#
# What I expected: drinking climbs across the undergrad years and
# drops off for grad students. 
# What the data shows: drinking climbs across the undergrad years,
# plateauing at 79% for seniors master's students and then decreases
# to 67% for doctoral students.

cat("\nCampus overall: 69.8% alcohol, 28.3% binge\n")
