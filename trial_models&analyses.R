# =============================================================
# NCHA-IIIb (Spring 2026, UTK)
#
# Checking the validity of my tidying and cleanup by
# running analyses that can be compared to the UTK
# executive summary and the NCHA general summary. 
# Substance use (3 types, 1 subtype), first-gen student 
# outcomes, and substance use x belonging, 
# loneliness, and wellbeing correlation and regression.
# I will also create visualizations to aid in explaining
# the data.
#
# =============SUBSTANCE USE ANALYSIS==========================
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

# ============FIRST-GENERATION ANALYSIS=========================

# ---- 1. Did I build first_gen right? ------------------------
# ACHA already scored this and got 25.6%. If mine is close, the
# cutoff is right. If it's off by a lot, it's wrong.

pct <- 100 * mean(dat$first_gen == "First-gen", na.rm = TRUE)

cat(sprintf("Mine: %.1f%%   ACHA: 25.6%%\n\n", pct))
print(table(dat$first_gen, useNA = "ifany"))

if (abs(pct - 25.6) > 2) warning("Off from ACHA. Check the cutoff first.")


# ---- 2. Who are they? ---------------------------------------
# Before comparing outcomes, check whether the two groups differ
# on anything else. If first-gen students are also older or more
# often grad students, a distress gap might just be an age gap.

for (g in c("class_yr", "housing", "greek_any", "athlete", "disability")) {
  if (g %in% names(dat)) {
    cat("\n--", g, "--\n")
    print(round(100 * prop.table(table(dat[[g]], dat$first_gen), 2), 1))
  }
}

cat("\n-- age --\n")
print(dat %>% group_by(first_gen) %>%
        summarise(n = n(), age = mean(age, na.rm = TRUE)))


# ---- 3. The comparison table --------------------------------
# Average score for each group, side by side, with a test for
# whether the gap is bigger than chance. No controls yet.

outs <- c(k6          = "Distress (0-24)",
          flourishing = "Wellbeing (8-56)",
          loneliness  = "Loneliness (3-9)",
          belonging   = "Belonging (4-24)",
          safety      = "Safety (4-16)",
          cdrisc2     = "Resilience (0-8)")

tab <- lapply(names(outs), function(v) {
  d <- dat[!is.na(dat$first_gen) & !is.na(dat[[v]]), ]
  m <- tapply(d[[v]], d$first_gen, mean)
  s <- tapply(d[[v]], d$first_gen, sd)
  n <- tapply(d[[v]], d$first_gen, length)
  data.frame(measure = outs[v],
             cont = sprintf("%.1f (%.1f)", m[1], s[1]),
             first = sprintf("%.1f (%.1f)", m[2], s[2]),
             diff = round(m[2] - m[1], 2),
             p = t.test(d[[v]] ~ d$first_gen)$p.value)
}) %>% bind_rows()

tab$p_adj <- round(p.adjust(tab$p, "BH"), 3)
tab$p     <- round(tab$p, 3)

cat("\n===== Scores by group: mean (SD) =====\n")
print(tab, row.names = FALSE)
write.csv(tab, "out/firstgen_table1.csv", row.names = FALSE)


# ---- 4. Does the gap survive controls? ----------------------
# Section 3 asks "are they different." This asks "are they
# different for reasons other than age and year in school."
# If a gap vanishes here, the raw one was really an age gap.

ctrl <- c("age", "class_yr")
ctrl <- ctrl[ctrl %in% names(dat)]

adj <- lapply(names(outs), function(v) {
  fit <- lm(reformulate(c("first_gen", ctrl), v), data = dat)
  r   <- grep("^first_gen", rownames(summary(fit)$coefficients))
  ci  <- confint(fit)[r, ]
  data.frame(measure = outs[v],
             est = round(coef(fit)[r], 2),
             low = round(ci[1], 2), high = round(ci[2], 2),
             p = summary(fit)$coefficients[r, 4],
             n = nobs(fit))
}) %>% bind_rows()

adj$p_adj <- round(p.adjust(adj$p, "BH"), 3)
adj$p     <- round(adj$p, 3)

cat("\n===== Adjusted for", paste(ctrl, collapse = " + "), "=====\n")
cat("est = points higher for first-gen. If low-high crosses 0, no clear gap.\n\n")
print(adj, row.names = FALSE)
write.csv(adj, "out/firstgen_adjusted.csv", row.names = FALSE)

