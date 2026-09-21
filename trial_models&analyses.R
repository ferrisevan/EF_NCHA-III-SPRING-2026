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

# ============FIRST-GEN VS. CONT-GEN ANALYSIS=========================
#
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
# whether the gap is bigger than chance.

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

# ---- 5. The chart -------------------------------------------
# The scores use different ranges, so raw averages can't share an
# axis. Converting to standard deviations makes the bars
# comparable to each other.

plot_dat <- lapply(names(outs), function(v) {
  z <- as.numeric(scale(dat[[v]]))
  data.frame(measure = outs[v],
             d = mean(z[dat$first_gen == "First-gen"], na.rm = TRUE) -
               mean(z[dat$first_gen == "Continuing-gen"], na.rm = TRUE))
}) %>% bind_rows()

f <- ggplot(plot_dat, aes(reorder(measure, d), d, fill = d > 0)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "grey") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(title = "First-generation vs. continuing-generation students",
       subtitle = "Right of the line = higher for first-gen students",
       x = NULL, y = "Difference (standard deviations)",
       caption = "NCHA-IIIb | UTK | Spring 2026") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey", size = 10),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))

print(f)
ggsave("out/fig_firstgen.png", f, width = 8, height = 5, dpi = 300)

#==============SUBSTANCE USE X WELLBEING, BELONGING, LONELINESS (Z-TEST ANALYSIS)=============
#
# ---- 1. Mark who used each substance --------------------------
# 
# Students who never saw the nicotine or binge
# questions were skipped because of an earlier answer, so a blank
# means no. And the cannabis question lists answers from most
# recent to least, so recent users are the middle codes.

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  stopifnot(length(h) == 1)
  h
}

q25 <- col1("^R?N3Q25A$")    # alcohol, how recently
q24 <- col1("^R?N3Q24$")     # cannabis, how recently
q28 <- col1("^R?N3Q28$")     # binge episodes, last 2 weeks

nic <- grep("^R?N3Q23[A-K]$", names(dat), value = TRUE)
stopifnot(length(nic) == 11)

nm <- as.matrix(dat[nic])

dat <- dat %>%
  mutate(
    alcohol  = as.integer(.data[[q25]] %in% 2:4),
    binge    = if_else(is.na(.data[[q28]]), 0L, as.integer(.data[[q28]] >= 2)),
    cannabis = as.integer(.data[[q24]] %in% 2:4),
    nicotine = as.integer(rowSums(nm == 1, na.rm = TRUE) > 0)
  )

# ---- 2. Raw correlations with substance use -------------------
# A first look with no controls. Positive means higher score goes
# with more use.

scores <- c("belonging", "loneliness", "k6", "flourishing")

cat("===== How the four scores correlate =====\n")
print(round(cor(dat[scores], use = "pairwise.complete.obs"), 2))

subs <- c(alcohol  = "Alcohol",
          binge    = "Binge drinking",
          cannabis = "Cannabis",
          nicotine = "Nicotine")

cat("\n===== Raw correlations with substance use =====\n")
raw_cor <- outer(scores, names(subs),
                 Vectorize(function(s, v)
                   cor(dat[[s]], dat[[v]], use = "complete.obs")))
dimnames(raw_cor) <- list(scores, subs)
print(round(raw_cor, 3))

# ---- 3. The models --------------------------------------------
# One model per score, per substance. Sixteen in total.
#
# Each score gets converted to standard deviations first. That way
# every result reads "per one standard deviation" and a 4-24 scale
# can sit next to a 0-24 scale on the same chart. Without it the
# bars aren't comparable and a longer one doesn't mean more.

ctrl <- c("age", "class_yr", "housing", "first_gen")
ctrl <- ctrl[ctrl %in% names(dat)]

cat("\nControlling for:", paste(ctrl, collapse = ", "), "\n")

for (s in scores) dat[[paste0(s, "_z")]] <- as.numeric(scale(dat[[s]]))

res <- lapply(scores, function(s) {
  lapply(names(subs), function(v) {
    
    f   <- reformulate(c(paste0(s, "_z"), ctrl), response = v)
    fit <- glm(f, data = dat, family = binomial)
    
    b  <- coef(fit)[2]
    se <- sqrt(diag(vcov(fit)))[2]
    
    data.frame(score     = s,
               substance = subs[v],
               or        = exp(b),
               low       = exp(b - 1.96 * se),
               high      = exp(b + 1.96 * se),
               p         = summary(fit)$coefficients[2, 4],
               n         = nobs(fit),
               row.names = NULL)
  }) %>% bind_rows()
}) %>% bind_rows()

res$p_adj <- p.adjust(res$p, "BH")

cat("\n===== Odds ratios, per 1 SD of each score =====\n")
cat("Above 1 = more use. Below 1 = less. If low-high crosses 1, no clear link.\n\n")

res %>%
  mutate(across(c(or, low, high), ~ round(., 2)),
         across(c(p, p_adj), ~ round(., 3))) %>%
  print(row.names = FALSE)

write.csv(res, "out/substance_wellbeing.csv", row.names = FALSE)

# ---- 4. The chart ---------------------------------------------
# Dot is the estimate, line is the range it probably sits in. The
# dashed line at 1 is no difference. Solid dots cleared it, hollow
# ones didn't, so the picture answers "which of these should we look at".

lab <- c(belonging   = "Belonging",
         loneliness  = "Loneliness",
         k6          = "Distress",
         flourishing = "Wellbeing")

plot_dat <- res %>%
  mutate(score = lab[score],
         clear = low > 1 | high < 1)

f <- ggplot(plot_dat, aes(or, score)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey") +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0,
                 linewidth = 0.8, color = "grey") +
  geom_point(aes(fill = clear), shape = 21, size = 3.5,
             stroke = 0.9, color = "grey", show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "red3", "FALSE" = "white")) +
  scale_x_log10() +
  facet_wrap(~ substance) +
  labs(title = "Wellbeing and substance use",
       subtitle = paste("Odds per 1 SD higher score.",
                        "Empty circle = no clear link."),
       x = "Odds ratio (log scale)", y = NULL,
       caption = "NCHA-IIIb | UTK | Spring 2026") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey", size = 10),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold"))

print(f)
ggsave("out/fig_substance_wellbeing.png", f, width = 9, height = 6, dpi = 300)

# ---- 6. Reading it ----------------------------------------
# Belonging going UP with drinking is the expected result.
# Drinking is social. The students who feel most
# connected are often the ones at the party. If that's what shows
# up, it's the finding, and it means "build connection" is not by
# itself an alcohol prevention strategy.
#
# Loneliness and distress going up with use is the other story:
# drinking to cope rather than to socialize.
#
# Both can be true at once for different substances. Alcohol is
# usually social, nicotine and cannabis usually aren't.
#
# What I can't say: which came first. This is one survey at one
# moment, so a student who drinks because she's lonely and a
# student who became lonely after her drinking got worse look
# identical in my data.

cat("\n-- Clear links --\n")
plot_dat %>%
  filter(clear) %>%
  arrange(desc(abs(log(or)))) %>%
  with(cat(sprintf("%s / %s: %.2f (%.2f-%.2f)\n",
                   substance, score, or, low, high)))
