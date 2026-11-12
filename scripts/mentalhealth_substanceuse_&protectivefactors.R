# ============================================================
# PART A: mental health x substance use
# PART B: protective factors
# Two separate figures.
# ============================================================

library(tidyverse)

dat <- readRDS("2026 data/ncha_clean_2026.rds")

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  stopifnot(length(h) == 1)
  h
}

q25 <- col1("^R?N3Q25A$")
q24 <- col1("^R?N3Q24$")
q28 <- col1("^R?N3Q28$")
q43 <- col1("^R?N3Q43$")
q49 <- col1("^R?N3Q49$")
q50 <- col1("^R?N3Q50$")
q51 <- col1("^R?N3Q51$")
q52 <- col1("^R?N3Q52$")

nic <- grep("^R?N3Q23[A-K]$", names(dat), value = TRUE)
slp <- grep("^R?N3Q16[A-D]$", names(dat), value = TRUE)
fs  <- grep("^R?N3Q12[A-E]$", names(dat), value = TRUE)


# ---- 1. Build the variables --------------------------------

# Food insecurity. Every item is coded from 0 (never / no).
# Items A, B, D, E: any answer above 0 counts as a problem.
# Item C (skipped meals): "Only 1 or 2 days" (1) does not count;
# "some days" (2) or "almost every day" (3) does.
# Need at least 4 of the 5 items answered.
fs_hit <- sapply(fs, function(v) {
  x <- dat[[v]]
  if (grepl("12C$", v)) as.integer(x >= 2) else as.integer(x >= 1)
})
fs_answered <- rowSums(!is.na(as.matrix(dat[fs])))

dat <- dat %>%
  mutate(
    # substance use
    alcohol  = as.integer(.data[[q25]] %in% 2:4),
    binge    = if_else(is.na(.data[[q28]]), 0L, as.integer(.data[[q28]] >= 2)),
    cannabis = as.integer(.data[[q24]] %in% 2:4),
    nicotine = as.integer(rowSums(as.matrix(dat[nic]) == 1, na.rm = TRUE) > 0),
    
    # suicide screen (SBQ-R, 7 or higher is positive)
    i1 = case_when(.data[[q49]] == 1 ~ 1, .data[[q49]] == 2 ~ 2,
                   .data[[q49]] %in% 3:4 ~ 3, .data[[q49]] %in% 5:6 ~ 4),
    i3 = case_when(.data[[q51]] == 1 ~ 1, .data[[q51]] %in% 2:3 ~ 2,
                   .data[[q51]] %in% 4:5 ~ 3),
    sbqr     = i1 + .data[[q50]] + i3 + .data[[q52]],
    sbqr_pos = as.integer(sbqr >= 7),
    
    # food security score and group
    fs_score = if_else(fs_answered >= 4,
                       rowSums(fs_hit, na.rm = TRUE), NA_real_),
    
    level    = factor(if_else(grepl("Master|Doctor", class_yr), "Grad", "Undergrad"),
                      levels = c("Undergrad", "Grad")),
    oncampus = factor(if_else(grepl("ampus", housing), "On campus", "Off campus"))
  )

# Controls. Disability added because it was the strongest group
# predictor in earlier analysis.
ctrl <- c("age", "level", "first_gen", "oncampus", "disability")
ctrl <- ctrl[ctrl %in% names(dat)]
cat("Controls used:", paste(ctrl, collapse = ", "), "\n")

# Scores in standard deviations. Wellbeing is flipped so that for
# every measure, higher = worse.
dat <- dat %>%
  mutate(k6_z         = as.numeric(scale(k6)),
         loneliness_z = as.numeric(scale(loneliness)),
         lowwell_z    = -as.numeric(scale(flourishing)))

# Serious distress. The data already has k6_serious; this checks
# that it matches the usual cutoff (K6 of 13 or higher).
if (!"k6_serious" %in% names(dat)) {
  dat$k6_serious <- as.integer(dat$k6 >= 13)
}
cat("\nk6_serious vs. K6 >= 13 (should match):\n")
print(table(k6_serious = dat$k6_serious, k6_13plus = dat$k6 >= 13,
            useNA = "ifany"))

fit_or <- function(x, y) {
  f <- glm(reformulate(c(x, ctrl), y), dat, family = binomial)
  s <- summary(f)$coefficients[2, ]
  data.frame(predictor = x, outcome = y,
             or = exp(s[1]), low = exp(s[1] - 1.96 * s[2]),
             high = exp(s[1] + 1.96 * s[2]), p = s[4], n = nobs(f),
             row.names = NULL)
}


# ============================================================
# PART A: mental health and substance use
# ============================================================

mh   <- c(k6_z = "Distress", loneliness_z = "Loneliness",
          lowwell_z = "Lower wellbeing", sbqr_pos = "Suicide risk screen")
subs <- c(alcohol = "Alcohol", binge = "Binge drinking",
          cannabis = "Cannabis", nicotine = "Nicotine")

# A1. Each mental health measure on its own ------------------
A <- expand_grid(x = names(mh), y = names(subs)) %>%
  pmap_dfr(function(x, y) fit_or(x, y)) %>%
  mutate(predictor = unname(mh[predictor]),
         outcome   = unname(subs[outcome]),
         p_adj     = p.adjust(p, "BH"))

cat("\n===== A1. One measure at a time (odds ratios) =====\n")
cat("Scores are per 1 SD worse. Suicide screen is yes vs. no.\n")
cat("Use p_adj, not p.\n\n")
print(A %>% mutate(across(c(or, low, high), ~ round(., 2)),
                   across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

write.csv(A, "2026 out/mh_substance_v2.csv", row.names = FALSE)

# A2. All four measures in the same model --------------------
# The four measures overlap, so this shows which ones still matter
# when the others are held equal.
fit_joint <- function(y) {
  pr <- names(mh)
  f  <- glm(reformulate(c(pr, ctrl), y), dat, family = binomial)
  s  <- summary(f)$coefficients[pr, , drop = FALSE]
  data.frame(predictor = unname(mh[pr]), outcome = unname(subs[y]),
             or   = exp(s[, 1]),
             low  = exp(s[, 1] - 1.96 * s[, 2]),
             high = exp(s[, 1] + 1.96 * s[, 2]),
             p    = s[, 4], n = nobs(f), row.names = NULL)
}

AJ <- map_dfr(names(subs), fit_joint) %>%
  mutate(p_adj = p.adjust(p, "BH"))

cat("\n===== A2. All four measures together (odds ratios) =====\n")
print(AJ %>% mutate(across(c(or, low, high), ~ round(., 2)),
                    across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

write.csv(AJ, "2026 out/mh_substance_joint.csv", row.names = FALSE)

# Figure A. Filled = still clear after adjusting for 16 tests.
figA <- A %>%
  mutate(clear     = p_adj < 0.05,
         predictor = factor(predictor, levels = rev(unname(mh))),
         outcome   = factor(outcome, levels = unname(subs))) %>%
  ggplot(aes(or, predictor)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                width = 0, linewidth = 1.1, color = "grey55") +
  geom_point(aes(fill = clear), shape = 21, size = 4.5,
             stroke = 1, color = "grey20", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", or)), vjust = -1.3,
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#B3352E", "FALSE" = "white")) +
  scale_x_log10() +
  facet_wrap(~ outcome) +
  labs(title = "Mental health and substance use",
       subtitle = paste0("Odds of use per 1 SD worse mental health ",
                         "(suicide screen: positive vs. negative).\n",
                         "Filled = clear link after adjusting for 16 tests. ",
                         "Hollow = not clear."),
       x = "Odds ratio (log scale)", y = NULL,
       caption = "NCHA-IIIb | University of Tennessee | Spring 2026") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30", size = 10,
                                     lineheight = 1.1),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))

print(figA)
ggsave("2026 out/fig_mh_substance_v2.png", figA, width = 10, height = 6.5,
       dpi = 300)


# ============================================================
# PART B: protective factors
# ============================================================

# The old "would seek help" factor was 100% of students, so it
# added 1 point to everyone and carried no information. It is
# dropped. This prints the answers so the coding can be checked.
cat("\nHelp-seeking item (", q43, ") answers, for checking:\n", sep = "")
print(table(dat[[q43]], useNA = "ifany"))

hi <- function(x) as.integer(x > median(x, na.rm = TRUE))

dat <- dat %>%
  mutate(
    p_belong  = hi(belonging),
    p_resil   = hi(cdrisc2),
    p_safe    = hi(safety),
    p_rested  = as.integer((.data[[slp[4]]] - 1) >= 4),
    p_foodsec = as.integer(fs_score < 2),
    prot_n    = p_belong + p_resil + p_safe + p_rested + p_foodsec
  )

prot <- c(p_belong = "High belonging", p_resil = "High resilience",
          p_safe = "Feels safe", p_rested = "Rested 4+ days",
          p_foodsec = "Food secure")

cat("\n% of students with each protective factor:\n")
print(round(colMeans(dat[names(prot)], na.rm = TRUE) * 100, 1))   # none should be 0 or 100
cat("\nMissing values per factor:\n")
print(colSums(is.na(dat[names(prot)])))
cat("\nNumber of protective factors (0 to 5):\n")
print(table(dat$prot_n, useNA = "ifany"))

outs <- c(k6_serious = "Serious distress", sbqr_pos = "Suicide risk screen",
          alcohol = "Alcohol", binge = "Binge drinking",
          cannabis = "Cannabis", nicotine = "Nicotine")

# B1. Number of protective factors -> each outcome -----------
B <- map_dfr(names(outs), function(y) {
  r <- fit_or("prot_n", y); r$outcome <- unname(outs[y]); r
}) %>% mutate(p_adj = p.adjust(p, "BH"))

cat("\n===== B1. Per extra protective factor (odds ratios) =====\n")
print(B %>% mutate(across(c(or, low, high), ~ round(., 2)),
                   across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

write.csv(B, "2026 out/protective_outcomes_v2.csv", row.names = FALSE)

# B2. Each factor on its own ---------------------------------
# Distress score (points) and suicide screen (odds ratio).
pf <- names(prot)

each_k6 <- map_dfr(pf, function(x) {
  f <- lm(reformulate(c(x, ctrl), "k6"), dat)
  s <- summary(f)$coefficients[2, ]
  data.frame(factor = unname(prot[x]), outcome = "Distress score (points)",
             est = s[1], low = s[1] - 1.96 * s[2],
             high = s[1] + 1.96 * s[2], p = s[4], row.names = NULL)
})

each_sb <- map_dfr(pf, function(x) {
  f <- glm(reformulate(c(x, ctrl), "sbqr_pos"), dat, family = binomial)
  s <- summary(f)$coefficients[2, ]
  data.frame(factor = unname(prot[x]), outcome = "Suicide screen (OR)",
             est = exp(s[1]), low = exp(s[1] - 1.96 * s[2]),
             high = exp(s[1] + 1.96 * s[2]), p = s[4], row.names = NULL)
})

each <- bind_rows(each_k6, each_sb) %>%
  group_by(outcome) %>%
  mutate(p_adj = p.adjust(p, "BH")) %>%
  ungroup()

cat("\n===== B2. Each factor on its own =====\n")
print(each %>% mutate(across(c(est, low, high), ~ round(., 2)),
                      across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

# B3. All factors in the same model --------------------------
# The factors overlap (belonging, resilience, and safety especially),
# so this shows which still matter when the others are held equal.
grab_joint <- function(fit, outcome, logit) {
  s  <- summary(fit)$coefficients[pf, , drop = FALSE]
  tr <- if (logit) exp else identity
  data.frame(factor = unname(prot[pf]), outcome = outcome,
             est  = tr(s[, 1]),
             low  = tr(s[, 1] - 1.96 * s[, 2]),
             high = tr(s[, 1] + 1.96 * s[, 2]),
             p    = s[, 4], n = nobs(fit), row.names = NULL)
}

joint <- bind_rows(
  grab_joint(lm(reformulate(c(pf, ctrl), "k6"), dat),
             "Distress score (points)", FALSE),
  grab_joint(glm(reformulate(c(pf, ctrl), "sbqr_pos"), dat, family = binomial),
             "Suicide screen (OR)", TRUE)
) %>%
  group_by(outcome) %>%
  mutate(p_adj = p.adjust(p, "BH")) %>%
  ungroup()

cat("\n===== B3. All protective factors together =====\n")
print(joint %>% mutate(across(c(est, low, high), ~ round(., 2)),
                       across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

write.csv(each,  "2026 out/protective_each_v2.csv",  row.names = FALSE)
write.csv(joint, "2026 out/protective_joint_v2.csv", row.names = FALSE)

# B4. Rates by number of protective factors ------------------
grad <- dat %>%
  filter(!is.na(prot_n)) %>%
  mutate(pn = if_else(prot_n <= 1, "0-1", as.character(prot_n))) %>%
  group_by(pn) %>%
  summarise(n = n(),
            across(all_of(names(outs)), ~ 100 * mean(., na.rm = TRUE))) %>%
  filter(n >= 25) %>%
  mutate(pn = factor(pn, levels = c("0-1", as.character(2:5)))) %>%
  arrange(pn) %>%
  mutate(pn_lab = factor(paste0(pn, "\n(n = ", n, ")"),
                         levels = paste0(pn, "\n(n = ", n, ")"))) %>%
  pivot_longer(all_of(names(outs)), names_to = "measure", values_to = "pct") %>%
  mutate(measure = outs[measure])

cat("\n===== B4. Percent by number of protective factors =====\n")
print(grad %>% select(pn, n, measure, pct) %>%
        mutate(pct = round(pct, 1)), n = Inf)

# Figure B: only the two outcomes with a real trend. The substance
# use outcomes showed no link and stay in the tables above.
figB <- grad %>%
  filter(measure %in% c("Serious distress", "Suicide risk screen")) %>%
  ggplot(aes(pn_lab, pct, group = measure)) +
  geom_line(color = "#B3352E", linewidth = 1) +
  geom_point(color = "#B3352E", size = 3) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), vjust = -1, size = 3.8) +
  facet_wrap(~ measure) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "More protective factors, lower distress and suicide risk",
       subtitle = paste0("Protective factors counted: belonging, resilience, ",
                         "safety, rested, food secure"),
       x = "Number of protective factors", y = "% of students",
       caption = "NCHA-IIIb | University of Tennessee | Spring 2026") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30", size = 10),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))

print(figB)
ggsave("2026 out/fig_protective_v2.png", figB, width = 10, height = 5.5,
       dpi = 300)