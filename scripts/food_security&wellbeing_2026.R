#==============FOOD INSECURITY X SLEEP, STRESS, AND HEALTH=============
#
# Do students with less food security report worse
# sleep, higher stress, and poorer health? And does that hold up
# after accounting for money problems, since the two relate
# and I don't want to report a food finding that is
# really an income finding.
#
# ACHA scored UTK on the standard six-item
# USDA short form and got 36.5% with any food insecurity. My
# instrument has only five items, and one of them (N3Q12C) merges two
# questions into a single 4-level answer. So my score and theirs
# will not match exactly.

library(tidyverse)

dat <- readRDS("2026 data/ncha_clean_2026.rds")

# ---- 1. Look at the items before scoring anything ------------
# I am not going to assume which answer counts as "yes, this was a
# problem." I print every item with its answer counts first, then
# set the rule below to match what is actually there.

fs <- grep("^R?N3Q12[A-E]$", names(dat), value = TRUE)
cat("Food security items found:", length(fs), "\n")
print(fs)

for (v in fs) {
  cat("\n--", v, "--\n")
  a <- attr(dat[[v]], "label")
  if (!is.null(a)) cat(a, "\n")
  print(table(dat[[v]], useNA = "ifany"))
}


# ---- 2. Score it ---------------------------------------------
# Count the number of answers that say food
# was a problem. Higher count = less food security.
#
# The first two items run "often true / sometimes true / never
# true," and both "often" and "sometimes" count as yes. The
# yes/no items count a yes. N3Q12C is the merged one, so anything
# other than the lowest answer counts.

affirm <- function(x, yes_codes) as.integer(x %in% yes_codes)

sc <- sapply(fs, function(v) {
  x <- dat[[v]]
  k <- max(x, na.rm = TRUE)
  
  if (k <= 2) {
    affirm(x, 1)            
  } else if (k == 3) {
    affirm(x, 1:2)          
  } else {
    affirm(x, 2:k)          
  }
})

answered <- rowSums(!is.na(dat[fs]))

dat$fs_score <- rowSums(sc, na.rm = TRUE)
dat$fs_score[answered < length(fs) - 1] <- NA   # need 4 of 5

dat$fs_cat <- factor(
  case_when(dat$fs_score <= 1 ~ "High/marginal",
            dat$fs_score <= 3 ~ "Low",
            dat$fs_score >= 4 ~ "Very low"),
  levels = c("High/marginal", "Low", "Very low"))

dat$fs_any <- as.integer(dat$fs_score >= 2)

cat("\n===== Food security score (0 to", length(fs), ") =====\n")
print(table(dat$fs_score, useNA = "ifany"))

cat("\n")
print(table(dat$fs_cat, useNA = "ifany"))

cat(sprintf("\nAny food insecurity: %.1f%%   (ACHA reported 36.5%% on the 6-item form)\n",
            100 * mean(dat$fs_any, na.rm = TRUE)))
cat("Close is fine. Mine is a 5-item version so it will not match exactly.\n")


# ---- 3. The outcomes and the money control -------------------
# Three outcomes, each turned into a yes/no so the table, the
# models, and the figure all use the same definitions.
#
# The money control is the whole point of this analysis. Students
# who are short on food are usually short on money, so without it
# I would just be measuring poverty and calling it nutrition.

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  if (length(h) != 1) return(NA_character_)
  h
}

q1   <- col1("^R?N3Q1$")       # self-rated health, 1 excellent to 5 poor
q48  <- col1("^R?N3Q48$")      # stress, 1 none to 4 high
q14  <- col1("^R?N3Q14$")      # weeknight sleep hours
fin  <- col1("^R?N3Q47A3$")    # finances were a problem, yes/no

cat("\nColumns:", q1, q48, q14, fin, "\n")

dat$poor_health <- if (!is.na(q1))  as.integer(dat[[q1]]  >= 4)  else NA_integer_
dat$high_stress <- if (!is.na(q48)) as.integer(dat[[q48]] == 4)  else NA_integer_
dat$short_sleep <- if (!is.na(q14)) as.integer(dat[[q14]] <= 2)  else NA_integer_

ctrl <- c("age", "class_yr", "housing", "first_gen")
ctrl <- ctrl[ctrl %in% names(dat)]


# ---- 4. Is there a gradient? --------------------------------
# Percent for each food security group. If the numbers
# go in one direction across the three groups, that is a
# gradient, and it is much stronger evidence than a single
# yes/no comparison.

tab <- dat %>%
  filter(!is.na(fs_cat)) %>%
  summarise(n           = n(),
            poor_health = 100 * mean(poor_health, na.rm = TRUE),
            high_stress = 100 * mean(high_stress, na.rm = TRUE),
            short_sleep = 100 * mean(short_sleep, na.rm = TRUE),
            .by = fs_cat) %>%
  arrange(fs_cat)

cat("\n===== Outcomes by food security (% of students) =====\n\n")
print(as.data.frame(tab %>% mutate(across(where(is.numeric), ~ round(., 1)))),
      row.names = FALSE)

write.csv(tab, "2026 out/foodsec_gradient.csv", row.names = FALSE)


# ---- 5. Does it survive the money control? -------------------
# Section 4 asks "are they different." This asks "are they
# different for a reason other than being low on money."
#
# Two models per outcome. The first adjusts for the usual
# background variables. The second adds the finances item. If the
# food effect shrinks toward nothing when finances goes in, then
# the honest finding is that this is about money.
#
# All three outcomes are yes/no, so all three use logistic
# regression. Results are odds ratios: 1 means no difference,
# above 1 means food insecure students have higher odds.

outs <- c(poor_health = "Poor health",
          high_stress = "High stress",
          short_sleep = "Short sleep (<7 hrs)")

fit_one <- function(y, with_fin) {
  
  preds <- c("fs_any", ctrl)
  if (with_fin && !is.na(fin)) preds <- c(preds, fin)
  
  f <- reformulate(preds, response = y)
  d <- dat[complete.cases(dat[all.vars(f)]), ]
  d <- droplevels(d)
  
  fit <- glm(f, data = d, family = binomial)
  
  b  <- coef(fit)["fs_any"]
  se <- sqrt(diag(vcov(fit)))["fs_any"]
  p  <- summary(fit)$coefficients["fs_any", 4]
  
  data.frame(est  = exp(b),
             low  = exp(b - 1.96 * se),
             high = exp(b + 1.96 * se),
             p    = p,
             n    = nobs(fit),
             row.names = NULL)
}

res <- lapply(names(outs), function(y) {
  a <- fit_one(y, FALSE)
  b <- fit_one(y, TRUE)
  
  data.frame(outcome  = outs[[y]],
             est_base = a$est, p_base = a$p,
             est_fin  = b$est, low_fin = b$low, high_fin = b$high,
             p_fin    = b$p,
             n        = b$n,
             row.names = NULL)
}) %>% bind_rows()

# Correction across the three outcomes, which are the family of
# hypotheses in the research question.
res$p_adj <- p.adjust(res$p_fin, "BH")

cat("\n===== Food insecure vs. food secure (odds ratios) =====\n")
cat("est_base = before the finances control\n")
cat("est_fin  = after it. This is the one to report.\n")
cat("1 means no difference. Above 1 means higher odds for food insecure students.\n\n")

print(as.data.frame(res %>%
                      mutate(across(c(est_base, est_fin, low_fin, high_fin), ~ round(., 2)),
                             across(c(p_base, p_fin, p_adj), ~ round(., 3)))),
      row.names = FALSE)

write.csv(res, "2026 out/foodsec_adjusted.csv", row.names = FALSE)


# ---- 6. Check: ranked versions of health and stress ---------
# Sections 4 and 5 collapse self-rated health and stress into
# yes/no. This re-runs them using every answer category, as a
# check that the yes/no cutoff is not driving the result. Result
# reads "times the odds of being in a worse category."

if (requireNamespace("MASS", quietly = TRUE)) {
  
  ord <- lapply(c(q1, q48), function(y) {
    if (is.na(y)) return(NULL)
    
    preds <- c("fs_any", ctrl)
    if (!is.na(fin)) preds <- c(preds, fin)
    
    d <- dat
    d$yy <- factor(d[[y]], ordered = TRUE)
    f <- reformulate(preds, response = "yy")
    d <- droplevels(d[complete.cases(d[all.vars(f)]), ])
    
    fit <- MASS::polr(f, data = d, Hess = TRUE)
    s   <- summary(fit)$coefficients["fs_any", ]
    
    data.frame(outcome = y,
               or   = exp(s[1]),
               low  = exp(s[1] - 1.96 * s[2]),
               high = exp(s[1] + 1.96 * s[2]),
               p    = 2 * pnorm(-abs(s[3])),
               n    = nobs(fit),
               row.names = NULL)
  }) %>% bind_rows()
  
  cat("\n===== Ranked outcomes, adjusted =====\n")
  cat("or = times the odds of a worse answer\n\n")
  print(as.data.frame(ord %>%
                        mutate(across(c(or, low, high), ~ round(., 2)),
                               p = round(p, 3))), row.names = FALSE)
}


# ---- 7. The chart -------------------------------------------
# One small panel per outcome, three bars each, with the exact
# percentage printed on every bar. Uses the same numbers as the
# gradient table in Section 4.

cols <- c("High/marginal" = "#8C8C8C",
          "Low"           = "#D9A441",
          "Very low"      = "#B3352E")

plot_dat <- tab %>%
  pivot_longer(c(poor_health, high_stress, short_sleep),
               names_to = "measure", values_to = "pct") %>%
  mutate(measure = factor(measure,
                          levels = c("poor_health", "high_stress", "short_sleep"),
                          labels = c("Poor health", "High stress", "Short sleep (<7 hrs)")))

f <- ggplot(plot_dat, aes(fs_cat, pct, fill = fs_cat)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.4, size = 4) +
  facet_wrap(~ measure) +
  scale_fill_manual(values = cols, name = "Food security") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Health, stress, and sleep by food security",
       x = NULL, y = "% of students",
       caption = "NCHA-III | UTK | Spring 2026") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

print(f)
ggsave("2026 out/fig_foodsec.png", f, width = 9, height = 5.5, dpi = 300)


# ---- 8. Reading it ------------------------------------------
# Report the est_fin column, not est_base. A difference that
# disappears once finances goes in is a finding. It
# means the problem is income.
#
# If it survives, that is the stronger result: food access is
# doing something on its own, beyond just being short on money.
#
# The gradient in Section 4 matters as much as the p-values. Three
# groups lining up in order is harder to explain away than one
# yes/no gap.

cat("\n-- Clear differences after the finances control --\n")
res %>%
  filter(p_adj < 0.05) %>%
  with(cat(sprintf("%s: OR %.2f (%.2f to %.2f)\n",
                   outcome, est_fin, low_fin, high_fin), sep = ""))

cat(sprintf("\nFood insecure n = %d of %d\n",
            sum(dat$fs_any == 1, na.rm = TRUE),
            sum(!is.na(dat$fs_any))))
cat("Use p_adj, not p.\n")