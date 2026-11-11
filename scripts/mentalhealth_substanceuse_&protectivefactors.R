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


# ---- 1. Substance use + suicide screen ---------------------

dat <- dat %>%
  mutate(
    alcohol  = as.integer(.data[[q25]] %in% 2:4),
    binge    = if_else(is.na(.data[[q28]]), 0L, as.integer(.data[[q28]] >= 2)),
    cannabis = as.integer(.data[[q24]] %in% 2:4),
    nicotine = as.integer(rowSums(as.matrix(dat[nic]) == 1, na.rm = TRUE) > 0),
    
    i1 = case_when(.data[[q49]] == 1 ~ 1, .data[[q49]] == 2 ~ 2,
                   .data[[q49]] %in% 3:4 ~ 3, .data[[q49]] %in% 5:6 ~ 4),
    i3 = case_when(.data[[q51]] == 1 ~ 1, .data[[q51]] %in% 2:3 ~ 2,
                   .data[[q51]] %in% 4:5 ~ 3),
    sbqr     = i1 + .data[[q50]] + i3 + .data[[q52]],
    sbqr_pos = as.integer(sbqr >= 7),
    
    level    = factor(if_else(grepl("Master|Doctor", class_yr), "Grad", "Undergrad"),
                      levels = c("Undergrad", "Grad")),
    oncampus = factor(if_else(grepl("ampus", housing), "On campus", "Off campus"))
  )

ctrl <- c("age", "level", "first_gen", "oncampus")
ctrl <- ctrl[ctrl %in% names(dat)]

for (v in c("k6", "loneliness", "flourishing"))
  dat[[paste0(v, "_z")]] <- as.numeric(scale(dat[[v]]))

fit_or <- function(x, y) {
  f <- glm(reformulate(c(x, ctrl), y), dat, family = binomial)
  s <- summary(f)$coefficients[2, ]
  data.frame(predictor = x, outcome = y,
             or = exp(s[1]), low = exp(s[1] - 1.96 * s[2]),
             high = exp(s[1] + 1.96 * s[2]), p = s[4], n = nobs(f))
}


# ============================================================
# PART A
# ============================================================

mh   <- c(k6_z = "Distress", loneliness_z = "Loneliness",
          flourishing_z = "Wellbeing", sbqr_pos = "Suicide risk screen")
subs <- c(alcohol = "Alcohol", binge = "Binge drinking",
          cannabis = "Cannabis", nicotine = "Nicotine")

A <- expand_grid(x = names(mh), y = names(subs)) %>%
  pmap_dfr(function(x, y) fit_or(x, y)) %>%
  mutate(predictor = mh[predictor], outcome = subs[outcome],
         p_adj = p.adjust(p, "BH"))

print(A %>% mutate(across(c(or, low, high), ~ round(., 2)),
                   across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

write.csv(A, "2026 out/mh_substance.csv", row.names = FALSE)

figA <- A %>%
  mutate(clear = low > 1 | high < 1) %>%
  ggplot(aes(or, predictor)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0,
                 linewidth = 1.1, color = "grey55") +
  geom_point(aes(fill = clear), shape = 21, size = 4.5,
             stroke = 1, color = "grey20", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", or)), vjust = -1.3,
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#B3352E", "FALSE" = "white")) +
  scale_x_log10() +
  facet_wrap(~ outcome) +
  labs(title = "Mental health and substance use",
       subtitle = "Odds of use per 1 SD worse mental health. Filled = clear link.",
       x = "Odds ratio (log scale)", y = NULL,
       caption = "NCHA-IIIb | University of Tennessee | Spring 2026") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30", size = 10),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))

print(figA)
ggsave("2026 out/fig_mh_substance.png", figA, width = 10, height = 6, dpi = 300)


# ============================================================
# PART B
# ============================================================

fsm <- as.matrix(dat[fs])
hi  <- function(x) as.integer(x > median(x, na.rm = TRUE))

dat <- dat %>%
  mutate(
    p_belong   = hi(belonging),
    p_resil    = hi(cdrisc2),
    p_safe     = hi(safety),
    p_rested   = as.integer((.data[[slp[4]]] - 1) >= 4),
    p_foodsec  = as.integer(rowSums(fsm > 1, na.rm = TRUE) < 2),
    p_helpseek = as.integer(.data[[q43]] <= 2),
    prot_n     = p_belong + p_resil + p_safe + p_rested + p_foodsec + p_helpseek
  )

prot <- c(p_belong = "High belonging", p_resil = "High resilience",
          p_safe = "Feels safe", p_rested = "Rested 4+ days",
          p_foodsec = "Food secure", p_helpseek = "Would seek help")

print(round(colMeans(dat[names(prot)], na.rm = TRUE) * 100, 1))
print(table(dat$prot_n, useNA = "ifany"))

# protective count -> each outcome
outs <- c(k6_serious = "Serious distress", sbqr_pos = "Suicide screen +",
          alcohol = "Alcohol", binge = "Binge drinking",
          cannabis = "Cannabis", nicotine = "Nicotine")

B <- map_dfr(names(outs), function(y) {
  r <- fit_or("prot_n", y); r$outcome <- outs[y]; r
}) %>% mutate(p_adj = p.adjust(p, "BH"))

print(B %>% mutate(across(c(or, low, high), ~ round(., 2)),
                   across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

# each factor separately, on distress
D <- map_dfr(names(prot), function(x) {
  f <- lm(reformulate(c(x, ctrl), "k6"), dat)
  s <- summary(f)$coefficients[2, ]
  data.frame(factor = prot[x], b = s[1],
             low = s[1] - 1.96 * s[2], high = s[1] + 1.96 * s[2], p = s[4])
}) %>% mutate(p_adj = p.adjust(p, "BH"))

print(D %>% mutate(across(c(b, low, high), ~ round(., 2)),
                   across(c(p, p_adj), ~ round(., 3))), row.names = FALSE)

write.csv(B, "2026 out/protective_outcomes.csv", row.names = FALSE)
write.csv(D, "2026 out/protective_each.csv", row.names = FALSE)

grad <- dat %>%
  filter(!is.na(prot_n)) %>%
  mutate(pn = if_else(prot_n <= 1, "0-1", as.character(prot_n))) %>%
  group_by(pn) %>%
  summarise(n = n(), across(all_of(names(outs)),
                            ~ 100 * mean(., na.rm = TRUE))) %>%
  filter(n >= 25) %>%
  pivot_longer(-c(pn, n), names_to = "measure", values_to = "pct") %>%
  mutate(measure = outs[measure])

print(grad, n = Inf)

figB <- ggplot(grad, aes(pn, pct, group = measure)) +
  geom_line(color = "#B3352E", linewidth = 1) +
  geom_point(color = "#B3352E", size = 3) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), vjust = -1, size = 3.4) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(title = "More protective factors, better outcomes",
       subtitle = "Belonging, resilience, safety, rested, food secure, would seek help",
       x = "Number of protective factors", y = "% of students",
       caption = "NCHA-III | University of Tennessee | 2026") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30", size = 10),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))

print(figB)
ggsave("2026 out/fig_protective.png", figB, width = 10, height = 6, dpi = 300)
