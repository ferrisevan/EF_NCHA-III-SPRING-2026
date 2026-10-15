# ============================================================
# Violence x Belonging x Substance Use
#
# Two questions:
#   1. Do students who experienced violence feel less like they
#      belong on campus?
#   2. Are they more likely to be using substances?
# ============================================================

library(tidyverse)

dat <- readRDS("2026 data/ncha_clean_2026.rds")

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  stopifnot(length(h) == 1)
  h
}


# ---- 1. Build the violence groups --------------------------
# Individual items are too rare to model on their own
# (sexual penetration without consent is 0.7%, about 5 students).
# So they get grouped into three categories decided in advance.
# A student counts as exposed if they said yes to any item in
# the group.

ipv  <- grep("^R?N3Q19[A-E]$", names(dat), value = TRUE)
npv  <- grep("^R?N3Q20[A-G]$", names(dat), value = TRUE)
sexv <- grep("^R?N3Q(19[DE]|20[DEF])$", names(dat), value = TRUE)

anyof <- function(cols) {
  m <- as.matrix(dat[cols])
  as.integer(rowSums(m == 1, na.rm = TRUE) > 0)
}

dat <- dat %>%
  mutate(v_partner = anyof(ipv),
         v_other   = anyof(npv),
         v_sexual  = anyof(sexv))

cat("How many students in each group (%):\n")
print(round(colMeans(dat[c("v_partner", "v_other", "v_sexual")],
                     na.rm = TRUE) * 100, 1))


# ---- 2. Mark who used each substance -----------------------
# Same rules used in the earlier substance scripts. A blank on
# the binge or nicotine questions means the survey skipped them
# because they already said they don't drink or vape, so that
# counts as a no.

q25 <- col1("^R?N3Q25A$")
q24 <- col1("^R?N3Q24$")
q28 <- col1("^R?N3Q28$")
nic <- grep("^R?N3Q23[A-K]$", names(dat), value = TRUE)
nm  <- as.matrix(dat[nic])

dat <- dat %>%
  mutate(alcohol  = as.integer(.data[[q25]] %in% 2:4),
         binge    = if_else(is.na(.data[[q28]]), 0L,
                            as.integer(.data[[q28]] >= 2)),
         cannabis = as.integer(.data[[q24]] %in% 2:4),
         nicotine = as.integer(rowSums(nm == 1, na.rm = TRUE) > 0))


# ---- 3. Controls -------------------------------------------
# Class year and housing get squeezed down to two groups each.
# Some of the original categories only have a few students in
# them, which makes the model unstable.

dat <- dat %>%
  mutate(level    = factor(if_else(grepl("Master|Doctor", class_yr),
                                   "Grad", "Undergrad"),
                           levels = c("Undergrad", "Grad")),
         oncampus = factor(if_else(grepl("Campus|campus hous", housing),
                                   "On campus", "Off campus")),
         belonging_z = as.numeric(scale(belonging)))

ctrl <- c("age", "level", "first_gen", "oncampus", "greek_any")
ctrl <- ctrl[ctrl %in% names(dat)]

cat("\nControlling for:", paste(ctrl, collapse = ", "), "\n")


# ---- 4. Violence and belonging -----------------------------
# Belonging is a score, not a yes/no, so this is ordinary
# regression. The number is how many standard deviations lower
# (or higher) the exposed group sits.

vs <- c(v_partner = "Partner violence",
        v_other   = "Non-partner violence",
        v_sexual  = "Sexual violence")

bel <- lapply(names(vs), function(v) {
  fit <- lm(reformulate(c(v, ctrl), "belonging_z"), dat)
  s <- summary(fit)$coefficients[2, ]
  data.frame(exposure = vs[v], est = s[1],
             low = s[1] - 1.96 * s[2], high = s[1] + 1.96 * s[2],
             p = s[4], n = nobs(fit), row.names = NULL)
}) %>% bind_rows()

cat("\nBelonging (SD difference vs. not exposed):\n")
print(bel %>% mutate(across(c(est, low, high), ~ round(., 2)),
                     p = round(p, 4)), row.names = FALSE)


# ---- 5. Violence and substance use -------------------------
# Substance use is yes/no, so this is logistic regression and
# the number is an odds ratio. Above 1 means more use.

sub <- c(alcohol = "Alcohol", binge = "Binge drinking",
         cannabis = "Cannabis", nicotine = "Nicotine")

res <- lapply(names(vs), function(v) {
  lapply(names(sub), function(y) {
    fit <- glm(reformulate(c(v, ctrl), y), dat, family = binomial)
    s <- summary(fit)$coefficients[2, ]
    data.frame(exposure = vs[v], outcome = sub[y],
               or = exp(s[1]),
               low = exp(s[1] - 1.96 * s[2]),
               high = exp(s[1] + 1.96 * s[2]),
               p = s[4], n = nobs(fit), row.names = NULL)
  }) %>% bind_rows()
}) %>% bind_rows()

# 12 tests, so p-values get adjusted. Use p_adj, not p.
res$p_adj <- p.adjust(res$p, "BH")

cat("\nSubstance use (odds ratios):\n")
print(res %>% mutate(across(c(or, low, high), ~ round(., 2)),
                     across(c(p, p_adj), ~ round(., 3))),
      row.names = FALSE)

write.csv(res, "2026 out/rq12_violence.csv", row.names = FALSE)


# ---- 6. Figure: substance use ------------------------------
# Dot is the estimate, line is the range it probably sits in.
# Filled means it cleared the no-difference line, hollow means
# we can't tell.

pd <- res %>%
  mutate(clear = low > 1 | high < 1,
         exposure = factor(exposure, levels = rev(vs)))

p <- ggplot(pd, aes(or, exposure)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey45", linewidth = 0.6) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                width = 0, linewidth = 1.1, color = "grey55") +
  geom_point(aes(fill = clear), shape = 21, size = 4.5,
             stroke = 1, color = "grey20", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", or)),
            vjust = -1.3, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#B3352E", "FALSE" = "white")) +
  scale_x_log10(breaks = c(0.5, 1, 2, 4)) +
  facet_wrap(~ outcome) +
  labs(title = "Violence exposure and substance use",
       subtitle = paste0("Odds of use among students who experienced ",
                         "violence vs. those who did not.\n",
                         "Filled = clear difference. Hollow = not ",
                         "distinguishable."),
       x = "Odds ratio (log scale)", y = NULL,
       caption = paste0("NCHA-IIIb | University of Tennessee | ",
                        "Spring 2026 | N = ", max(pd$n))) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30", size = 10,
                                     lineheight = 1.1),
        plot.caption = element_text(color = "grey45", size = 9),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"))

print(p)
ggsave("2026 out/fig_rq12_violence.png", p,
       width = 10, height = 6, dpi = 300)


# ---- 7. Figure: belonging ----------------------------------
# Filled = range stays below 0. Hollow = range touches or crosses 0.

bel <- bel %>% mutate(clear = low > 0 | high < 0)

p2 <- ggplot(bel, aes(est, fct_rev(factor(exposure, levels = vs)))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                width = 0, linewidth = 1.1, color = "grey55") +
  geom_point(aes(fill = clear), shape = 21, size = 4.5,
             stroke = 1, color = "grey20", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", est)),
            vjust = -1.3, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#B3352E", "FALSE" = "white")) +
  labs(title = "Violence exposure and campus belonging",
       subtitle = paste0("Below 0 means lower belonging than students not exposed.\n",
                         "Filled = clear difference. Hollow = not distinguishable."),
       x = "Difference in belonging (standard deviations)", y = NULL,
       caption = "NCHA-IIIb | University of Tennessee | Spring 2026") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(color = "grey30", size = 10,
                                     lineheight = 1.1),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))

print(p2)
ggsave("2026 out/fig_rq12_belonging.png", p2,
       width = 9, height = 4.5, dpi = 300)