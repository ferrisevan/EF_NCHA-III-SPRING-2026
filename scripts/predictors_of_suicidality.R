library(tidyverse)

dat <- readRDS("2026 data/ncha_clean_2026.rds")

# ---- 0. Find the columns ---------------------------------------
# Looks up each question by its ID. Stops if a name doesn't match,
# so I find out here instead of halfway through a model.

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  stopifnot(length(h) == 1)
  h
}

q49 <- col1("^R?N3Q49$")
q50 <- col1("^R?N3Q50$")
q51 <- col1("^R?N3Q51$")
q52 <- col1("^R?N3Q52$")
q14 <- col1("^R?N3Q14$")
q96 <- col1("^R?N3Q96$")

a3 <- col1("^R?N3Q47A3$")   # financial problems
a6 <- col1("^R?N3Q47A6$")   # family problems
a7 <- col1("^R?N3Q47A7$")   # intimate relationship problems

dx   <- grep("^R?N3Q65A(3|7|12|14|20|26)$", names(dat), value = TRUE)
viol <- grep("^R?N3Q(19[A-E]|20[A-G])$", names(dat), value = TRUE)
fs   <- grep("^R?N3Q12[A-E]$", names(dat), value = TRUE)


# ---- 1. SBQ-R score --------------------------------------------
# The suicide screener is four questions added into one score from
# 3 to 18. Two of them have more answer choices in this survey than
# the scale is built for, so those get collapsed back down first.
# A score of 7 or higher is the standard cutoff for a positive screen.

dat <- dat %>%
  mutate(
    i1 = case_when(.data[[q49]] == 1 ~ 1,
                   .data[[q49]] == 2 ~ 2,
                   .data[[q49]] %in% 3:4 ~ 3,
                   .data[[q49]] %in% 5:6 ~ 4),
    i2 = .data[[q50]],
    i3 = case_when(.data[[q51]] == 1 ~ 1,
                   .data[[q51]] %in% 2:3 ~ 2,
                   .data[[q51]] %in% 4:5 ~ 3),
    i4 = .data[[q52]],
    sbqr = i1 + i2 + i3 + i4,
    sbqr_pos = as.integer(sbqr >= 7)
  )

# Range should print 3 to 18 and the percent should land near 25.2.
# If either is off, the collapse rules above need adjusting.
cat("SBQ-R range:", range(dat$sbqr, na.rm = TRUE), "\n")
cat(sprintf("Positive screen: %.1f%%  (ACHA reported 25.2%%)\n\n",
            100 * mean(dat$sbqr_pos, na.rm = TRUE)))


# ---- 2. Build the predictors -----------------------------------
# Several of these are grids with one column per option. Collapsing
# each to a single yes/no keeps the model small enough for the
# number of students I have.

anyof <- function(cols) {
  if (!length(cols)) return(NA_integer_)
  m <- as.matrix(dat[cols])
  as.integer(rowSums(m == 1, na.rm = TRUE) > 0)
}

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
    any_dx     = anyof(dx),    # ever diagnosed with any of 6 MH conditions
    any_viol   = anyof(viol),  # any violence, partner or not, last 12 months
    fs_score   = if_else(fs_answered >= 4,
                         rowSums(fs_hit, na.rm = TRUE), NA_real_),
    food_insec = as.integer(fs_score >= 2),
    
    # N3Q96 runs backwards, so flipping it makes higher = worse.
    socmed     = 6 - .data[[q96]],
    short_sleep = as.integer(.data[[q14]] <= 2),
    
    # Adversity items are coded 1 / 2. Recoded to 0 / 1 so that
    # 1 = yes.
    adv_money  = as.integer(.data[[a3]] == 2),
    adv_family = as.integer(.data[[a6]] == 2),
    adv_intim  = as.integer(.data[[a7]] == 2),
    
    # Class year and housing get squeezed to two groups each. With
    # ~176 positive screens there isn't room for six categories.
    level    = factor(if_else(grepl("Master|Doctor", class_yr),
                              "Grad", "Undergrad"),
                      levels = c("Undergrad", "Grad")),
    oncampus = factor(if_else(grepl("Campus|campus hous", housing),
                              "On campus", "Off campus"))
  )

# Scores get converted to standard deviations so a 4-24 scale and a
# 0-24 scale can sit next to each other on the same chart.
for (v in c("k6", "loneliness", "belonging", "socmed"))
  dat[[paste0(v, "_z")]] <- as.numeric(scale(dat[[v]]))

# Every value here should now be a percent between 0 and 100.
# Food insecurity should land in the 30s.
cat("Prevalence of added predictors (%):\n")
print(round(colMeans(dat[c("any_dx", "any_viol", "food_insec",
                           "adv_money", "adv_family", "adv_intim")],
                     na.rm = TRUE) * 100, 1))


# ---- 3. The models ---------------------------------------------
# Four versions of the same model, each adding more. The point is to
# watch what happens to belonging as distress, then loneliness, then
# everything else gets added. If belonging fades once distress is in,
# that means belonging works through distress rather than on its own.

ctrl <- c("age", "level", "first_gen", "disability", "oncampus")
ctrl <- ctrl[ctrl %in% names(dat)]
cat("\nControls used:", ctrl, "\n")

m1 <- glm(reformulate(c("belonging_z", ctrl),
                      "sbqr_pos"), dat, family = binomial)

m2 <- glm(reformulate(c("belonging_z", "k6_z", ctrl),
                      "sbqr_pos"), dat, family = binomial)

m3 <- glm(reformulate(c("belonging_z", "k6_z", "loneliness_z", ctrl),
                      "sbqr_pos"), dat, family = binomial)

m4 <- glm(reformulate(c("belonging_z", "k6_z", "loneliness_z",
                        "any_dx", "any_viol", "food_insec", "socmed_z",
                        "adv_money", "adv_family", "adv_intim", ctrl),
                      "sbqr_pos"), dat, family = binomial)

# Pulls the results out of each model and stacks them into one table.
grab <- function(fit, label) {
  s <- summary(fit)$coefficients
  k <- grep("_z$|^any_|^food_|^adv_", rownames(s), value = TRUE)
  b  <- s[k, 1]
  se <- s[k, 2]
  data.frame(model = label,
             term  = k,
             or    = exp(b),
             low   = exp(b - 1.96 * se),
             high  = exp(b + 1.96 * se),
             p     = s[k, 4],
             n     = nobs(fit),
             row.names = NULL)
}

res <- bind_rows(grab(m1, "1. Belonging only"),
                 grab(m2, "2. + Distress"),
                 grab(m3, "3. + Loneliness"),
                 grab(m4, "4. Full model"))

# Above 1 means higher odds of a positive screen, below 1 means lower.
# If the low-high range crosses 1, there's no clear link.
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

print(res %>%
        mutate(across(c(or, low, high), ~ round(., 2)),
               p = fmt_p(p)),
      row.names = FALSE)

write.csv(res, "2026 out/rq9_sbqr.csv", row.names = FALSE)

# This is the answer to the research question, on its own.
cat("\nBelonging across models:\n")
res %>% filter(term == "belonging_z") %>%
  with(cat(sprintf("%-20s OR %.2f (%.2f-%.2f)  p = %s\n",
                   model, or, low, high, fmt_p(p)), sep = ""))


# ---- 4. Treatment gap ------------------------------------------
# Of the students who screened positive, how many actually used
# mental health services in the last year.

q54b <- col1("^R?N3Q54B$")

cat("\nServices in last 12 months, by screen result (%):\n")
print(round(100 * prop.table(
  table(screen = dat$sbqr_pos, services = dat[[q54b]]), 1), 1))


# ---- 5. Main figure --------------------------------------------
# Everything from the full model, sorted and split into three groups
# so it's readable at a glance. Dot is the estimate, line is the
# range. Filled dot means a clear effect, hollow means unclear.

lab <- c(belonging_z  = "Campus belonging",
         k6_z         = "Psychological distress",
         loneliness_z = "Loneliness",
         socmed_z     = "Social media negativity",
         any_dx       = "Prior MH diagnosis",
         any_viol     = "Violence exposure",
         food_insec   = "Food insecurity",
         adv_money    = "Financial problems",
         adv_family   = "Family problems",
         adv_intim    = "Relationship problems")

grp <- c(belonging_z  = "Psychological",
         k6_z         = "Psychological",
         loneliness_z = "Psychological",
         socmed_z     = "Psychological",
         any_dx       = "History & exposure",
         any_viol     = "History & exposure",
         food_insec   = "Life circumstances",
         adv_money    = "Life circumstances",
         adv_family   = "Life circumstances",
         adv_intim    = "Life circumstances")

pd <- res %>%
  filter(model == "4. Full model") %>%
  mutate(name  = lab[term],
         group = factor(grp[term],
                        levels = c("Psychological",
                                   "History & exposure",
                                   "Life circumstances")),
         clear = low > 1 | high < 1,
         name  = fct_reorder(name, or))

p <- ggplot(pd, aes(or, name)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey45", linewidth = 0.6) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                width = 0, linewidth = 1.1, color = "grey55") +
  geom_point(aes(fill = clear), shape = 21, size = 5,
             stroke = 1, color = "grey20", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", or)),
            vjust = -1.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#B3352E", "FALSE" = "white")) +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3)) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y",
             switch = "y") +
  labs(title = "What predicts a positive suicide risk screen",
       subtitle = paste0("Adjusted odds ratios. Scales are per 1 SD; ",
                         "the rest are yes vs. no.\n",
                         "Filled = clear effect. Hollow = not distinguishable ",
                         "from no effect."),
       x = "Odds of a positive SBQ-R screen (log scale)", y = NULL,
       caption = paste0("NCHA-IIIb | University of Tennessee | Spring 2026 | N = ",
                        unique(pd$n))) +
  theme_minimal(base_size = 15) +
  theme(plot.title      = element_text(face = "bold", size = 18),
        plot.subtitle   = element_text(color = "grey30", size = 11,
                                       lineheight = 1.1),
        plot.caption    = element_text(color = "grey45", size = 9),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold",
                                         hjust = 1, size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text = element_text(color = "black"),
        panel.spacing = unit(0.8, "lines"))

print(p)
ggsave("2026 out/fig_rq9_sbqr.png", p, width = 10, height = 7, dpi = 300)


# ---- 6. Belonging across models --------------------------------
# The same number shown four times, once per model. If the dots
# shift toward 1 as more gets added, belonging's protection is
# really coming from the things added alongside it.

b <- res %>% filter(term == "belonging_z")

p2 <- ggplot(b, aes(or, fct_rev(model))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                width = 0, linewidth = 1.1, color = "grey55") +
  geom_point(size = 5, color = "#B3352E") +
  geom_text(aes(label = sprintf("%.2f", or)),
            vjust = -1.3, size = 4.2, fontface = "bold") +
  scale_x_log10() +
  labs(title = "Does belonging stay protective?",
       subtitle = "Odds ratio for campus belonging as other factors enter the model",
       x = "Odds ratio per 1 SD of belonging (log scale)", y = NULL,
       caption = "NCHA-IIIb | University of Tennessee | Spring 2026") +
  theme_minimal(base_size = 15) +
  theme(plot.title    = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "grey30", size = 11),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"))

print(p2)
ggsave("2026 out/fig_rq9_belonging.png", p2, width = 9, height = 5, dpi = 300)