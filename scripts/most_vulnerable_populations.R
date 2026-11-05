library(tidyverse)

dat <- readRDS("2026 data/ncha_clean_2026.rds")

col1 <- function(p) {
  h <- grep(p, names(dat), value = TRUE)
  stopifnot(length(h) == 1)
  h
}

q49 <- col1("^R?N3Q49$")
q50 <- col1("^R?N3Q50$")
q51 <- col1("^R?N3Q51$")
q52 <- col1("^R?N3Q52$")


# ---- Outcomes --------------------------------------------------

dat <- dat %>%
  mutate(
    i1 = case_when(.data[[q49]] == 1 ~ 1,
                   .data[[q49]] == 2 ~ 2,
                   .data[[q49]] %in% 3:4 ~ 3,
                   .data[[q49]] %in% 5:6 ~ 4),
    i3 = case_when(.data[[q51]] == 1 ~ 1,
                   .data[[q51]] %in% 2:3 ~ 2,
                   .data[[q51]] %in% 4:5 ~ 3),
    sbqr     = i1 + .data[[q50]] + i3 + .data[[q52]],
    sbqr_pos = as.integer(sbqr >= 7),
    distress = as.integer(k6 >= 13),
    lonely   = as.integer(loneliness >= 6),
    level    = factor(if_else(grepl("Master|Doctor", class_yr),
                              "Grad", "Undergrad"),
                      levels = c("Undergrad", "Grad")),
    oncampus = factor(if_else(grepl("Campus|campus hous", housing),
                              "On campus", "Off campus"))
  )

outs <- c(distress = "Serious distress",
          lonely   = "Lonely",
          sbqr_pos = "Suicide risk screen")

grps <- c(level      = "Student level",
          first_gen  = "First generation",
          disability = "Disability",
          intl       = "International",
          oncampus   = "Housing",
          greek_any  = "Greek life",
          athlete    = "Athlete",
          relation   = "Relationship status")

grps <- grps[names(grps) %in% names(dat)]


# ---- Lens 1 and 3: prevalence and absolute count ---------------

tab <- lapply(names(grps), function(g) {
  lapply(names(outs), function(o) {
    d <- dat[!is.na(dat[[g]]) & !is.na(dat[[o]]), ]
    d %>%
      group_by(group = grps[g], level = as.character(.data[[g]])) %>%
      summarise(outcome = outs[o],
                n       = n(),
                pct     = 100 * mean(.data[[o]]),
                cases   = sum(.data[[o]]),
                .groups = "drop")
  }) %>% bind_rows()
}) %>% bind_rows() %>%
  filter(n >= 30) %>%
  select(outcome, group, level, n, pct, cases)

print(tab %>% mutate(pct = round(pct, 1)) %>% arrange(outcome, desc(pct)),
      n = Inf)

write.csv(tab, "2026 out/rq8_prevalence.csv", row.names = FALSE)


# ---- Lens 2: adjusted disparity --------------------------------

ctrl <- c("age", "level", "first_gen", "disability", "oncampus")
ctrl <- ctrl[ctrl %in% names(dat)]

res <- lapply(names(outs), function(o) {
  fit <- glm(reformulate(ctrl, o), dat, family = binomial)
  s <- summary(fit)$coefficients
  k <- rownames(s)[-1]
  data.frame(outcome = outs[o],
             term    = k,
             or      = exp(s[k, 1]),
             low     = exp(s[k, 1] - 1.96 * s[k, 2]),
             high    = exp(s[k, 1] + 1.96 * s[k, 2]),
             p       = s[k, 4],
             n       = nobs(fit),
             row.names = NULL)
}) %>% bind_rows()

res$p_adj <- p.adjust(res$p, "BH")

print(res %>% mutate(across(c(or, low, high), ~ round(., 2)),
                     across(c(p, p_adj), ~ round(., 3))),
      row.names = FALSE)

write.csv(res, "2026 out/rq8_adjusted.csv", row.names = FALSE)


# ---- Top groups by each lens -----------------------------------

top_pct <- tab %>% group_by(outcome) %>% slice_max(pct, n = 3) %>%
  select(outcome, group, level, pct, cases)

top_n <- tab %>% group_by(outcome) %>% slice_max(cases, n = 3) %>%
  select(outcome, group, level, pct, cases)

print(top_pct, n = Inf)
print(top_n, n = Inf)


# ---- Figure: dumbbell plot -------------------------------------

pd <- tab %>%
  mutate(outcome = factor(outcome, levels = outs),
         level = recode(level,
                        "In a relationship but not married/partnered" = "In relationship",
                        "Not in a relationship" = "Not in relationship")) %>%
  group_by(outcome, group) %>%
  mutate(gap  = max(pct) - min(pct),
         rnk  = rank(-pct, ties.method = "first"),
         nlev = n(),
         side = case_when(rnk == 1    ~ "R",    # highest: label to the right
                          rnk == nlev ~ "L",    # lowest: label to the left
                          TRUE        ~ "M"),   # middle: label above
         high = rnk == 1,
         lab  = case_when(side == "R" ~ paste0("  ", level),
                          side == "L" ~ paste0(level, "  "),
                          TRUE        ~ level),
         hj   = case_when(side == "R" ~ 0, side == "L" ~ 1, TRUE ~ 0.5),
         vj   = if_else(side == "M", -0.9, 0.5)) %>%
  ungroup() %>%
  mutate(group = fct_reorder(group, gap, .fun = mean))

p <- ggplot(pd, aes(pct, group)) +
  geom_line(aes(group = group), color = "grey75", linewidth = 1.6) +
  geom_point(aes(color = high), size = 3.5) +
  geom_text(aes(label = lab, color = high, hjust = hj, vjust = vj),
            size = 3, show.legend = FALSE) +
  scale_color_manual(values = c(`TRUE` = "#B3352E", `FALSE` = "grey40"),
                     guide = "none") +
  facet_wrap(~ outcome, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = c(0.35, 0.35))) +
  scale_y_discrete(expand = expansion(add = 0.7)) +
  labs(title = "Disability stands out across all three outcomes",
       subtitle = "Percent screening positive; each row compares levels of one grouping variable (red = higher)",
       x = "% of students", y = NULL,
       caption = "NCHA-III | University of Tennessee | 2026") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey30", size = 10),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold"),
        panel.spacing.x = unit(1.2, "lines"))

print(p)
ggsave("2026 out/fig_rq8_vulnerable.png", p, width = 14, height = 8, dpi = 300)