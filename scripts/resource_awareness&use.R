library(tidyverse)

dat <- readRDS("2026 data/ncha_clean_2026.rds")

resource_lookup <- tibble(
  suffix = as.character(1:9),
  resource = c(
    "Drug and alcohol use and recovery",
    "Campus care team",
    "Disability/accommodations support",
    "Basic needs support",
    "Health clinic",
    "Health promotion and wellness programs",
    "Mental health counseling or referral",
    "Recreation center",
    "Sexual assault or relationship violence support"
  )
)

# Identify awareness and use variables
aware <- grep("^R?N3Q87_[0-9]+$", names(dat), value = TRUE)
used  <- grep("^R?N3Q88_[0-9]+$", names(dat), value = TRUE)

aware_suffix <- sub("^R?N3Q87_", "", aware)
used_suffix  <- sub("^R?N3Q88_", "", used)

# Match variables to the ten resources by suffix
if (!all(resource_lookup$suffix %in% aware_suffix)) {
  stop("Some resource awareness variables are missing. Check names(dat).")
}
if (!all(resource_lookup$suffix %in% used_suffix)) {
  stop("Some resource use variables are missing. Check names(dat).")
}

aware <- aware[match(resource_lookup$suffix, aware_suffix)]
used  <- used[match(resource_lookup$suffix, used_suffix)]

# Convert responses to binary indicators
sel <- function(x) as.integer(!is.na(x) & x != 0)

A <- as.data.frame(lapply(dat[aware], sel))
U <- as.data.frame(lapply(dat[used], sel))

# Calculate awareness and use percentages
gap <- tibble(
  resource = resource_lookup$resource,
  n_aware = colSums(A),
  n_used = colSums(U),
  pct_aware = 100 * colMeans(A),
  pct_used = 100 * colMeans(U),
  pct_used_of_aware =
    100 * colSums(A == 1 & U == 1) / pmax(colSums(A), 1)
) %>%
  mutate(gap = pct_aware - pct_used) %>%
  arrange(desc(gap))

print(
  gap %>% mutate(across(where(is.numeric), ~ round(., 1))),
  n = Inf
)

write.csv(gap, "2026 out/rq13_gap.csv", row.names = FALSE)

# Prepare data for the graph
pd <- gap %>%
  select(resource, Aware = pct_aware, Used = pct_used) %>%
  pivot_longer(
    -resource,
    names_to = "status",
    values_to = "pct"
  ) %>%
  mutate(
    resource = factor(resource, levels = rev(gap$resource)),
    resource = forcats::fct_reorder(resource, pct, .fun = max)
  )

# Plot using the actual resource names
p <- ggplot(pd, aes(pct, resource, fill = status)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.68
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3.2
  ) +
  scale_fill_manual(
    values = c(
      Aware = "#8FA8C8",
      Used = "#B3352E"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 108),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Campus Resources: Awareness vs. Actual Use",
    subtitle = "Percentage of all students surveyed",
    x = "Percentage of students",
    y = NULL,
    fill = NULL,
    caption = "NCHA-III | UTK | Spring 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", size = 11),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 25, 10, 10)
  )

print(p)

ggsave(
  "2026 out/fig_rq13_gap.png",
  p,
  width = 12,
  height = 7.5,
  dpi = 300
)