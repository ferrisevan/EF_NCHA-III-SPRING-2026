library(tidyverse)

files <- c(
  "2020" = "2020 data/ncha_clean_2020.rds",
  "2023" = "2023 data/ncha_clean_2023.rds",
  "2026" = "2026 data/ncha_clean_2026.rds"
)

years <- as.integer(names(files))
out_dir <- "2026 out"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

missing_files <- files[!file.exists(files)]

if (length(missing_files) > 0) {
  stop(
    "The following data files could not be found:\n",
    paste(missing_files, collapse = "\n")
  )
}

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

sel <- function(x) {
  as.integer(!is.na(x) & x != 0)
}

lab <- function(x) {
  a <- attr(x, "label")
  if (is.null(a)) NA_character_ else as.character(a)
}

calc_gap <- function(yr) {
  
  path <- files[[as.character(yr)]]
  dat <- readRDS(path)
  
  aw <- grep("^R?N3Q87_[0-9]+$", names(dat), value = TRUE)
  us <- grep("^R?N3Q88_[0-9]+$", names(dat), value = TRUE)
  
  aw_map <- setNames(aw, sub("^R?N3Q87_", "", aw))
  us_map <- setNames(us, sub("^R?N3Q88_", "", us))
  
  rows <- lapply(seq_len(nrow(resource_lookup)), function(i) {
    
    s <- resource_lookup$suffix[i]
    a <- unname(aw_map[s])
    u <- unname(us_map[s])
    
    if (
      length(a) == 0 || is.na(a) ||
      length(u) == 0 || is.na(u)
    ) {
      warning(
        "Year ", yr, ": missing awareness or use column for ",
        resource_lookup$resource[i],
        call. = FALSE
      )
      
      return(tibble(
        resource = resource_lookup$resource[i],
        aware_var = NA_character_,
        used_var = NA_character_,
        aware_label = NA_character_,
        n_total = nrow(dat),
        n_aware = NA_real_,
        n_used = NA_real_,
        pct_aware = NA_real_,
        pct_used = NA_real_,
        pct_used_of_aware = NA_real_
      ))
    }
    
    A <- sel(dat[[a]])
    U <- sel(dat[[u]])
    
    tibble(
      resource = resource_lookup$resource[i],
      aware_var = a,
      used_var = u,
      aware_label = lab(dat[[a]]),
      n_total = nrow(dat),
      n_aware = sum(A),
      n_used = sum(U),
      pct_aware = 100 * mean(A),
      pct_used = 100 * mean(U),
      pct_used_of_aware = if (sum(A) > 0) {
        100 * sum(A == 1 & U == 1) / sum(A)
      } else {
        NA_real_
      }
    )
  })
  
  bind_rows(rows) %>%
    mutate(
      year = yr,
      gap = pct_aware - pct_used
    )
}

gap_all <- bind_rows(lapply(years, calc_gap))

gap_all <- gap_all %>%
  mutate(year = factor(year, levels = years))

cat("\nStudents per year:\n")

print(
  gap_all %>%
    distinct(year, n_total) %>%
    arrange(year),
  n = Inf
)

cat("\nColumn check:\n")

print(
  gap_all %>%
    select(year, resource, aware_var, used_var, aware_label),
  n = Inf,
  width = Inf
)

latest <- max(years)

ord <- gap_all %>%
  filter(as.integer(as.character(year)) == latest) %>%
  arrange(desc(gap)) %>%
  pull(resource)

cat("\nPercentage aware by year:\n")

print(
  gap_all %>%
    select(resource, year, pct_aware) %>%
    pivot_wider(
      names_from = year,
      values_from = pct_aware,
      names_prefix = "aware_"
    ) %>%
    arrange(match(resource, ord)) %>%
    mutate(across(where(is.numeric), ~ round(., 1))),
  n = Inf,
  width = Inf
)

cat("\nPercentage used by year:\n")

print(
  gap_all %>%
    select(resource, year, pct_used) %>%
    pivot_wider(
      names_from = year,
      values_from = pct_used,
      names_prefix = "used_"
    ) %>%
    arrange(match(resource, ord)) %>%
    mutate(across(where(is.numeric), ~ round(., 1))),
  n = Inf,
  width = Inf
)

cat("\nAwareness minus use, in percentage points:\n")

print(
  gap_all %>%
    select(resource, year, gap) %>%
    pivot_wider(
      names_from = year,
      values_from = gap,
      names_prefix = "gap_"
    ) %>%
    arrange(match(resource, ord)) %>%
    mutate(across(where(is.numeric), ~ round(., 1))),
  n = Inf,
  width = Inf
)

write.csv(
  gap_all %>% mutate(year = as.integer(as.character(year))),
  file.path(out_dir, "rq13_gap_by_year.csv"),
  row.names = FALSE
)

yr_cols <- c(
  "2020" = "#8C8C8C",
  "2023" = "#D9A441",
  "2026" = "#B3352E"
)

pd <- gap_all %>%
  mutate(year = as.character(year)) %>%
  select(year, resource, Aware = pct_aware, Used = pct_used) %>%
  pivot_longer(
    cols = c(Aware, Used),
    names_to = "status",
    values_to = "pct"
  ) %>%
  mutate(
    status = factor(
      status,
      levels = c("Aware", "Used"),
      labels = c("Aware of resource", "Used resource")
    ),
    resource = factor(resource, levels = rev(ord)),
    year = factor(year, levels = c("2020", "2023", "2026"))
  )

p <- ggplot(
  pd,
  aes(x = pct, y = resource, fill = year)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.72,
    na.rm = TRUE
  ) +
  geom_text(
    aes(
      label = ifelse(
        is.na(pct),
        "",
        sprintf("%.0f%%", pct)
      )
    ),
    position = position_dodge(width = 0.8),
    hjust = -0.15,
    size = 2.6,
    na.rm = TRUE
  ) +
  facet_wrap(~status, nrow = 1) +
  scale_fill_manual(
    values = yr_cols,
    name = NULL,
    drop = FALSE,
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(
    xlim = c(0, 112),
    clip = "off"
  ) +
  labs(
    title = "Campus Resources: Awareness vs. Actual Use, Over Time",
    subtitle = "Percentage of all students surveyed, by survey year",
    x = "Percentage of students",
    y = NULL,
    caption = "NCHA | UTK | 2020, 2023, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", size = 11),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "top",
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 30, 10, 10)
  )

ggsave(
  filename = file.path(out_dir, "fig_rq13_gap_by_year.png"),
  plot = p,
  width = 14,
  height = 9,
  dpi = 300,
  limitsize = FALSE
)

cat("\nSaved CSV: ", normalizePath(
  file.path(out_dir, "rq13_gap_by_year.csv"),
  mustWork = FALSE
), "\n")

cat("Saved plot: ", normalizePath(
  file.path(out_dir, "fig_rq13_gap_by_year.png"),
  mustWork = FALSE
), "\n")

print(p)

if (interactive()) {
  try(grDevices::dev.flush(), silent = TRUE)
}
```