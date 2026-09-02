# =============================================================
# NCHA-IIIb (Spring 2026, UTK)
#
# Only what's needed to make the data analyzable:
#   find columns -> blank out non-answers -> fix Yes/No coding
#   -> build scores -> check
#
# Saves data/ncha_clean.rds. Load that from now on.
#
# NOT IN THIS DATASET: sex assigned at birth (N3Q67), gender
# identity, sexual orientation (N3Q68), and race/ethnicity (N3Q75).
# Those questions were not collected. The column names jump straight
# from N3Q66R to N3Q69, and from N3Q74A to N3Q76. So I can't run any
# analysis by gender, orientation, or race, and that has to be
# stated as a limitation.

library(haven)
library(dplyr)
library(psych)

dir.create("out",  showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

raw <- read_sav("NCHA-III WEB SPRING 2026 UNIVERSITY OF TENNESSEE KNOXVILLE.sav")
cat("Rows:", nrow(raw), "Columns:", ncol(raw), "\n")

# ---- 1. Find the columns ------------------------------------
# NCHA added an "R" to revised questions one at a time, so a block
# can be mixed (N3Q2A + RN3Q2B/C/D). Searching by pattern finds them
# either way. Stops if the count is wrong, because a score built
# from the wrong questions is wrong.

find_items <- function(pattern, expect) {
  hits <- grep(pattern, names(raw), value = TRUE)
  stopifnot(length(hits) == expect)
  hits
}

flourish <- find_items("^R?N3Q41[A-H]$", 8)   # wellbeing:  8-56
k6i      <- find_items("^R?N3Q44[A-F]$", 6)   # distress:   0-24
ucla     <- find_items("^R?N3Q45[A-C]$", 3)   # loneliness: 3-9
belong   <- find_items("^R?N3Q2[A-D]$",  4)   # belonging:  4-24
safety_i <- find_items("^R?N3Q21[A-D]$", 4)   # safety:     4-16
cdrisc   <- find_items("^R?N3Q42[AB]$",  2)   # resilience: 0-8

# ---- 2. Blank out the non-answers ---------------------------
# Answers that aren't really answers. Left in, they get treated as
# real numbers and mess up analyses.
#
# 99 = "Don't know" on the nine prescription misuse questions. It's
# a label, not a quantity. Only these nine questions have it,
# a blanket version could delete real write-in numbers like
# a weight of 99 lbs.
#
# 5 = "Does not apply" on the safety questions, which run 1-4 from
# unsafe to very safe. Leave the 5 in and those people look like the
# safest students on campus.
#
# 1 = "I didn't do this activity" on the protective behaviors, and
# 6 = "I don't use social media" on N3Q96.

dk    <- grep("^R?N3Q22(E|F1|F2|G|H1|H2|I|J1|J2)$", names(raw), value = TRUE)
prot  <- grep("^R?N3Q17[A-C]$", names(raw), value = TRUE)
socm  <- grep("^R?N3Q96$",      names(raw), value = TRUE)

dat <- raw %>%
  mutate(across(all_of(dk),       ~ replace(., . == 99, NA)),
         across(all_of(safety_i), ~ replace(., . == 5,  NA)),
         across(all_of(prot),     ~ replace(., . == 1,  NA)),
         across(all_of(socm),     ~ replace(., . == 6,  NA)))

# ---- 3. Fix the Yes/No coding -------------------------------
# The survey stores No = 1 and
# Yes = 2. glm() (fits generalized linear models) flat out refuses that. 
# It wants 0 and 1. And even where it runs, 
# everything gets measured against the wrong baseline,
# so the odds ratio I'd report is wrong.
#
# I'm not recoding blind. Two conditions, both have to be true:
#   1. the column is in one of the known Yes/No blocks, and
#   2. every answer in it is actually a 1 or a 2
#
# The second check is what keeps me out of trouble. N3Q63A is Yes/No
# but N3Q63B right next to it is a 1-3 academic impact question, and
# a pattern that's even slightly loose would grab it and silently
# turn a 3-level answer into garbage. Checking the values first
# means that can't happen.

yn_blocks <- paste0("^R?N3Q(19[A-E]|20[A-G]|29[A-L]|23[A-K]|25B[12]|",
                    "33[A-C]|63A[0-9]+|65A[0-9]+|30A|31A|32|39|40|",
                    "54[AB]|55A|64B|22P|77[AB]|81[A-C]|82[A-G])$")

cand <- grep(yn_blocks, names(dat), value = TRUE)

is_12 <- function(x) {
  v <- unique(x[!is.na(x)])
  length(v) > 0 && all(v %in% c(1, 2))
}

yn <- cand[sapply(dat[cand], is_12)]

cat("Yes/No columns recoded to 0/1:", length(yn), "of",
    length(cand), "candidates\n")

dat <- dat %>%
  mutate(across(all_of(yn), ~ case_when(. == 1 ~ 0, . == 2 ~ 1)))

# The substance use grid is the exception. It uses No = 0 and Yes = 3
# because of how that scale is scored. I leave the originals alone
# and add a parallel set of 0/1 copies ending in "_bin", so I have
# whichever one a given analysis needs.

assist <- grep("^R?N3Q22A[0-9]+$", names(dat), value = TRUE)

if (length(assist)) {
  dat <- dat %>%
    mutate(across(all_of(assist),
                  ~ case_when(. == 0 ~ 0, . == 3 ~ 1),
                  .names = "{.col}_bin"))
  cat("ASSIST columns given 0/1 copies:", length(assist), "\n")
}

# ---- 4. Build the scores ------------------------------------
# Adds each set of questions into one score. Answered at least 80%?
# Average what they gave and scale it up. Below that the score is
# blank. The person stays in the data, they just sit out models
# using that one score.

score <- function(df, items) {
  m <- as.matrix(df[items]); k <- length(items)
  out <- rowMeans(m, na.rm = TRUE) * k
  out[rowSums(!is.na(m)) < ceiling(0.8 * k)] <- NA
  out
}

dat <- dat %>%
  mutate(
    flourishing = score(., flourish),   # higher = better
    k6          = score(., k6i),        # higher = worse
    loneliness  = score(., ucla),       # higher = lonelier
    belonging   = score(., belong),     # higher = connected
    safety      = score(., safety_i),   # higher = safer
    cdrisc2     = score(., cdrisc),     # higher = resilient
    k6_serious  = if_else(k6 >= 13, 1, 0)   # clinical cutoff
  )


# ---- 5. Sanity check ----------------------------------------
# Confirms each score landed inside its only possible range. A
# distress score of 47 on a 0-24 scale means something broke above.

check <- function(x, lo, hi) {
  r <- range(x, na.rm = TRUE)
  r[1] >= lo && r[2] <= hi
}

stopifnot(
  check(dat$flourishing, 8, 56),
  check(dat$k6,          0, 24),
  check(dat$loneliness,  3, 9),
  check(dat$belonging,   4, 24),
  check(dat$safety,      4, 16),
  check(dat$cdrisc2,     0, 8)
)
cat("Range checks passed.\n")

# ---- 6. Reliability -----------------------------------------
# Checks whether the questions in each scale hang together. These
# are published measures, so a low number means my code broke
# something. Expect ~.90 / ~.85 / ~.80.
#
# The warnings are left switched on. If an item runs backwards,
# alpha() says so, and that message is the only thing that would
# catch a reverse-coding problem.

alpha_of <- function(items)
  psych::alpha(as.data.frame(dat[items]), check.keys = FALSE)$total$raw_alpha

rel <- data.frame(
  scale = c("Flourishing","K6","UCLA-3","Belonging","Safety","CD-RISC-2"),
  alpha = round(c(alpha_of(flourish), alpha_of(k6i), alpha_of(ucla),
                  alpha_of(belong), alpha_of(safety_i), alpha_of(cdrisc)), 3)
)
print(rel)
write.csv(rel, "out/scale_reliability.csv", row.names = FALSE)


# ---- 7. Demographics as factors -----------------------------
# Class year coded 1-6 is not a quantity. Left as
# a number, the model fits one straight line from freshman to grad
# student instead of comparing the groups. Wrong, with no warning.
# as_factor() also keeps the text labels, so results read
# "Sophomore" instead of "2".

# Only the demographics that are actually in the file are listed
# below. Sex at birth, gender identity, orientation, and race were
# not collected, so they're gone from here on purpose.

# Two ways to find a column, tried in order. The question ID is
# checked first. If the export renamed it, the question wording is
# searched instead. The .sav carries the text of every question,
# and the wording doesn't change even when the name does.
#
# The wording search warns when it fires, so a column that got
# matched by text instead of by ID is something I see rather than
# something I find out about later.

q_text <- sapply(dat, function(x) {
  a <- attr(x, "label"); if (is.null(a)) "" else a
})

find_col <- function(id, text) {
  hit <- grep(paste0("^R?", id, "(_[0-9]+)?$"), names(dat), value = TRUE)
  if (length(hit) == 1) return(hit)
  hit <- names(q_text)[grepl(text, q_text, ignore.case = TRUE)]
  if (length(hit) >= 1) {
    warning("[", id, "] not found by ID -- matched by question text to '",
            hit[1], "'. Verify this is right.", call. = FALSE)
    return(hit[1])
  }
  NA_character_
}

demo <- list(
  class_yr = c("N3Q72",  "your year in school"),
  enroll   = c("N3Q73",  "enrollment status"),
  intl     = c("N3Q74A", "international student"),
  relation = c("N3Q76",  "your relationship status"),
  housing  = c("N3Q78",  "where do you currently live")
)

for (nm in names(demo)) {
  col <- find_col(demo[[nm]][1], demo[[nm]][2])
  if (!is.na(col)) {
    dat[[nm]] <- as_factor(dat[[col]])
    cat(nm, "<-", col, "\n")
  } else {
    warning("[", nm, "] not found by ID or question text -- skipped.",
            call. = FALSE)
  }
}

# Athletics and disability are grids: one column per option, not one
# column per question. Collapse each to a single yes/no. These
# already went through Section 3, so a yes is now a 1, not a 2.
# Any 1 across the row means yes.
#
# Anyone who saw none of the columns is left blank, NOT counted as a
# No. If the printed table below shows a big pile of NA, that block
# was behind skip logic and the blanks are really Nos -- switch the
# not_asked argument to 0 for that variable.

any_yes <- function(pattern, not_asked = NA) {
  cols <- grep(pattern, names(dat), value = TRUE)
  if (!length(cols)) return(NULL)
  m   <- as.matrix(dat[cols])
  hit <- rowSums(m == 1, na.rm = TRUE) > 0
  out <- factor(if_else(hit, "Yes", "No"), levels = c("No", "Yes"))
  if (is.na(not_asked)) out[rowSums(!is.na(m)) == 0] <- NA
  out
}

dat$athlete    <- any_yes("^R?N3Q81[A-C]$")
dat$disability <- any_yes("^R?N3Q82[A-G]$")

# Greek life comes from the membership question alone (N3Q77A).
# N3Q77B asks whether they live in a chapter house, which is only
# shown to members and isn't a difference I care about.

greek_col <- grep("^R?N3Q77A$", names(dat), value = TRUE)
if (length(greek_col) == 1) {
  dat$greek_any <- factor(case_when(dat[[greek_col]] == 0 ~ "No",
                                    dat[[greek_col]] == 1 ~ "Yes"),
                          levels = c("No", "Yes"))
}

for (nm in c("athlete", "disability", "greek_any"))
  if (!is.null(dat[[nm]])) { cat("\n--", nm, "--\n"); print(table(dat[[nm]], useNA = "ifany")) }

# Age is a real number, so it stays numeric.
age_col <- find_col("N3Q69", "how old are you")
if (!is.na(age_col)) dat$age <- as.numeric(dat[[age_col]])

# First-gen: neither parent finished a bachelor's (codes 1-4).
# Code 8 is "Don't know" and must be blank, not continuing-gen.
pe_col <- find_col("N3Q84", "highest level of education completed by either")
if (!is.na(pe_col)) {
  pe <- as.numeric(dat[[pe_col]]); pe[pe == 8] <- NA
  dat$first_gen <- factor(if_else(pe <= 4, "First-gen", "Continuing-gen"),
                          levels = c("Continuing-gen", "First-gen"))
  print(table(dat$first_gen, useNA = "ifany"))
}


# ---- 7b. What I can and can't compare -----------------------
# Prints the group variables that survived, so I know what my
# predictor list can actually contain. Anything not on this list
# either wasn't collected or wasn't found.

have <- c("class_yr", "enroll", "intl", "relation", "housing",
          "athlete", "disability", "greek_any", "first_gen", "age")
have <- have[have %in% names(dat)]

cat("\n-- Group variables available --\n")
print(have)
cat("\nNot available (not collected): sex at birth, gender identity,",
    "sexual orientation, race/ethnicity\n")


# ---- 8. Save ------------------------------------------------
# zap_labels() drops the internal SPSS codes 
# but keeps the question wording, so tables read "How often
# did you feel nervous?" instead of "N3Q44A".

dat <- zap_labels(dat)
saveRDS(dat, "data/ncha_clean.rds")
cat("Saved data/ncha_clean.rds\n")

# Next session:  dat <- readRDS("data/ncha_clean.rds")
#
# Yes/No outcomes are already 0/1, so a logistic model just runs:
#   glm(N3Q54B ~ belonging + age, data = dat, family = binomial)

View(dat)
