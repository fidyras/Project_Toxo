library(tidyverse)
library(epiR)
library(forcats)
library(readxl)

#--------------------------------------------------
# 1. Import and clean
#--------------------------------------------------

toxoLit <- read_csv("data/Updated-data-added-Ingrid-Jun28-2025.csv",
                    na = c("", "NA"))

toxoLit %>% 
  select(
    reference = 1,
    data.year = 2,
    town = 12,
    state = 13,
    country = 3,
    country.code = 4,
    age.lower = 5,
    age.upper = 6,
    seropositive = 7,
    tested = 8,
    latitude = 9,
    longitude = 10,
    temp = 11,
    note = 16,
    link = 15,
    ref = 1
  ) %>%
  separate(reference, into = c("authors", "pub.year", "journal"), sep = ",") %>%
  mutate(
    seropositive = round(seropositive),
    tested = round(tested)
  ) %>% 
  filter(complete.cases(country, age.lower, age.upper, seropositive, tested)) -> toxoLit

#--------------------------------------------------
# 2. Row-level prevalence estimates
#--------------------------------------------------

row_matrix <- toxoLit %>%
  select(seropositive, tested) %>%
  as.matrix()

row_ci <- epiR::epi.conf(
  dat = row_matrix,
  ctype = "prevalence",
  method = "exact",
  conf.level = 0.95
)

toxoLit <- toxoLit %>%
  cbind(row_ci) %>%
  rename(
    prevalence = est,
    LowerCI = lower,
    UpperCI = upper
  )

#--------------------------------------------------
# 3. Africa-wide pooled prevalence
#--------------------------------------------------

Africa_matrix <- toxoLit %>%
  summarise(
    total.positive = sum(seropositive, na.rm = TRUE),
    total.tested = sum(tested, na.rm = TRUE)
  ) %>%
  as.matrix()

Africa <- epiR::epi.conf(
  dat = Africa_matrix,
  ctype = "prevalence",
  method = "exact",
  conf.level = 0.95
)

#--------------------------------------------------
# 4. Study-level prevalence (aggregated by ref)
#--------------------------------------------------

study_df <- toxoLit %>%
  group_by(ref, authors, pub.year, country) %>%
  summarise(
    total.positive = sum(seropositive),
    total.tested = sum(tested),
    .groups = "drop"
  ) %>% 
  mutate(label = paste0(authors, " ", pub.year, " ", country, " (n=", total.tested, ")"))

study_matrix <- study_df %>%
  select(total.positive, total.tested) %>%
  as.matrix()

study_ci <- epiR::epi.conf(
  dat = study_matrix,
  ctype = "prevalence",
  method = "exact",
  conf.level = 0.95
)

study_df <- cbind(study_ci, study_df) %>%
  rename(
    ref.seroprev = est,
    ref.lower = lower,
    ref.upper = upper
  ) %>%
  mutate(label = fct_reorder(label, ref.seroprev))

#--------------------------------------------------
# 5. Forest plot of study-level prevalence
#--------------------------------------------------

study_df %>%
  ggplot(aes(
    x = label,
    y = ref.seroprev,
    ymin = ref.lower,
    ymax = ref.upper,
    color = country
  )) +
  geom_pointrange(size = 0.9, linewidth = 1.2) +
  geom_hline(aes(yintercept = Africa$est), linetype = "dashed") +
  scale_color_viridis_d(option = "magma") +
  guides(color = "none") +
  coord_flip() +
  scale_y_continuous("Seroprevalence", labels = scales::percent) +
  scale_x_discrete("") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))
