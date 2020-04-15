library("dmetric", warn.conflicts = FALSE)
library("dplyr")
set.seed(123)

m_0   <- load_model("1339_0",   model_dir = "models_2020", project = "Jamaica")
m_11  <- load_model("1339_11",  model_dir = "models_2020", project = "Jamaica")
m_33  <- load_model("1339_33",  model_dir = "models_2020", project = "Jamaica")
m_184 <- load_model("1339_184", model_dir = "models_2020", project = "Jamaica")

mod <- bind_rows(
  m_0$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_000"),
  m_11$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_011"),
  m_33$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_033"),
  m_184$dscore %>%
    select(subjid, cohort, a, d, daz) %>%
    mutate(model = "1339_184")) %>%
  mutate(month = a * 12) %>%
  select(-a) %>%
  sample_frac(size = 0.25)

model_1339_data <- mod

saveRDS(model_1339_data, file = "data/model_1339_data.rds")
