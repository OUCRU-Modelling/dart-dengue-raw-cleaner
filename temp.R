library(tidyverse)

full_inc <- read_csv("incidence_full.csv") %>% rowid_to_column()
full_inc


glued_addrs <- full_inc %>%
  select(rowid, full_addr, commune, district) %>%
  mutate(across(c(full_addr, commune, district), ~ trimws(tolower(.x)))) %>%
  mutate(across(
    c(full_addr, commune, district),
    ~ case_when(.x %in% c("khong ro", "thi tran") ~ NA, .default = .x)
  )) %>%
  mutate(
    d_type = case_when(
      is.na(district) ~ NA,
      district %in% c("binh chanh", "hoc mon", "can gio", "cu chi", "nha be") ~
        "huyen",
      .default = "quan"
    ),
    c_type = case_when(
      is.na(commune) ~ NA,
      d_type == "quan" ~ "phuong",
      d_type == "huyen" ~ "xa",
    ),
  ) %>%
  unite(district, d_type, district, sep = " ", na.rm = TRUE) %>%
  unite(commune, c_type, commune, sep = " ", na.rm = TRUE) %>%
  mutate(across(
    c(full_addr, commune, district),
    ~ ifelse(.x == "", NA, .x)
  )) %>%
  unite(
    glue_addr,
    full_addr,
    commune,
    district,
    sep = ", ",
    na.rm = TRUE
  )

glued_addrs

glued_addrs %>% write_csv("glued_addrs.csv")
