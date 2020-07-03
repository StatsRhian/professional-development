# Read in all CPD

library("tidyverse")
library("yaml")
library("glue")

path = "test"

filename = list.files(path = path, pattern = "*.md", recursive = TRUE)

df = tibble(filename = filename)

yaml_to_tibble = function(file) {
  read_yaml(file) %>%
    as_tibble()
}

df %>%
  mutate(data = map(.x = filename, ~yaml_to_tibble(file = glue("{path}/{.x}")))) %>%
  select(-filename) %>%
  unnest(cols = c(data)) %>%
  View()

