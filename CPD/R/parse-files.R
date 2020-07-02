filenames = list.files(path = "data/2018", full.names = TRUE)

yaml_to_tibble = function(df){
  df %>%
    yaml::read_yaml() %>%
    list_modify(tags = list(.$tags)) %>%
    tibble::as_tibble()
}

all_data = tibble(filenames = filenames) %>%
  mutate(data = map(filenames, yaml_to_tibble)) %>%
  unnest(cols = data) %>%
  select(-filenames)
