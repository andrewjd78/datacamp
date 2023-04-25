# Code to calculate growth rate

require(tidyverse)

plot_stature_age <- function(x, ht = NULL, f_sex = NA){
  if (!is.na(f_sex)){
    x %<>% dplyr::filter(sex == f_sex)
  }
  p <- ggplot() + 
    geom_line(
      data = x, 
      aes(x = agemos, y = height, group = percentile), 
      color = "skyblue", 
      alpha = 0.8)
  if (!is.null(ht))
    p <- p + geom_line(data = ht, aes(x = agemos, y = height, color = name), size = 1.5)
  if (is.na(f_sex))
    p <- p + facet_grid(. ~ sex) 
  return(p)
}

preprocess_data_lenage <- function(data){
  data %>% 
    filter(Sex != "Sex") %>%
    select(Sex, Agemos, !contains("u")) %>% 
    select(Sex, Agemos, starts_with("P")) %>% 
    pivot_longer(
      starts_with("P"), 
      names_to = "percentile", 
      values_to = "height") %>% 
    rename(sex = Sex, agemos = Agemos) %>%
    mutate(across(c("agemos", "height"), as.numeric))
}

bday = c(Gabrielle = "6/11/2011", Sophie = "6/04/2014")

convert_height <- function(data, bday){
  data %>% 
    mutate(
      bday = recode(name, !!!bday) %>% lubridate::mdy(),
      date = date %>% lubridate::mdy(),
      agemos = lubridate::time_length(date - bday, unit="months"),
      height = height_in * 2.54) %>%
    select(-weight_lbs, -height_in)
}

data <- read_csv("~/RWorkspace/data/heightweight.txt")

growth_rate <- function(data, filter_name = NULL){
  if(!is.null(filter_name)){
    data <- data %>% filter(name == filter_name)
  } else {
    data <- data
  }

  data %>%
    group_by(name) %>%
    mutate(
      date = date %>% strptime("%m/%d/%Y"),
      dh = height_in %>% lead() - height_in,
      dt = (date %>% lead() - date) %>% as.integer(),
      growth_rate = dh/dt*100) %>%
    ungroup() %>%
    return()
}

gr <- data %>% growth_rate()

p <-
  ggplot(gr, aes(x = name, y = growth_rate), fill=name) +
  geom_boxplot(alpha = 0.80) +
  geom_point(aes(fill=name),size = 5, shape = 21, position = position_jitter())
