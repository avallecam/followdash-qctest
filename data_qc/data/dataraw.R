library(here)
library(tidyverse)
library(lubridate)
library(charlatan)

n_obs <- 33

ch_data <- tibble(
  #participante
  name = ch_name(n = n_obs,messy = T),
  edad = ch_integer(n = n_obs,min = 7,max = 14),
  sexo = ch_integer(n = n_obs,min = 0,max = 1) %>% as.character(),
  peso_nino = ch_double(n = n_obs,mean = 3,sd = 2) %>% round(digits = 2),
  #ubicacion
  distrito = ch_integer(n = n_obs,min = 0,max = 3) %>% as.character(),
  nucleo = ch_integer(n = n_obs,min = 0,max = 4) %>% as.character(),
  ciudad = ch_integer(n = n_obs,min = 0,max = 10) %>% as.character(),
  position = ch_position(n = n_obs,bbox = c(-3.88, -73.54, -3.84, -73.49)),
  #fechas
  random_time = ch_integer(n = n_obs,min = 5,max = 10),
  primera_consulta = ch_date_time(n = n_obs),
  #estado
  seguimiento = ch_integer(n = n_obs,min = 5,max = 30),
  atenciones = ch_integer(n = n_obs,min = 0,max = 10)
  ) %>%

  # create missings
  # mutate_at(.vars = vars(-(name:sexo)),.funs = ch_missing) %>%
  missForest::prodNA(noNA = 0.2) %>%

  mutate(
    closed = ch_integer(n = n_obs,min = 0,max = 1) %>% as.logical() #caso en seguimiento cerrado?
  ) %>%

  # unnest dates
  unnest(cols = c(primera_consulta)) %>%
  mutate_at(.vars = vars(primera_consulta),
            .funs = lubridate::date) %>%
  mutate(fecha_atencion = primera_consulta %m+% days(random_time),
         prox_atencion = fecha_atencion %m+% days(7)) %>%
  select(-random_time) %>%

  # unnest geolocation
  mutate(position_geo = map(.x = position,.f = as.matrix),
         position_geo = map(.x = position_geo,.f = t),
         position_geo = map(.x = position_geo,.f = as_tibble)) %>%
  unnest(cols = c(position_geo)) %>%
  rename(lat = V1,
         lon = V2) %>%
  select(-position) %>%

  # hash identities
  mutate(hash=pmap(.l = select(.,name,edad,sexo),
                   .f = epitrix::hash_names)) %>%
  unnest(cols = c(hash)) %>%
  select(starts_with("hash"),starts_with("label"),everything()) %>%

  # create variable
  mutate(adolescente=if_else(edad>12,"Si","No"),
         sexo=if_else(sexo=="1","Femenino","Masculino")) %>%

  # create duplicates
  union_all(slice(.,1:6)) %>%
  select(-hash)

ch_data %>% glimpse()

rute <- "inst/rmarkdown/templates/cohort_followup/skeleton/"

# ch_data %>%
#   write_rds(str_c(rute,"data/ch_data.rds"))

ch_data %>%
  write_csv(str_c(rute,"data/ch_data.csv"))

ch_data %>%
  slice(1:15) %>%
  write_csv(str_c(rute,"data/ch_data_02.csv"))

# library(rlang)
# ch_data %>% group_by(adolescente) %>% skimr::skim(edad)
#
# ch_data %>%
#   ggplot(aes(x = lon,y = lat)) +
#   geom_point()
#
# library(leaflet)
# leaflet(data = ch_data) %>%
#   addTiles() %>%
#   addMarkers(lng = ~lon, lat = ~lat)
#
# ch_data %>%
#   count(distrito,nucleo,ciudad) %>%
#   avallecam::print_inf()


