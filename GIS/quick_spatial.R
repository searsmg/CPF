
library(tidyverse)
library(sf)
library(mapview)


ss <- read_csv('/Users/megansears/Documents/CPF/Snowsurvey_2022/geode_master_2022.csv') %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)


mapview(ss)


st_write(ss, '/Users/megansears/Documents/CPF/Snowsurvey_2022/snowsurvey_points.shp')


watersheds <- st_read('/Users/megansears/Documents/CPF/GIS/cpf_watersheds_20210618.shp') %>%
  st_as_sf() %>% 
  filter(!Name %in% c('dadd', 'dry', 'dry_trib', 'washout')) %>%
  mutate(Name = ifelse(Name == "burn_transitional", "tunnel", Name),
         Name = ifelse(Name == 'noburn_transitional', 'mtcampus', Name)) %>%
  st_transform(crs = st_crs(ss))


mapview(watersheds)

st_write(watersheds, '/Users/megansears/Library/CloudStorage/OneDrive-Colostate/BRproposal_GISforDan/cpf_watersheds.shp')

## et watersheds for Dan
et_sheds <- st_read('/Users/megansears/Documents/Repos/ETF/GIS/watershed_bnd_UTM.shp') %>%
  st_as_sf() %>%
  filter(Name %in% c('Pass 1', 'Pass 2', 'HighUnmulch', 'HighMulch')) %>%
  st_transform(crs = st_crs(watersheds))

crs(et_sheds)

st_write(et_sheds, '/Users/megansears/Library/CloudStorage/OneDrive-Colostate/BRproposal_GISforDan/etf_watersheds.shp')


unsure <- st_read('/Volumes/Kampf/Private/field_data/hydroshare_2016_2019/spatial/watersheds.shp') %>%
  st_as_sf %>%
  filter(Site %in% c('Michigan River')) %>%
  st_transform(crs = st_crs(watersheds)) %>%
  rename(Name = Site)

watersheds <- bind_rows(watersheds, unsure)


mapview(unsure)
