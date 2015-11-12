library(ggmap)
library(readxl)
library(dplyr)


# get address list
raw <- read_excel('data/raw/towing_addrs.xlsx', col_names = c('isValid', 'raw_addr'), skip = 1)

# prep data
df <- 
    raw %>% 
    select(raw_addr) %>%
    # strip city state
    mutate(addr = gsub(', Baltimore MD$', '', raw_addr)) 

# aggregate by address
# to reduce the number of API calls
df_agg <- 
    df %>% 
    # remove invalid addresses
    filter(!grepl("^0",addr) & addr!="") %>% 
    group_by(addr) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    arrange(-n) %>% 
    # add city/ state for geocoding
    mutate(geo = paste0(addr, ', Baltimore, MD'))


# geocode
geo <- 
    geocode(
        location = df_agg$geo,
        # use Data Science Toolkit
        source = 'dsk',
        # this is junk to get around Google API error
        client = 'foo', signature = 'bar'
    )
df_agg_geo <- bind_cols(df_agg, geo)

# de-aggregate back to original records
df_geo <- 
    left_join(
        df,
        df_agg_geo %>% select(addr, lon, lat),
        by = 'addr'
    )

# save results
save(df_agg_geo, file = 'data/cache/tow_addrs_coord.Rdata')
write.csv(df_geo, file = 'output/towing_addrs_coord.csv', row.names = F)
