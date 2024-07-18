# after storing data as external datasets (parquet files) for inital phase of development I decided to try
# storing them as package data which is typically recommended. I had it in my head that parquet files
# would be more compressed, lighter, and faster. But now I am not sure.

# therefore I moved the parquet files to a folder called `.data-scrap` and am using the functions
# I had previously used to load the parquet files to first load them and then convert to rda data files

# It seems like the data storage particulars and best option may ultimately depend on deployment environemtn
# For now if we are going to bundle as docker container for azure it would be interesting to see if we can
# just bundle the data as well.


library(arrow)
ldf <- load_df_forecast_parquets(
  dataset_dir = ".data-scrap/data_to_combine",
  dataset= "combined"
)

usethis::use_data(ldf)

library(tidyverse)
library(sf)
lgdf<- read_rds(".data-scrap/lgdf_combined.rds")
usethis::use_data(lgdf)




df_area_lookup <- arrow::read_parquet(".data-scrap/df_admin_area_lookup.parquet")
usethis::use_data(df_area_lookup)

