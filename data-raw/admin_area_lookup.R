#' Code to create a long data.frame (parquet) that holds each admin unit, it's level (adm1,2,3) and the area of the polygon.
#' **WHY:** To combine admins for combined historical analysis and thresholding we need areas so we can use the areas as weights for `weighed.mean` calculations
#' **NOTE:**


library(sf)
library(tidyverse)
library(janitor)
library(arrow)


CODAB_FP = file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public","raw","eth","cod_ab",
  "eth_adm_csa_bofedb_2021_shp"
)

st_layers(CODAB_FP)
admin_layer_names <- paste0("eth_admbnda_adm",1:3,"_csa_bofedb_2021")
lgdf
# loop through layers clean up names, calc area of each row geom.
# likely someone will want a map viz added at some point to dashboard therefore..
# `lgdf` object can be used to generate it.
lgdf <- set_names(admin_layer_names,admin_layer_names) |>
  map(
    \(lyr_nm_tmp){
      st_read(CODAB_FP,
              layer = lyr_nm_tmp) %>%
        clean_names() %>%
        mutate(
          area = as.numeric(st_area(.))
        ) %>%
        select(
          matches("^adm\\d_[ep]|^area|pct_area")
        )
    }
  )


# loop through list of sf objects and put the areas in long format and bind together
df_area_lookup <- lgdf |>
  imap(
  \(gdf_tmp,nm_tmp){
  df_id <- str_extract(
    nm_tmp,
    "adm\\d"
  )
  gdf_tmp |>
    st_drop_geometry() |>
    select(starts_with(df_id),area) |>
    rename_with(.cols = matches(df_id),.fn = function(nm_tmp) str_replace(nm_tmp,"adm\\d","adm")) |>
    mutate(
      admin_level = df_id
    )
  }
  ) |>
  list_rbind()


# write as parquet to data folder
df_area_lookup |>
  write_parquet(
    file.path("data",
              "df_eth_admin_area_lookup.parquet")
  )

