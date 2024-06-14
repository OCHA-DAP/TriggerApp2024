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
admin_layer_names <- c("eth_admbnda_adm0_csa_bofedb_itos_2021",paste0("eth_admbnda_adm",1:3,"_csa_bofedb_2021"))

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
eth_area_lookup <- lgdf |>
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
if(only_eth){
  df_area_lookup |>
    write_parquet(
      file.path("data",
                "df_eth_admin_area_lookup.parquet")
    )

}



fp_lgdf <-     file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "processed",
  "lac",
  "lac_all_cod_adms.rds"
)
lgdf_lac <- read_rds(fp_lgdf)

lac_df_area_lookup <- lgdf_lac |>
  imap(\(gdf_tmp,nm_tmp){

    gdf_tmp_area <- gdf_tmp |>
      rename_with(
      ~str_replace(.x,"_es","_en")
    ) %>%
      mutate(
        area= as.numeric(st_area(st_geometry(.)))
      ) |>
      select(
        matches("^adm\\d_[ep]|^area|pct_area")
      )

    df_id <- str_extract(
      nm_tmp,
      "adm\\d"
    )
    gdf_tmp_area |>
      st_drop_geometry() |>
      select(starts_with(df_id),area) |>
      rename_with(.cols = matches(df_id),.fn = function(nm_tmp) str_replace(nm_tmp,"adm\\d","adm")) |>
      mutate(
        admin_level = df_id
      )

  }) |>
  list_rbind()

df_area_lookup <- bind_rows(
  eth_area_lookup,
  lac_df_area_lookup
)
df_area_lookup |>
  write_parquet(
    file.path("data",
              "df_admin_area_lookup.parquet")
  )


# add in afg area and  overwrite

df_lookup <- read_parquet(
    file.path("data",
              "df_admin_area_lookup.parquet")
  )


# load in afg shapefiles
zf <- "../ds-aa-afg-drought/afg_admbnda_agcho.zip"
zf_vp <- paste0("/vsizip/",zf)
st_layers(zf_vp)

lgdf_afg <- map(
  c(adm0_pcode = "afg_admbnda_adm0_agcho_20211117",
    adm1_pcode = "afg_admbnda_adm1_agcho_20211117",
    adm2_pcode = "afg_admbnda_adm2_agcho_20211117"),
  \(lyr){
    gdf <- st_read(zf_vp,lyr) |>
      janitor::clean_names() |>
      dplyr::select(matches("adm\\d_[pe]"))
    sf::st_simplify(gdf,dTolerance = 1000)
  }
)


afg_df_area_lookup <- lgdf_afg |>
  imap(\(gdf_tmp,nm_tmp){

    gdf_tmp_area <- gdf_tmp %>%
      mutate(
        area= as.numeric(st_area(st_geometry(.)))
      ) |>
      select(
        matches("^adm\\d_[ep]|^area|pct_area")
      )

    df_id <- str_extract(
      nm_tmp,
      "adm\\d"
    )
    gdf_tmp_area |>
      st_drop_geometry() |>
      select(starts_with(df_id),area) |>
      rename_with(.cols = matches(df_id),.fn = function(nm_tmp) str_replace(nm_tmp,"adm\\d","adm")) |>
      mutate(
        admin_level = df_id
      )

  }) |>
  list_rbind()

bind_rows(
  df_lookup,
  afg_df_area_lookup
) |>
  write_parquet(
    file.path("data",
              "df_admin_area_lookup.parquet")
  )
