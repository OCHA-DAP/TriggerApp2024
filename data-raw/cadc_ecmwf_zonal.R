library(terra)
library(sf)
library(exactextractr)
library(tidyverse)
aggregate_tabular_forecast <- function(df,
                                       report_level){

  grp_adms <- switch(report_level,
                     "adm0" =c("adm0_en","adm0_pcode"),
                     "adm1" =c("adm0_en","adm0_pcode","adm1_en","adm1_pcode"),
                     "adm2" =c("adm0_en","adm0_pcode","adm1_en","adm1_pcode","adm2_en","adm2_pcode")
  )
  # input data already aggreated to admin 3 -- so don't need to do anything to further
  # aggregate those for adm 3 level reporting
  if(report_level =="adm3"){
    ret <- df
  }else{

    ret <- df |>
      group_by(
        !!!syms(grp_adms),
        lt,
        pub_date,
        valid_date,
        pub_month,
        valid_month

      ) |>
      mutate(
        pct_area = pct_area/(sum(pct_area))
      ) %>%
      summarise(
        value = weighted.mean(x= value, w= pct_area),
        .groups="drop"
      )
  }
  return(ret)
}


#' zonal_ecmwf_mars
#'
#' @param r_wrapped
#' @param zone polygon of zones
#' @param stat `character` stat to reduce raster to over zones
#'
#' @return data.frame
#' @examples \dontrun{
#' library(tidyverse)
#' library(targets)
#' tar_source() # source `R/`
#'
#' # load adm boundaries
#' tar_load(gdf_aoi_adm)
#' gdb_ecmwf_mars_tifs <- file.path(
#'   Sys.getenv("AA_DATA_DIR"),
#'   "private",
#'   "processed",
#'   "lac",
#'   "ecmwf_seasonal",
#'   "seas51",
#'   "mars"
#' )
#' r <- load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
#' zonal_ecmwf_mars(r_wrapped = r, zone = gdf_aoi_adm$adm0, stat = "mean")
#' }
zonal_ecmwf_mars <- function(r_wrapped = r_ecmwf_mars,
                             zone ,
                             stat = "mean",
                             cols_keep) {
  r <- unwrap(r_wrapped)
  df_long <- exact_extract(
    x = r,
    y = zone,
    fun = stat,
    append_cols = cols_keep
  )
  df_long %>%
    # tibble()
    # glimpse()
    pivot_longer(cols = matches(paste0("^",stat))) %>%
    separate(name, into = c("stat", "pub_date", "lt"), sep = "\\.|_") %>%
    # add season information by combing dates w/ leadtimes
    mutate(
      pub_date = as_date(pub_date),
      pub_month = month(pub_date),
      lt = parse_number(lt),
      lt = lt - 1,
      valid_date = pub_date + months(lt),
      valid_month = month(valid_date)
    ) %>%
    select(-stat)
}

#' load_mars_raster
#'
#' @param gdb `character` filepath to ecmwf tifs
#' @note
#' I have had issues w/ this concept of wrapping rasters in targets before,
#' but this seems to be working as is here. For some reason in previous work I've had to write out the raster
#' to a file path and then wrap from there. If something goes wrong i'd like to try document the best solution,
#' @return wrapped spatraster object
#' @examples \dontrun{
#' library(tidyverse)
#' library(targets)
#' tar_source
#'
#' gdb_ecmwf_mars_tifs <- file.path(
#'   Sys.getenv("AA_DATA_DIR"),
#'   "private",
#'   "processed",
#'   "eth",
#'   "ecmwf_seasonal",
#'   "seas51",
#'   "mars"
#' )
#' load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
#' }
load_mars_raster <- function(gdb = gdb_ecmwf_mars_tifs,
                             rm_name = "lac_seasonal-montly-individual-members_tprate-") {
  fps <- list.files(
    path = gdb, pattern = "\\.tif$",
    full.names = T
  )
  r <- rast(fps)
  names(r) <- str_remove(names(r), rm_name)
  return(wrap(r))
}

# file path to folder containing cogs
gdb_ecmwf_mars_tifs <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "lac",
  "ecmwf_seasonal",
  "seas51",
  "mars"
)

# file path to shape files adm 0- 2
fp_lgdf <-     file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public",
      "processed",
      "lac",
      "lac_all_cod_adms.rds"
    )

r_lac <- load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
unwrap(r_lac) |> names()
lgdf_lac <- read_rds(fp_lgdf)


# rename and prep zonal file to use for extraction
gdf_adm2_lac <- lgdf_lac$adm2 |>
  rename_with(
    ~str_replace(.x,"_es","_en")
  ) %>%
  mutate(
    area= st_area(st_geometry(.)),
    pct_area = as.numeric(area/sum(area))
  )

# extract to adm2
df_ecmwf_zonal_adm2 <- zonal_ecmwf_mars(r_wrapped = r_lac,
                                        zone = gdf_adm2_lac,
                                        stat = 'mean',
                                        cols_keep = colnames(gdf_adm2_lac)
                                        )

# loop through admins in zonal file
lgdf_zonal_lac <- map(
  c(adm0="adm0",adm1="adm1",adm2="adm2"),
  \(rep_level_tmp){
    aggregate_tabular_forecast(df = df_ecmwf_zonal_adm2,
                               report_level = rep_level_tmp)
  }
)

library(arrow)

# open each admin zonal file for ethiopia and
# bind the resepctive admin zonal calcs for lac admins
lgdf_combined_zonal <- map(  c(adm0="adm0",adm1="adm1",adm2="adm2"),
      \(adm_tmp){
        fp_eth <- paste0( "df_eth_mars_zonal_",adm_tmp,".parquet")
        df_tmp <- arrow::read_parquet(file.path("data",fp_eth))
        lac_tmp <-  lgdf_zonal_lac[[adm_tmp]]
        bind_rows(
          df_tmp,
          lac_tmp
        )
      }
)

# now write these out as w/ generic names
imap(
  lgdf_combined_zonal,\(df_tmp,nm_tmp){
    arrow::write_parquet(
      df_tmp,
      file.path("data",
                paste0("df_mars_zonal_",nm_tmp,".parquet")
                )
    )
  }
)


lgdf_eth <- read_rds("data/lgdf.rds")

lac_lgdf_fullsize <- lgdf_lac |>
  map(\(gdf_tmp){
    gdf_tmp |>
      rename_with(
        ~str_replace(.x,"_es","_en")
      ) |>
      dplyr::select(matches("adm\\d_[ep]"))
  }
  )
lac_lgdf_fullsize|>
  map(\(gdf_tmp){
    gdf_tmp |>
      object.size()*1e-6
  }
  )

# massively simplify
lac_lgdf_simp1 <- lac_lgdf_fullsize |>
  purrr::map(~ sf::st_simplify(.x,dTolerance = 1000) )

# check size of simplified - looks fine
lac_lgdf_simp1|>
  map(\(gdf_tmp){
    gdf_tmp |>
      object.size()*1e-6
  }
  )

# an manually check rendering
p_lac_lgdf_simp1 <- lac_lgdf_simp1 |>
  purrr::map(
    ~ ggplot2::ggplot()+
      ggplot2::geom_sf(data= .x)
  )

# add pcode to name
names(lac_lgdf_simp1) <- paste0(names(lac_lgdf_simp1),"_pcode")


lgdf_shapes <- map(  c(adm0_pcode="adm0_pcode",adm1_pcode="adm1_pcode",adm2_pcode="adm2_pcode"),
                             \(adm_tmp){
                               eth_gdf <- lgdf_eth[[adm_tmp]]
                               lac_gdf <-  lac_lgdf_simp1[[adm_tmp]]
                               bind_rows(
                                 eth_gdf,
                                 lac_gdf
                               )
                             }
)

lgdf_shapes$adm3_pcode <- lgdf_eth$adm3_pcode

lgdf_shapes |>
  map(
    ~object.size(.x)*1e-6
  )
write_rds(lgdf_shapes,file.path("data","lgdf_combined.rds"))

# lgdf_combined_zonal$adm0 |> count(adm0_en)
