## code to prepare `DATASET` dataset goes here

# usethis::use_data(DATASET, overwrite = TRUE,)

migrate_eth_parquets <-  function(path){
  ldf <- readr::read_rds(path)
  purrr::imap(
    ldf,
    \(dft, dft_name){
      dft |>
        arrow::write_parquet(
          file.path(
            "data",
            glue::glue("df_eth_mars_zonal_{dft_name}.parquet")
          )

        )
    }

  )

}


migrate_eth_parquets(path = "../ds-aa-eth-drought/_targets/objects/ldf_ecmwf_zonal")

df <- arrow::read_parquet("data/df_eth_mars_zonal_adm0.parquet")


dir("../ds-aa-eth-drought/_targets/objects/")


CODAB_FP = file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public","raw","eth","cod_ab",
  "eth_adm_csa_bofedb_2021_shp"
)
sf::st_layers(CODAB_FP)

lyr_names <- c(
  "eth_admbnda_adm0_csa_bofedb_itos_2021",
  "eth_admbnda_adm1_csa_bofedb_2021",
  "eth_admbnda_adm2_csa_bofedb_2021",
  "eth_admbnda_adm3_csa_bofedb_2021")

lyr_list_names <- paste0(stringr::str_extract(lyr_names,"adm\\d"),"_pcode")
lgdf_full_size <- purrr::map(
  rlang::set_names(
    lyr_names,
    lyr_list_names
  ),\(lyr_nm_tmp){
  sf::st_read(CODAB_FP,lyr_nm_tmp) |>
      janitor::clean_names() |>
      dplyr::select(matches("adm\\d_[ep]"))

  }
)
lgdf_full_size |>
  purrr::map(~object.size(.x)*1e-6)

lgdf_full_size$adm3_pcode <- sf::st_cast(lgdf_full_size$adm3_pcode,"POLYGON")
# lgdf_full_size$adm3_pcode <- lgdf_full_size$adm3_pcode[!sf::st_is_empty(lgdf_full_size$adm3_pcode),]

lgdf_simp1 <- lgdf_full_size |>
  purrr::map(~ sf::st_simplify(.x,dTolerance = 1000) )

lgdf_simp1 |>
  purrr::map(~object.size(.x)*1e-6)

p_lgdf_simp1 <- lgdf_simp1 |>
  purrr::map(
   ~ ggplot2::ggplot()+
     ggplot2::geom_sf(data= .x)
  )

lgdf_simp1$adm3_pcode <- sf::st_cast(lgdf_simp1$adm3_pcode,"POLYGON")


lgdf_simp1$adm3_pcode <- lgdf_simp1$adm3_pcode[!sf::st_is_empty(lgdf_simp1$adm3_pcode),]

#
# lgdf_simp1$adm3_pcode |>
#   filter(!sf::st_is_empty(.))

lgdf_simp1 |>
  readr::write_rds(
    file.path("data",
              "lgdf.rds")
  )



