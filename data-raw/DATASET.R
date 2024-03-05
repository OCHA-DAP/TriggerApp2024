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
