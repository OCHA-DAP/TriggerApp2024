
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TriggerApp2024

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

TriggerApp2024 is an exploratory AA drought trigger app built in R with
`{shiny}` & `{golem}`.

This repo/version of app is more of a proof of concept and is not yet
deployed as it will require a more robust software stack to deploy.

## Data Sources

- ECMWF Seasonal Forecasts (SEAS5) aggregated to admin boundaries of
  several countries

## Countries included (so far)

- Afghanistan (adm 0-2)
- Ethiopia (admin 0-3)
- Guatemala (admin 0-2)
- Nicaragua (admin 0-2)
- Honduras (admin 0-2)
- El Salvador (admin (0-2)

## Installation

You can install the development version of TriggerApp2024 like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Dev Information

- Currently using local data files (mostly parquets) located in the
  `.data-scrap` folder. These need to be shared in order for other
  analysts to work in current version.
- From what I understand `{golem}` expects data to be stored as `.rda`
  files as a normal package would. I tried this, but the load time for
  the app when using `golem::run_dev()` was frustratingly slow so I
  switched back to parquets.
- As this is more of a proof of concept I have not spent a huge amount
  of time checking the proper storage solution yet as I think alot will
  change as we think about how to properly deploy the app with our
  current and future available tech stack. Therefore, so far this repo
  is focused on getting the desired functionality using a couple local
  data sets.

## Known Issues

- App freezes if a gap becomes present in valid month check box. For
  example if you check `May` and `July` but not `June`. I am considering
  testing out using a slider widget rather than `checkboxGroupButtons`
  for valid months to avoid this.
- Several issues where old values from map stay in background when
  selecting a new variable - should be easily fixable.

## Thoughts/notes on future of deployment/data storage

- **Deployment:** based on current stack we will probably want to deploy
  as Azure web app.
  - unlike python Azure Web Apps do not natively render R and therefore
    require a deployment through a dockerized container
- **Data storage:** it seems likely that we will want to access data
  stored on our blob storage or potentially bundle this data in the
  docker container required. I am not sure of the advantageous of
  either, but given that we will eventually want to scale this app
  globally the data size will be significant. Access via `duckdb` seems
  like a good and promising option in either case.
