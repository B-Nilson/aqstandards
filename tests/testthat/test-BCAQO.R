
# TODO: proper test case
test_that("BCAQO returns expected output", {
  obs <- data.frame(
    date = seq(
      lubridate::ymd_h("2020-01-01 00"),
      lubridate::ymd_h("2023-12-31 23"),
      "1 hours"
    ),
    pm10 = 100,
    pm25 = 50,
    o3 = 15,
    no2 = 1,
    so2 = 30
  )

  output <- BCAQO(
    dates = obs$date,
    pm25_ugm3 = obs$pm25,
    pm10_ugm3 = obs$pm10,
    no2_ppb = obs$no2,
    o3_ppb = obs$o3,
    so2_ppb = obs$so2
  )
  expect_snapshot(output)
  expect_snapshot(output$attainment)
})
