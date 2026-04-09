# BCAQO returns expected output

    Code
      output
    Output
      # A tibble: 4 x 10
         year pm25_daily_mean_annual_9~1 pm25_daily_mean_annu~2 pm10_daily_mean_annu~3
        <dbl>                      <dbl>                  <dbl>                  <dbl>
      1  2020                         50                     50                    100
      2  2021                         50                     50                    100
      3  2022                         50                     50                    100
      4  2023                         50                     50                    100
      # i abbreviated names: 1: pm25_daily_mean_annual_98th,
      #   2: pm25_daily_mean_annual_mean, 3: pm10_daily_mean_annual_mean
      # i 6 more variables: o3_8hr_mean_daily_max_annual_4th_highest <dbl>,
      #   no2_daily_max_annual_98th <dbl>, so2_daily_max_annual_99th <dbl>,
      #   no2_annual_mean <dbl>, so2_annual_mean <dbl>, attainment <tibble[,8]>

---

    Code
      output$attainment
    Output
      # A tibble: 4 x 8
        pm25_daily_mean_annual_98th pm25_daily_mean_annual_mean pm10_daily_mean_annu~1
        <lgl>                       <lgl>                       <lgl>                 
      1 TRUE                        TRUE                        TRUE                  
      2 TRUE                        TRUE                        TRUE                  
      3 TRUE                        TRUE                        TRUE                  
      4 TRUE                        TRUE                        TRUE                  
      # i abbreviated name: 1: pm10_daily_mean_annual_mean
      # i 5 more variables: o3_8hr_mean_daily_max_annual_4th_highest <lgl>,
      #   no2_daily_max_annual_98th <lgl>, no2_annual_mean <lgl>,
      #   so2_daily_max_annual_99th <lgl>, so2_annual_mean <lgl>

