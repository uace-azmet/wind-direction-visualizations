df <- azmetr::az_daily(station_id = "az01", start_date = "2022-12-01", end_date = "2023-02-28")

# Search: r graphics wind rose ggplot
# Search: r graphics wind rose plotly
# https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
# https://rstudio-pubs-static.s3.amazonaws.com/284981_2e1c4c62b74446008d7ca449b744f7ab.html
# https://gist.github.com/eliocamp/20cc47b2a4b39af602a57f1e2cdd437f
# https://www.kuan-liu.com/posts/2022/02/creating-static-and-interactive-nightingale-rose-diagram-using-ggplot-and-plotly-in-r/

df <- df %>%
  dplyr::select(meta_station_name, wind_vector_dir)

