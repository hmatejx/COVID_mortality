require(rprojroot)


root <- rprojroot::has_file(".git/index")


download <- function(url, path, filename) {
  tmp <- tempfile()
  res <- download.file(url, tmp)
  if (res == 0) {
    destfile <- root$find_file(path, filename)
    if (file.exists(destfile)) {
      file.remove(destfile)
    }
    file.rename(tmp, destfile)
  }
}


# Get excess mortality data
download("https://github.com/dkobak/excess-mortality/raw/main/excess-mortality-timeseries.csv",
         "data_raw",
         "excess-mortality-timeseries.csv")


# Get COVID-19 data
download("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv",
         "data_raw",
         "owid-covid-data.csv")


# Get COVID-19 stringency index
# download manually from here: https://ourworldindata.org/grapher/covid-stringency-index
# go to the "DOWNLOAD" tab and click the blue "covid-stringency-index.csv" button
