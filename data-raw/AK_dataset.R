## code to prepare Angrist-Krueger dataset

rm(list = ls())

AK <- readstata13::read.dta13("./data-raw/AK.dta")

usethis::use_data(AK, overwrite = TRUE)
