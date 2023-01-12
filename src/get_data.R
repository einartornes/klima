# Scriptet laster ned datakilder som brukes i appen fra noradstats google drive
# Scriptet sources inn i scriptet app.R, og trenger dermed ikke å kjøres separat
# Datakildene lastes ned til undermappen ./data

# Loading packages
library(googledrive)
library(purrr)
library(here)

# Connecting to noradstats google drive
googledrive::drive_auth(email = "noradstats@gmail.com")

# Find available files
googledrive::drive_find()

# Selecting files to download
files <- c("statsys_ten.csv","imputed_multilateral_shares_climate.xlsx",
           "norfund_capitalisation_agreements.xlsx")

# Filepaths to subfolder data/
paths <- paste0(here("data", files))

# Create data-folder if not present.
if(file.exists(here("data")) == FALSE) {
  dir.create(here("data"))
}

#file.exists(here("data", files))

# Download selected files to data-folder using purrr
map2(.x = files, .y = paths, ~ googledrive::drive_download(file = .x,
                                                           path = .y,
                                                           overwrite = TRUE))
