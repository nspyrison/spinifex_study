##### googledrive -----
# https://googledrive.tidyverse.org/

# install.packages("googledrive")
library("googledrive")
library("dplyr")

(my_drive_sheets <- drive_find(type = "spreadsheet", ))

(tgt <- drive_download("TEST_gsheet", type = "csv"))
str(tgt$drive_resource)

sys_info <- as_tibble(t(Sys.info()[1:4]))
df <- tibble(sys_info, ff_text = "Hello google sheets!")
df

googledrive::drive_upload()



#### googlesheets -----
# https://github.com/jennybc/googlesheets
# https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/

# install.packages("googlesheets")
library("googlesheets") ## app has not verified by google yet; looks like an api issue with the package.
# library("dplyr")
# gs_ls()

warning(
  paste0("Oauth app not working, oauth credentials, or ",
         "a better solution may be to go with the `googledrive` package.")
)






