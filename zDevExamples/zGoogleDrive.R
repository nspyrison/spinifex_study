message("Use `googledrive` it is build on the current api 4.0. don't use Jenny's `googlesheets`, uses outdated API.")
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




