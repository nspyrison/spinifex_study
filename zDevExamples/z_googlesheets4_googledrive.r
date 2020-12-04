#### Notes for google drive auth setup.
## Following: https://googlesheets4.tidyverse.org/articles/articles/drive-and-sheets.html
require(googledrive)
require(googlesheets4)

drive_auth(email = "nicholas.spyrison@monash.edu") ## I think this is sufficient for the app too. otherwise see below.
if(F){
  require(googledrive)
  require(googlesheets4)
  options(gargle_oauth_email = "nicholas.spyrison@monash.edu") ## for gs4 auth
  gs4_auth(email = "nicholas.spyrison@monash.edu") ## I think
  token <- environment()
  saveRDS(token, "./apps/study/www/auth_env.rds")
}

## List all spreadsheets
(my_drive_sheets <- drive_find(type = "spreadsheet"))

## Download a specific 'dribble'
(tgt <- drive_download("TEST_gsheet", type = "csv"))
str(tgt$drive_resource)
str(tgt)

## make a dummy df
sys_info <- as_tibble(t(Sys.info()[1:4]))
df <- tibble(sys_info, ff_text = "Hello google sheets!")
df

## New methods, {googlesheets4}: ------
?sheet_append() ## EXPECTS A DF, not a vector
## Example ##
## Creates a new doc.
ss <- gs4_create("sheet-append-demo", sheets = list(deaths = deaths_one)) ## returns IDof the sheet
ss %>% sheet_append(deaths_two) ## appends a row.


## Previous method, {googledrive} ------
?googledrive::drive_upload() ## {googledrive} update a google doc
## for example:
## convert to google sheet csv file
chicken_sheet <- drive_upload(
  drive_example("chicken.csv"),
  name = "chicken-sheet-upload.csv",
  type = "spreadsheet"
)

## check out the new Sheet! (open in a browser)
drive_browse(chicken_sheet)

## clean-up
drive_find("chicken.*upload") %>% drive_rm()

## Upload a file and, at the same time, star it
chicken <- drive_upload(
  drive_example("chicken.jpg"),
  starred = "true"
)


## clean up, googelsheets4 -----
gs4_find("gs4-create-demo") %>%
  googledrive::drive_trash()