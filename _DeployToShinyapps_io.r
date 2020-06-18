browseURL("https://www.youtube.com/watch?v=ZKWLfW4zBYs")
#install.packages("rsconnect")

# library("rsconnect")
# help(package = "rsconnect")

message("From within the server.r/app.r, click the blue publish to server 
        button next to the 'run app' button.")
message("Deselect 'www' and 'output' folders and attempt to deploy.") 
## Attempting to deploy will create a local rsconnect.
file.edit(file.path(".", "apps/study/app.r"))
## Error Spinifex v0.2.0 not available on Cran yet.

message("Ok, this won't work yet; lets try the PoC_webGL app;")
message("Deselect all except server.r and ui.r and attempt to deploy.")
file.edit(file.path(".", "../PoC_WebGL_shiny/_NicholasSpyrison_rgl/server.r"))
## Error Spinifex v0.2.0 not available on Cran yet.

message("Ok, this won't work yet; lets try the DuncanMurdoch_rgl;")
file.edit(file.path(".", "../PoC_WebGL_shiny/DuncanMurdoch_rgl/server.r"))
### MAKE SURE TO CALL library('rgl') within the ui.r as well.

message("Uploaded again after minor chnage and worked like a charm.")
browserURL("https://ebsmonash.shinyapps.io/DuncanMurdoch_rgl/")