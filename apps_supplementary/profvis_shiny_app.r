require(profvis)
require(shiny)

pv <- profvis({
  runApp("apps/spinifex_study")
})
str(pv)
print(pv)
profvis_obj <- pv
saveRDS(profvis_obj, "profvis_obj.rds")
