library(asciicast)
library(magick)
source("rmd_to_df.R")
file = "test.Rmd"
stub = sub("[.][R|r]md", "", file)
output_format = rmarkdown::html_document(self_contained = FALSE)
# rmarkdown::render(input = file, output_format = output_format)
x = rmd_to_df(file, execute = TRUE, output_format = output_format)

code = x$code[x$echo & x$include & x$eval]

# script = tempfile(fileext = ".R")
# writeLines(code, script)
# out = asciicast::record(script, typing_speed = 0.12)


process <- asciicast_start_process()
casts = lapply(code, function(r) {
  out = record(textConnection(r), typing_speed = 0.12, process = process)
  Sys.sleep(0.5)
  out
})
process$kill()

outcasts = sapply(casts, function(cast) {
  svg = tempfile(fileext = ".cast")
  asciicast::write_json(cast, svg)
  svg
})

gifs = sapply(casts, function(cast) {
  svg = tempfile(fileext = ".cast")
  asciicast::write_json(cast, svg)
  gif = tempfile(fileext = ".gif")
  system2("asciicast2gif", c(svg, gif))
  gif
})



o1 = image_read_svg(svgs[[1]])
tgif = tempfile(fileext = ".gif")
magick::image_write_gif(o1, path = tgif)
o2 = image_read(svgs[[1]])

