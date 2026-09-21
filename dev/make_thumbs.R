# Normalizes README gallery thumbnails to one canvas size.
# Run from the package root: source("dev/make_thumbs.R")
library(magick)

W <- 1000; H <- 750            # 4:3 canvas for every tile

shots <- c(
  latex      = "dev/gallery/latex.png",
  word       = "dev/gallery/word.png",
  excel      = "dev/gallery/excel.png",
  dotwhisker = "dev/gallery/dotwhisker.png"
)

for (nm in names(shots)) {
  image_read(shots[[nm]]) |>
    image_trim(fuzz = 2) |>
    image_border("white", "16x16") |>
    image_resize(sprintf("%dx", W)) |>          # width-fill, aspect preserved
    image_write(sprintf("man/figures/README-%s.png", nm))
}


