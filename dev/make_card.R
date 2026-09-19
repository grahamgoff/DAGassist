# Builds the social preview card for the pkgdown site (1200 x 630 px).
# Run from the package root: source("dev/make_card.R")
library(grid)

W <- 1200; H <- 630          # card size in px
gap   <- 70                  # space between hex and text
hex_h <- 440                 # hex height in px

bg  <- "#FEE5D0"
red <- "#AC4848"
ink <- "#2B2424"

logo <- png::readPNG("man/art/DAGassist.png")
# crop the transparent padding so the hex itself (not the canvas) gets centred
opaque <- logo[, , 4] > 0
rows <- range(which(rowSums(opaque) > 20))
cols <- range(which(colSums(opaque) > 20))
logo <- logo[rows[1]:rows[2], cols[1]:cols[2], ]
hex_w <- hex_h * ncol(logo) / nrow(logo)

png("man/figures/card.png", width = W, height = H, res = 144)
grid.newpage()
pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))
grid.rect(gp = gpar(fill = bg, col = NA))

# text grobs (built first so we can measure them)
title   <- textGrob("DAGassist", just = c("left", "bottom"),
                    gp = gpar(col = red, fontsize = 46, fontface = "bold"))
tagline <- textGrob("Align your regressions with\nthe estimands they target",
                    just = c("left", "top"),
                    gp = gpar(col = ink, fontsize = 21, lineheight = 1.2))
url     <- textGrob("grahamgoff.com/DAGassist", just = c("left", "top"),
                    gp = gpar(col = red, fontsize = 13))

w <- function(g) convertWidth(grobWidth(g), "native", valueOnly = TRUE)
h <- function(g) convertHeight(grobHeight(g), "native", valueOnly = TRUE)

# centre the whole group (hex + gap + text) horizontally
text_w <- max(w(title), w(tagline), w(url))
left   <- (W - (hex_w + gap + text_w)) / 2
hex_cx <- left + hex_w / 2
text_x <- left + hex_w + gap

# stack the text block and centre it vertically on the hex
tag_gap <- 40; url_gap <- 46
block_h <- h(title) + tag_gap + h(tagline) + url_gap + h(url)
top     <- H / 2 + block_h / 2

grid.raster(logo, x = unit(hex_cx, "native"), y = unit(H / 2, "native"),
            height = unit(hex_h, "native"))

y <- top - h(title)
grid.draw(editGrob(title, x = unit(text_x, "native"), y = unit(y, "native")))

y <- y - tag_gap
grid.draw(editGrob(tagline, x = unit(text_x, "native"), y = unit(y, "native")))

y <- y - h(tagline) - url_gap
grid.draw(editGrob(url, x = unit(text_x, "native"), y = unit(y, "native")))

dev.off()
message(sprintf("side margins: %.0fpx", left))