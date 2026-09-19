# Builds the social preview card for the pkgdown site (1200 x 630 px).
# Run from the package root: source("dev/make_card.R")
library(grid)

logo <- png::readPNG("man/art/DAGassist.png")

png("man/figures/card.png", width = 1200, height = 630, res = 144)
grid.newpage()

# background
grid.rect(gp = gpar(fill = "#00612E", col = NA))

# hex logo, left side
grid.raster(logo, x = unit(0.20, "npc"), y = 0.5, height = unit(0.70, "npc"))

# title + pitch, right side
grid.text("DAGassist", x = 0.42, y = 0.64, just = "left",
          gp = gpar(col = "white", fontsize = 40, fontface = "bold"))
grid.text("DAG-driven robustness checks\nfor regression models in R",
          x = 0.42, y = 0.42, just = "left",
          gp = gpar(col = "white", fontsize = 18, lineheight = 1.15))
grid.text("grahamgoff.com/DAGassist", x = 0.42, y = 0.17, just = "left",
          gp = gpar(col = "#CFE8D8", fontsize = 12))

dev.off()