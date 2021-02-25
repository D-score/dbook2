# automatically run before each chapter

# load standard packages
pkg <- c("knitr", "kableExtra", "officedown", "officer", "flextable",  "ggplot2", "tidyr", "dplyr", "tibble",
         "maps", "ggthemes", "gridExtra", "RColorBrewer", "mice",
          "dscore", "dmetric", "gseddata", "ddata",
         "dmodel", "ddomain", "dinstrument")
loaded <- sapply(pkg, require, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE)

# transparent background
# https://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2
# https://gist.github.com/cboettig/5600558
theme_set(theme_light())
theme_update(panel.background = element_rect(fill = "transparent", colour = NA),
             plot.background = element_rect(fill = "transparent", colour = NA),
             legend.key = element_blank(),
             rect = element_rect(fill = "transparent") # all rectangles
)
opts_chunk$set(dev.args = list(png = list(bg = "transparent")),
               fig.cap = TRUE, tab.cap.sep = ". ", fig.cap.sep = ". ",
               dpi = 288)
