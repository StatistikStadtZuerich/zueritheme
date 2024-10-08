# Example Dev Plots -------------------------------------------------------

set.seed(1313)

# Required Libraries ------------------------------------------------------

devtools::load_all()


# Color Palettes ----------------------------------------------------------

qual6 <- c("#3431DE", "#DB247D", "#1F9E31", "#FBB900", "#23C3F1", "#FF720C")
qual8 <- c("#3431DE", "#0A8DF6", "#23C3F1", "#7B4FB7", "#DB247D", "#FB737E", "#007C78", "#1F9E31")
greys <- c("#D6D6D6", "#7C7C7C")


# Example Data ------------------------------------------------------------

kat <- c("Car", "Bicycle", "Motorbike", "Boat", "Tram", "Bus", "Skateboard", "Airplane")
wert <- round(runif(length(kat), min = 0, max = 100), 1)
wert_y <- round(runif(length(kat), min = 0, max = 10000), 1)
facet <- c("motorized", "not motorized", "motorized", "motorized", "motorized", "motorized", "not motorized", "motorized")
dat_wert <- data.frame(kat, wert, wert_y, facet)


# Print -------------------------------------------------------------------

# Plot with grid lines x
ggplot(
  data = dat_wert,
  aes(x = wert, y = kat, fill = facet)
) +
  geom_col(width = 0.7) +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    x = "Proportion (in %)",
    y = "Vehicles",
    caption = "The numbers shown above are imaginary data."
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(qual6[1], qual6[4])) +
  ssz_theme(media = "print", grid_lines = "x")


# Plot with grid lines y
ggplot(
  data = dat_wert,
  aes(x = kat, y = wert, fill = facet)
) +
  geom_col(width = 0.7) +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    x = "Vehicles",
    y = "Proportion (in %)",
    caption = "The numbers shown above are imaginary data."
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(qual6[1], qual6[4])) +
  ssz_theme(media = "print", grid_lines = "y")

# Plots with grid lines both
ggplot(
  data = dat_wert,
  aes(x = wert, y = wert_y, color = facet)
) +
  geom_point(size = 3) +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    x = "Gallons per Mile",
    y = "Velocity",
    caption = "The numbers shown above are imaginary data."
  ) +
  scale_y_continuous(limits = c(0, max(dat_wert$wert_y) + 100)) +
  scale_color_manual(values = c(qual6[1], qual6[4])) +
  ssz_theme(media = "print", grid_lines = "both")

# Plots with grid lines none
ggplot(
  data = dat_wert,
  aes(x = wert, y = wert_y, color = facet)
) +
  geom_point(size = 3) +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    x = "Gallons per Mile",
    y = "Velocity",
    caption = "The numbers shown above are imaginary data."
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(dat_wert$wert_y) + 100)) +
  scale_color_manual(values = c(qual6[1], qual6[4])) +
  ssz_theme(media = "print", grid_lines = "none")

# Plot with no grid lines and no axis text (ssz_theme_void)
ggplot(
  dat_wert,
  aes(
    x = factor(1),
    y = kat,
    fill = factor(kat)
  )
) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = qual8) +
  ssz_theme_void() +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    caption = "The numbers shown above are imaginary data."
  )


# Web ---------------------------------------------------------------------

# Plot with grid lines x
g1 <- ggplot(
  data = dat_wert,
  aes(
    x = wert,
    y = kat,
    fill = facet,
    tooltip = glue("{kat}: {wert}")
  )
) +
  geom_col_interactive(aes(data_id = kat),
    width = 0.7
  ) +
  labs(
    title = "Vehicle Types in Narnia",
    subtitle = "Proportion of Vehicle Types",
    x = "Vehicle Type",
    y = "Proportion (in %)",
    caption = "Imaginary data represented here."
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_fill_manual(values = c(qual6[1], qual6[4])) +
  ssz_theme(media = "web", grid_lines = "x")

girafe(
  ggobj = g1,
  options = list(
    opts_tooltip(
      css = glue("padding:5pt;font-style:normal;font-size:0.75rem;color:white;background-color:{greys[[2]]}")
    ),
    opts_toolbar(
      saveaspng = FALSE,
      hidden = c("selection")
    ),
    opts_zoom(max = 1),
    opts_hover(
      css = girafe_css(
        css = glue("fill:{greys[[1]]};"),
      )
    )
  ),
  height = 4
)

# Plot with grid lines y
g2 <- ggplot(data = dat_wert, aes(
  x = kat,
  y = wert,
  fill = kat,
  tooltip = glue("{kat}: {wert}")
)) +
  geom_col_interactive(aes(data_id = kat),
    width = 0.7,
    show.legend = FALSE
  ) +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    x = "Vehicles",
    y = "Proportion (in %)",
    caption = "The numbers shown above are imaginary data."
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = qual8) +
  ssz_theme(media = "web")

girafe(
  ggobj = g2,
  options = list(
    opts_tooltip(
      css = glue("padding:5pt;font-style:normal;font-size:0.75rem;color:white;background-color:{greys[[2]]}")
    ),
    opts_toolbar(
      saveaspng = FALSE,
      hidden = c("selection")
    ),
    opts_zoom(max = 1),
    opts_hover(
      css = girafe_css(
        css = glue("fill:{greys[[1]]};"),
      )
    )
  ),
  height = 4
)

g3 <- ggplot(
  data = dat_wert,
  aes(
    x = wert,
    y = kat,
    fill = facet,
    tooltip = glue("{kat}: {wert}")
  )
) +
  geom_col_interactive(aes(data_id = kat),
    width = 0.7,
    show.legend = FALSE
  ) +
  labs(
    title = "Vehicle types used in the city of Zurich",
    subtitle = "by engine type",
    x = "Proportion (in %)",
    y = "Vehicles",
    caption = "The numbers shown above are fictious data."
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 101)
  ) +
  scale_fill_manual(values = c(qual6[1], qual6[4])) +
  ssz_theme(media = "web", grid_lines = "y")

girafe(
  ggobj = g3,
  options = list(
    opts_tooltip(
      css = glue("padding:5pt;font-style:normal;font-size:0.75rem;color:white;background-color:{greys[[2]]}")
    ),
    opts_toolbar(
      saveaspng = FALSE,
      hidden = c("selection")
    ),
    opts_zoom(max = 1),
    opts_hover(
      css = girafe_css(
        css = glue("fill:{greys[[1]]};"),
      )
    )
  ),
  height = 4
)

# Plot with no grid lines and no axis text (ssz_theme_void)
g4 <- ggplot(
  dat_wert,
  aes(
    x = factor(1),
    y = kat,
    fill = factor(kat),
    tooltip = glue("{kat}: {wert}")
  )
) +
  geom_col_interactive(aes(data_id = kat),
    width = 1
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = qual8) +
  ssz_theme_void() +
  labs(
    title = "Vehicle types used in Narnia",
    subtitle = "by engine type",
    caption = "The numbers shown above are imaginary data."
  )

girafe(
  ggobj = g4,
  options = list(
    opts_tooltip(
      css = glue("padding:5pt;font-style:normal;font-size:0.75rem;color:white;background-color:{greys[[2]]}")
    ),
    opts_toolbar(
      saveaspng = FALSE,
      hidden = c("selection")
    ),
    opts_zoom(max = 1),
    opts_hover(
      css = girafe_css(
        css = glue("fill:{greys[[1]]};"),
      )
    )
  ),
  height = 4
)
