### Expample dev plots

## Libraries
library(ggplot2)
library(dplyr)
library(stringi)

## Data
kat <- c("Car", "Bicycle", "Motorbike", "Boat", "Tram", "Bus", "Skateboard", "Airplane")
wert <- runif(length(kat), min = 0, max = 100)
wert_y <- runif(length(kat), min = 0, max = 10000)
facet <- c("motorized", "not motorized", "motorized", "motorized", "motorized", "motorized", "not motorized", "motorized")
dat_wert <- data.frame(kat, wert, wert_y, facet)

# Plot with grid lines x
ggplot(data = dat_wert,
			 aes(x = wert, y = kat, fill = facet)) +
	geom_bar(stat = "identity") +
	labs(title = "Vehicle types used in the city of Zurich",
			 subtitle = "by engine type",
			 x = "Proportion (in %)",
			 y = "Vehicles",
			 caption = "The numbers shown above are fictious data.") +
	facet_wrap(~ facet, scales = "free") +
	scale_x_continuous(expand = c(0,0)) +
	ssz_theme(grid_lines = "x")

# Plot with grid lines y
ggplot(data = dat_wert,
			 aes(x = kat, y = wert, fill = kat)) +
	geom_bar(stat = "identity") +
	labs(title = "Vehicle types used in the city of Zurich",
			 subtitle = "by engine type",
			 x = "Vehicles",
			 y = "Proportion (in %)",
			 caption = "The numbers shown above are fictious data.") +
	scale_y_continuous(expand = c(0,0)) +
	ssz_theme(grid_lines = "y")

# Plots with grid lines both
ggplot(data = dat_wert,
			 aes(x = wert, y = wert_y, color = kat)) +
	geom_point() +
	labs(title = "Vehicle types used in the city of Zurich",
			 subtitle = "by engine type",
			 x = "Gallons per Mile",
			 y = "Velocity",
			 caption = "The numbers shown above are fictious data.") +
	scale_y_continuous(expand = c(0,0), limits = c(0, max(dat_wert$wert_y)+ 100)) +
	facet_wrap(~ kat) +
	ssz_theme(grid_lines = "both")

# Plots with grid lines none
ggplot(data = dat_wert,
			 aes(x = wert, y = wert_y, color = kat)) +
	geom_point() +
	labs(title = "Vehicle types used in the city of Zurich",
			 subtitle = "by engine type",
			 x = "Gallons per Mile",
			 y = "Velocity",
			 caption = "The numbers shown above are fictious data.") +
	scale_y_continuous(expand = c(0,0), limits = c(0, max(dat_wert$wert_y)+ 100)) +
	ssz_theme(grid_lines = "none")

# Plot with no grid lines (ssz_theme_void)
ggplot(dat_wert,
			 aes(x = factor(1),
			 		fill = factor(kat))) +
	geom_bar(width = 1) +
	coord_polar(theta = "y") +
	ssz_theme_void() +
	labs(title = "Vehicle types used in the city of Zurich",
			 subtitle = "by engine type",
			 caption = "The numbers shown above are fictious data.")
