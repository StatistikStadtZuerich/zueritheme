### Test Plots

##
library(ggplot2)

## Data
kat <- c("Autos", "Velos", "Flugis", "Rollerblades", "Skateboards", "Schiffe", "Pferde", "Jetskis", "Raumschiffe")
wert <- runif(length(kat), min = 0, max = 100)
facet <- c("Motor", "kein Motor", "Motor", "kein Motor", "kein Motor", "Motor", "PS", "Motor", "Motor")
dat <- data.frame(kat, wert, facet)

x_wert <- runif(1000, min = 0, max = 100000)
y_wert <- runif(1000, min = 0, max = 100000)
dat_wert <- data.frame(x_wert, y_wert)

# Plot with grid lines x
ggplot(data = dat,
			 aes(x = wert, y = kat, fill = facet)) +
	geom_bar(stat = "identity") +
	labs(title = "Anteil in der Stadt Zürich genutzten Fahrzeuge",
			 subtitle = "nach Antriebsart",
			 x = "Anteil (in %)",
			 y = "Fahrzeugtypen",
			 caption = "Dies ist eine Caption, mit wichtigen Informationen über die Grafik.") +
	facet_wrap(~ facet) +
	scale_x_continuous(expand = c(0,0)) +
	ssz_theme(grid_lines = "x")

# Plot with grid lines y
ggplot(data = dat,
			 aes(x = kat, y = wert, fill = kat)) +
	geom_bar(stat = "identity") +
	labs(title = "Das ist ein Titel",
			 subtitle = "Und hier kommt ein Untertitel",
			 x = "Fahrzeugtypen",
			 y = "Anteil (in %)",
			 caption = "Dies ist eine Caption") +
	scale_y_continuous(expand = c(0,0)) +
	ssz_theme(grid_lines = "y")

# Plots with grid lines both
ggplot(data = dat_wert,
			 aes(x = x_wert, y = y_wert)) +
	geom_point() +
	labs(title = "Das ist ein Titel",
			 subtitle = "Und hier kommt ein Untertitel",
			 x = "Fahrzeugtypen",
			 y = "Anteil (in %)",
			 caption = "Dies ist eine Caption") +
	scale_y_continuous(expand = c(0,0)) +
	ssz_theme(grid_lines = "both")
