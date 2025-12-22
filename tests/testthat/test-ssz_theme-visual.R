# R CMD Check in devtools::check()
is_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))

test_that("ssz_theme print y gridlines looks correct", {

	# Do not run tests in devtools::check()
	skip_if_not_installed("vdiffr")
	skip_if(is_check)

	p <- ggplot(
		data = iris,
		aes(x = Sepal.Length, y = Sepal.Width, color = Species)
	) +
		geom_point() +
		scale_y_continuous(expand = c(0, 0)) +
		ssz_theme(
			publication_type = "print",
			grid_lines = "y",
			base_family = "sans"
		)

	vdiffr::expect_doppelganger(
		title = "print theme with y gridlines",
		fig = p
	)
})
