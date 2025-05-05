test_that("ssz_theme returns a ggplot theme", {
	expect_s3_class(ssz_theme(grid_lines = "x", publication_type = "print"),
									c("theme", "gg"))
})

test_that("ssz_theme returns errors if incorrect parameters specified", {
	expect_error(ssz_theme(grid_lines = "a"))
	expect_error(ssz_theme(publication_type = "something"))
	expect_error(ssz_theme(grid_lines = "both", publication_type = "web"))
})
