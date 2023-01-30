library(shinytest2)
## shinytest2::record_test()

test_that("{shinytest2} recording: BiocHubsShiny-EH", {
    app <- AppDriver$new(
        variant = platform_variant(),
        name = "BiocHubsShiny-EH",
        height = 1303,
        width = 2259
    )
    app$set_inputs(hub = "ExperimentHub")
    app$expect_screenshot(delay = 30)
})

test_that("{shinytest2} recording: BiocHubsShiny-about", {
    app <- AppDriver$new(
        variant = platform_variant(),
        name = "BiocHubsShiny-about",
        height = 1256, width = 2259
    )
    app$set_inputs(navbarID = "about")
    app$expect_screenshot()
})

test_that("{shinytest2} recording: BiocHubsShiny-selection", {
    app <- AppDriver$new(
        variant = platform_variant(),
        name = "BiocHubsShiny-selection",
        height = 1256, width = 2259
    )
    app$set_inputs(tbl_rows_selected = 1, allow_no_input_binding_ = TRUE)
    app$set_inputs(
        tbl_cell_clicked = c("1", "0", "AH5012"),
        allow_no_input_binding_ = TRUE,
        priority_ = "event"
    )
    app$set_inputs(tbl_rows_selected = c(1, 2), allow_no_input_binding_ = TRUE)
    app$set_inputs(
        tbl_cell_clicked = c("2", "0", "AH5013"),
        allow_no_input_binding_ = TRUE,
        priority_ = "event"
    )
    app$set_inputs(
        tbl_rows_selected = c(1, 2, 3), allow_no_input_binding_ = TRUE
    )
    app$set_inputs(
        tbl_cell_clicked = c("3", "0", "AH5014"),
        allow_no_input_binding_ = TRUE,
        priority_ = "event"
    )
    app$wait_for_idle()
    app$expect_screenshot(delay = 30)
})
