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
    app$expect_screenshot(delay = 10)
})

test_that("{shinytest2} recording: BiocHubsShiny-about", {
    app <- AppDriver$new(variant = platform_variant(), name = "BiocHubsShiny-about",
        height = 1256, width = 2259)
    app$set_inputs(navbarID = "about")
    app$expect_screenshot()
})

test_that("{shinytest2} recording: BiocHubsShiny-selection", {
    app <- AppDriver$new(variant = platform_variant(), name = "BiocHubsShiny-selection",
        height = 1256, width = 2259)
    app$set_inputs(code = "## Make sure BiocManager is installed\nif (!require('BiocManager', quietly = TRUE))\n    install.packages('BiocManager')\n\n## Make sure AnnotationHub is installed\nif (!require(\"AnnotationHub\", quietly = TRUE))\n    BiocManager::install(\"AnnotationHub\")\n\n## Use this code to download the resource\nlibrary(\"AnnotationHub\")\nhub <- AnnotationHub()\n\n## Select rows in the table\nhub[['AH5012']]\nhub[['AH5013']]\nhub[['AH5014']]")
    app$expect_screenshot(delay = 10)
})
