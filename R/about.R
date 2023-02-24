aboutPanel <- function() {
    sessionText <-
        if (requireNamespace("sessioninfo", quietly = TRUE))
            "sessioninfo::session_info()"
        else
            "utils::sessionInfo()"
    bioc_version <-
        if (requireNamespace("BiocManager", quietly = TRUE))
            as.character(BiocManager::version())
        else
            "version not available"
    pkgVer <- as.character(utils::packageVersion("BiocHubsShiny"))
    HTML(paste0(
        h4("BiocHubsShiny"),
        p("Package version: ", strong(pkgVer)),
        p("Bioconductor version: ", strong(bioc_version)),
        p("Last updated: ", strong("2022-02-07")),
        span("Source: ", a(
            "https://github.com/Bioconductor/BiocHubsShiny",
            href="https://github.com/Bioconductor/BiocHubsShiny"
        )),
        hr(),
        "<details style='margin-bottom:10px;'>", "<summary>",
        "&#9654; Session Info",
        "</summary>",
        "<pre class='r'><code>", sessionText,
        verbatimTextOutput("sessioninfo"),
        "</code></pre></details>"
    ))
}
