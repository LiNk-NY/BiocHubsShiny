aboutPanel <- function() {
    sessionText <-
        if (requireNamespace("sessioninfo", quietly = TRUE))
            "sessioninfo::session_info()"
        else
            "utils::sessionInfo()"
    tabPanel("About",
             HTML(paste0(
                 h4("shinyhubs"),
                 p("Package version: ", as.character(packageVersion("shinyhubs"))),
                 p("Last updated: 2022-02-04"),
                 span("Source: ", a(
                     "https://github.com/LiNk-NY/shinyhubs",
                     href="https://github.com/LiNk-NY/shinyhubs"
                 )),
                 hr(),
                 "<details style='margin-bottom:10px;'>", "<summary>",
                 "&#9654; Session Info",
                 "</summary>",
                 "<pre class='r'><code>", sessionText,
                 verbatimTextOutput("sessioninfo"),
                 "</code></pre></details>"
             )),
             value="about"
    )
}
