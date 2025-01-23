.shinyAce_template <- function(hub, hubid = "click_a_row") {
    hubname <- dQuote(hub, FALSE)
    p0 <- paste0
    paste(
        "## Make sure BiocManager is installed",
        "if (!require('BiocManager', quietly = TRUE))",
        "    install.packages('BiocManager')\n",
        p0("## Make sure ", hub, " is installed"),
        p0("if (!require(", hubname, ", quietly = TRUE))"),
        p0("    BiocManager::install(", hubname, ")", "\n"),
        "## Use this code to download the resource",
        p0("library(", hubname, ")"),
        p0("hub <- ", hub, "()", "\n"),
        "## Select rows in the table",
        p0(p0("hub[['", hubid, "']]"), collapse = "\n"),
        sep = "\n"
    )
}

#' Initialize the shiny application for Bioconductor Hub resources
#'
#' @description
#' The shiny app will allow the user to view a table of either
#' [AnnotationHub] or [ExperimentHub] resources depending on the
#' sidebar selection. It provides example code for downloading the selected
#' resources.
#'
#' @details Note. The code here was adapted from `interactiveDisplayBase` and
#' `?'display,Hub-method'` which are now deprecated.
#'
#' @param ... Further arguments to the `runApp` function
#'
#' @md
#'
#' @return Mainly called for the side effect of displaying the shiny app in a
#'   browser
#'
#' @import shiny AnnotationHub ExperimentHub
#'
#' @examples
#' if (interactive()) {
#'     BiocHubsShiny()
#' }
#' @export
BiocHubsShiny <- function(...) {

    ui <- fluidPage(
        tags$head(
            tags$style(
                "#big-heading{color: #F15340; font-weight: bold;}",
                paste0(
                    "#snapshotdate{color: #87B13F; font-size:18px; ",
                    "font-style: italic;}"
                ),
                "#hubtitle{color: #076570; font-size:24px; font-weight: bold;}"
            )
        ),
        theme = shinythemes::shinytheme("simplex"),
        shinytoastr::useToastr(),
        # https://stackoverflow.com/questions/53616176/
        # shiny-use-validate-inside-downloadhandler
        shinyjs::useShinyjs(),
        titlePanel(
            windowTitle = "BiocHubsShiny",
            title = div(
                img(
                    src = system.file(
                        "images",
                        "bioconductor_logo_rgb_small.png",
                        package = "BiocHubsShiny"
                    ),
                    align = "right",
                    style = "margin-right:10px"
                ),
                h1(id = "big-heading", "Bioconductor *Hub Resources")
            )
        ),
        h3(
            "The online shop for AnnotationHub and ExperimentHub Data"
        ),
        br(),
        navbarPage(
            title = actionLink("sidebar_button", "", icon = icon("bars")),
            id = "navbarID",
            tabPanel(
                title = h4("Bioconductor Hub"),
                sidebarLayout(
                    div(class = "sidebar",
                        sidebarPanel(
                            radioButtons(
                                "hub",
                                label = h4(strong("Select A Bioconductor Hub")),
                                choices = c("AnnotationHub", "ExperimentHub")
                            ),
                            hr(),
                            h4(strong("Download")),
                            h5(strong("*Hub Resources")),
                            helpText(
                                "Select the rows of interest and then run the",
                                "code below the table within an R session."
                            ),
                            h5(strong("*Hub Metadata")),
                            helpText(
                                "Select rows and click 'Download metadata'"
                            ),
                            downloadButton(
                                "btnDown", "Download metadata"
                            ),
                            br(),
                            helpText(
                                "Click 'Send metadata' to interactively add ",
                                "selected rows to the current R session.",
                                "If viewing the app on a webpage, use the ",
                                "'Download metadata' button instead to obtain",
                                "an Rds of the selections."
                            ),
                            actionButton(
                                "btnSend", "Send metadata",
                                class = "btn-link"
                            ),
                            br(),
                            helpText(
                                strong("Tip"),
                                ": Use the search box at the top",
                                "right of the table to filter records."
                            ),
                            hr(),
                            actionButton(
                                "stopBtn", "Stop BiocHubsShiny",
                                class = "btn-primary"
                            ),
                            width = 2
                        )
                    ),
                    mainPanel(
                        h3(textOutput("hubtitle")),
                        textOutput("snapshotdate"),
                        hr(),
                        fluidRow(
                            DT::dataTableOutput('tbl')
                        ),
                        hr(),
                        fluidRow(
                            uiOutput("ace_input")
                        ),
                        width = 10
                    )
                )
            ),
            tabPanel(
                title = h4("About"),
                aboutPanel(),
                value = "about"
            )
        ) # end navbarPage
    ) # end fluidPage

    ## from interactiveDisplayBase:::.dataFrame3
    server <- function(input, output, session) {

        output$ace_input <- renderUI({
            shinyAce::aceEditor(
                outputId = "code",
                value = .shinyAce_template(hub = input$hub),
                height = "380px", fontSize = 18, mode = "r"
            )
        })
        # data retrieval, massaging
        hub_data <- reactive({
            if (identical(input$hub, "AnnotationHub")) {
                hub <- AnnotationHub::AnnotationHub(ask = FALSE)
            } else if (identical(input$hub, "ExperimentHub")) {
                hub <- ExperimentHub::ExperimentHub(ask = FALSE)
            }
            hub
        })
        hub_obj <- reactive({
            # let the user know that action is ongoing during loading
            shinytoastr::toastr_info(
                "retrieving *Hub data...", timeOut=3000
            )
            hub <- hub_data()
            md <- S4Vectors::mcols(hub)
            ans <- as.data.frame(md)
            ans <- as.data.frame(
                append(as.list(ans), list(HUBID = rownames(ans)), 0L),
                row.names = NULL
            )
            col_names <- c(
                "rdataclass", "rdatapath", "sourceurl",
                "sourcetype", "preparerclass"
            )
            ans <- ans[, -which(names(ans) %in% col_names)]
            ans$tags <- vapply(
                unname(unclass(md$tags)),
                base::paste,
                character(1), collapse = ", "
            )
            ans
        })
        hub_ss <- reactive({
            hub <- hub_data()
            AnnotationHub::snapshotDate(hub)
        })

        # table rendering
        output$tbl <- DT::renderDataTable(
            {
                shinytoastr::toastr_info(
                    "preparing *Hub data...", timeOut=4500
                )
                on.exit({
                    shinytoastr::toastr_info("done.", timeOut=2500)
                })
                hub_obj()
            },
            server = TRUE,
            rownames = FALSE,
            filter = "top",
            options = list(
                orderClasses = TRUE, pageLength = 6,
                columnDefs = list(list(
                    targets = c(2, 10),
                    render = htmlwidgets::JS(
                        "function(data, type, row) {",
                        "return type === 'display' && data.length > 10 ?",
                        paste0("'<span title=\"' + data + '\">' + ",
                        "data.substr(0, 10) + '...</span>' : data;"),
                        "}"
                    )
                ))
            )
        )

        # render title text
        output$hubtitle <- renderText({
            biochub <- hub_obj()
            nrec <- nrow(biochub)
            nspec <- length(unique(biochub[["species"]]))
            sprintf(
                paste(
                    "Search through %d", input$hub,
                    "resources from %d distinct species in Bioconductor"
                ),
                nrec, nspec
            )
        })

        output$snapshotdate <- renderText({
            sprintf(
                "Snapshot Date: %s",
                hub_ss()
            )
        })

        observeEvent(
            input$tbl_rows_selected,
            {
                idx <- input$tbl_rows_selected
                biochub <- hub_obj()
                ans <- biochub[idx, ]
                shinyAce::updateAceEditor(
                    session,
                    "code",
                    value = .shinyAce_template(
                        hub = input$hub, hubid = ans$HUBID
                    )
                )
            }
        )

        observeEvent(
            input$btnSend,
            {
                idx <- input$tbl_rows_selected
                biochub <- hub_obj()
                value <- biochub[idx, ]
                oname <- paste0(substr(tolower(input$hub), 1, 1), "h_meta")
                if (exists(oname))
                    warning("Overwriting existing '", oname, "'")
                message("Setting '", oname, "' in .GlobalEnv")
                cat(
                    "To save as Rds, run:\n",
                    "  saveRDS(", oname, ", file = '", oname, ".Rds')\n",
                    sep = ""
                )
                if (!identical(unname(Sys.info()["nodename"]), "shiny"))
                    assign(oname, value, envir = .GlobalEnv)
            }
        )

        output$btnDown <- downloadHandler(
            filename = function() {
                fprefix <- paste0(input$hub, "_meta_sel_")
                paste0(
                    fprefix, format(Sys.time(), "%F_%Hh%Mm"), ".Rds"
                )
            },
            content = function(con) {
                idx <- input$tbl_rows_selected
                biochub <- hub_obj()
                ans <- biochub[idx, ]
                saveRDS(ans, file=con)
            },
            contentType="application/octet-stream"
        )

        observeEvent(
            input$stopBtn,
            {
                # could return information here
                stopApp(returnValue=NULL)
            }
        )

        observeEvent(input$sidebar_button,{
            shinyjs::toggle(selector = ".sidebar")
        })

        output$sessioninfo <- renderPrint({
            if (requireNamespace("sessioninfo", quietly = TRUE))
                utils::capture.output(sessioninfo::session_info())
            else
                utils::capture.output(utils::sessionInfo())
        })
    }

    shinyApp(ui, server, ...)
}
