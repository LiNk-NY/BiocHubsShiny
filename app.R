# Launch the ShinyApp (Do not remove this comment)

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

options(repos = BiocManager::repositories())

if (!requireNamespace("BiocHubsShiny", quietly = TRUE))
    BiocManager::install("BiocHubsShiny")

BiocHubsShiny::BiocHubsShiny() # add parameters here (if any)
