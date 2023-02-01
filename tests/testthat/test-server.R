testServer(
    app = BiocHubsShiny::BiocHubsShiny(),
    expr = {
        session$setInputs(hub = "AnnotationHub")
        expect_true(
            is(hub_data(), "AnnotationHub")
        )
        session$setInputs(hub = "ExperimentHub")
        expect_true(
            is(hub_data(), "ExperimentHub")
        )
    }
)
