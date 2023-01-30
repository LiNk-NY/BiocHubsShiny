testServer(expr = {
    session$setInputs(hub = "AnnotationHub")
    expect_true(
        is(hub_data(), "AnnotationHub")
    )
})
