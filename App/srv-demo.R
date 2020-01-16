observeEvent(input$tour, {
  switch (input$tabs,
    "subitem1" = introjs(
      session,
      options = list(
        steps = list(
          list(
            element = "#data_1",
            intro = "This tab shows the aggregate results of the model predictions.  These outputs are displaying a summarization of
            all of the predicted claim amounts combined",
            position = "left"
          ),
          
          list(
            element = "#prueba123",
            intro = "This tab shows the aggregate results of the model predictions.  These outputs are displaying a summarization of
            all of the predicted claim amounts combined",
            position = "left"
          ),
          
          list(
            element = "#prueba1234",
            intro = "This tab shows the aggregate results of the model predictions.  These outputs are displaying a summarization of
            all of the predicted claim amounts combined",
            position = "left"
          )
        )
      )
    )
)
})







