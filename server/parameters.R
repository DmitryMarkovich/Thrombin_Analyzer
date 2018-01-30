################################################################################
################################################################################
######################################## Parameters tab
################################################################################
################################################################################

output$cal.ShowParms <- renderTable({
    if (Cal$model_fitted) {
        cal$parms_model(input$cal.model, input$cal.e0, input$cal.s0);
    } else {
        NULL;
    }
}, digits = 6)  ## End of output$cal.ShowParms
output$tg.ShowParms <- renderTable({
    if (TG$model_fitted) {
        tg$parms_model(input$tg.model, input$cal.CF);
    }
}, digits = 3)  ## End of output$tg.ShowParms
output$tg.ShowParmsNum <- renderTable({
    if (TG$data_loaded) {
        return(tg$num_parms());
    }
}, digits = 3)  ## End of output$tg.ShowParmsNum
