################################################################################
################################################################################
######################################## Login
################################################################################
################################################################################

USER <- reactiveValues(Logged = FALSE, Login = FALSE, Logout = FALSE,
                       UsernameError = FALSE, PasswordError = FALSE,
                       Selection = NULL);

observe({  ## shows the login page login_ui() or app page app_ui()
    if (USER$Logged == FALSE) {
        output$page <- renderUI({
            div(class = "outer",
                do.call(bootstrapPage, c("", login_ui())))
        })
    }
    if (USER$Logged == TRUE) {
        output$page <- renderUI({
            app_ui();
        })
    }
})  ## End of observe login_ui() or app_ui()

observeEvent(  ## tracks input$Login
    eventExpr = input$Login, handlerExpr = {
        if (!USER$Login)
            USER$Login <- TRUE;
    })  ## End of observeEvent input$Login

output$UsernameError <- renderUI({
    if (USER$UsernameError) {
        HTML(">> Username is incorrect (try 'guest' without password)!");
    }
})  ## End of output$UsernameError

output$PasswordError <- renderUI({
    if (USER$PasswordError) {
        HTML(">> Password is incorrect!");
    }
})  ## End of output$PasswordError

observe({  ## logs the user in
    if (USER$Logged == FALSE) {
        if (USER$Login) {
            if (any(input$Username == users$Username)) {
                USER$UsernameError <- FALSE;
                if (users$Password[users$Username == input$Username] ==
                    input$Password) {
                    USER$PasswordError <- FALSE;
                    USER$Logged <- TRUE; USER$Logout <- FALSE;
                    print(paste0(">> Logged with Username ",
                                 input$Username, " and Password ",
                                 input$Password));
                } else {
                    USER$Login <- FALSE;
                    USER$PasswordError <- TRUE;
                }
            } else {
                USER$Login <- FALSE;
                USER$UsernameError <- TRUE;
            }
        }
    }
})  ## End of observe logs user in

observeEvent(  ## tracks input$kTimeSC
    eventExpr = input$kTimeSC, handlerExpr = {
        if (!is.null(input$kTimeSC)) {
            kTimeSC <<- input$kTimeSC;
        }
    })  ## End of observeEvent input$kTimeSC

observeEvent(  ## tracks input$kSCRatio
    eventExpr = input$kSCRatio, handlerExpr = {
        if (!is.null(input$kSCRatio)) {
            kSCRatio <<- input$kSCRatio;
        }
    })  ## End of observeEvent input$kSCRatio

observeEvent(  ## tracks input$kMode
    eventExpr = input$kMode, handlerExpr = {
        if (!is.null(input$kMode)) {
            kMode <<- input$kMode;
        }
    })  ## End of observeEvent input$kMode

## output$PrintUser <- renderUI({
##     HTML(paste0("Hello ", input$Username, "!"));
## })  ## End of output$PrintUser

observeEvent(  ## tracks input$Logout
    eventExpr = input$Logout, handlerExpr = {
        if (!USER$Logout)
            USER$Logout <- TRUE;
    })  ## End of observeEvent input$Logout

observe({  ## logs the user out
    if (USER$Logged == TRUE) {
        if (USER$Logout) {
            print(">> Logout, clearing all!");
            USER$Logged <- FALSE; USER$Login <- FALSE;
            USER$Selection <- NULL;
            Dataset$DO_Analysis <- FALSE; Dataset$res <- NULL;
            Dataset$data_loaded <- FALSE; Dataset$res_loaded <- FALSE;
            Dataset$parms_loaded <- FALSE;
            dataset$clear();
            Cal$data_loaded <- FALSE; Cal$model_fitted <- FALSE;
            cal$clear();
            TG$data_loaded <- FALSE; TG$model_fitted <- FALSE;
            tg$clear();
        }
    }
})  ## End of observe logs user out
