print("#################### >> Thrombin Analyzer reloaded! << ####################");
source("src/libraries.R"); app.path <- getwd();  ## stores current directory
source("src/Base_class.R"); source("src/Cal_class.R"); source("src/TG_class.R");
source("src/Dataset_class.R");
## placeholders for currently empty objects
dataset <- Dataset$new(); cal <- Cal$new(); tg <- TG$new();
print(">> Ready to analyze!");
################################################################################
shinyServer(
    func = function(input, output, session) {
        source(file.path("server", "login.R"),  local = TRUE)$value;
        source(file.path("server", "dataset.R"),  local = TRUE)$value;
        source(file.path("server", "calibration.R"),  local = TRUE)$value;
        source(file.path("server", "thrombin_generation.R"),  local = TRUE)$value;
        source(file.path("server", "parameters.R"),  local = TRUE)$value;
        source(file.path("server", "demo_signals.R"),  local = TRUE)$value;
        source(file.path("server", "tutorial.R"),  local = TRUE)$value;
        ## session$onSessionEnded(stopApp);  ## stop the app when the browser is closed
    }  ## End of function
)  ## End of shinyServer
################################################################################
