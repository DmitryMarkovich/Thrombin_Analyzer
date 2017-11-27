library(shiny);
source("src/global_parameters.R");
source("ui/login_ui.R");
source("ui/app_ui.R");
################################################################################
shinyUI(ui = htmlOutput(outputId = "page"))  ## End of ShinyUI
################################################################################
