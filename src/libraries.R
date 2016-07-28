## additional R libraries
kLibraries <- c("shiny", "minpack.lm", "LambertW", "parallel", "compiler", "R6",
                "Rcpp", "markdown");
CheckAllLibrariesAvailable <- function(libraries) {
    try({require(markdown)});  ## fix for work on shinyapps.io
    for (i in 1:length(libraries)) {
        if (!any(libraries[i] == installed.packages())) {
            stop(paste0(">> Library ", libraries[i], " is not installed!"));
        }
    }
}  ## End of CheckAllLibrariesAvailable
CheckAllLibrariesAvailable(kLibraries);  ## checks if libraries are available

library(shiny);
options(shiny.maxRequestSize = 500 * 1024 ^ 2);  ## max file upload size in B
## options for compiler package
compiler::setCompilerOptions("optimize" = 3);
compiler::setCompilerOptions("suppressAll" = TRUE);
compiler::setCompilerOptions("suppressUndefined" = TRUE);
## print(compiler::getCompilerOption(name = "optimize"));
kCmpFunOptions <- list("optimize" = 3, "suppressAll" = TRUE,
                       "suppressUndefined" = TRUE);
## Disable warnings and set print line to 300 characters
options(warn = -1, width = 300);

## Fast C++ versions of several simple R functions
Rcpp::sourceCpp(file = "src/Rcpp_functions.cpp");
