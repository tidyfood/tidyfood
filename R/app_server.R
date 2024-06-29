#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_themer
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  bs_themer()
  # Call module server functions
  if(Sys.info()["sysname"] == "Windows") {
    volumes = getVolumes_win()
  } else {
    volumes = shinyFiles::getVolumes()()
  }
  #> project init
  prj_init <- reactiveValues(data = NULL) # project init
  project_init_server(id = "project_init_id",volumes = volumes,prj_init)
  project_init_server(id = "project_restart_id",volumes = volumes,prj_init)
}
