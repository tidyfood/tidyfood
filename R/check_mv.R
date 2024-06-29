#' Check missing value
#'
#' Detected errors or batch effects of your untargetd metabolomics data.
#' @return A ggplot object of missing value.
#' @param object a mass_dataset class
#' @param colby the boxplot were color by the columns of `sample_info` in object
#' @param orderby the order of x-axis
#' @param type output plot format, 'plot or plotly'
#' @importFrom massdataset show_sample_missing_values
#' @importFrom ggsci scale_color_aaas
#' @importFrom ggplot2 geom_hline ylim scale_size_continuous theme element_text
#' @importFrom plotly ggplotly
#' @references see massdataset::show_sample_missing_values
#'
#' @noRd
#' @export
#'

check_mv <- function(object,colby,orderby,type = "plot") {
  plt_mv<-
    show_sample_missing_values(object = object, percentage = TRUE,color_by = colby,order_by = orderby)+
    theme1() +
    geom_hline(yintercept = 80,color = "red",linetype="dashed")+
    ylim(c(0,100))+
    scale_size_continuous(range = c(0.1, 2)) +
    ggsci::scale_color_aaas()+
    theme(axis.text.x = element_text(size = 3))
  if(type == "plot") {
    return(plt_mv)
  } else if (type == "plotly") {
    return(plotly::ggplotly(plt_mv))
  }
}
