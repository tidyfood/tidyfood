#' Check batch effect
#'
#' Detected errors or batch effects of your untargetd metabolomics data.
#' @return A string contains clean adducts.
#' @param object a mass_dataset class
#' @param colby the boxplot were color by the columns of `sample_info` in object
#' @param type output plot format, 'plot or plotly'
#' @importFrom massdataset extract_sample_info extract_expression_data
#' @importFrom dplyr filter mutate left_join arrange
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom ggplot2 ggplot aes geom_boxplot ylab xlab labs theme_bw theme element_text
#' @importFrom plotly ggplotly
#'
#' @noRd
#' @export
#'
#'

QC_boxplot = function(object,colby,type = c('plot','plotly')) {
  type = match.arg(type)
  temp_sample_info =
    object %>%
    extract_sample_info() %>%
    filter(class == "QC") %>%
    mutate(batch = as.character(batch))
  if(colby == 'batch') {
    temp_sample_info <-
      temp_sample_info %>%
      mutate(key = batch)
  } else {
    temp_sample_info <-
      temp_sample_info %>%
      mutate(key = injection.order)
  }
  plt_batch =
    object %>%
    extract_expression_data() %>%
    dplyr::select(all_of(temp_sample_info %>% pull(sample_id))) %>%
    pivot_longer(all_of(temp_sample_info %>% pull(sample_id)),values_to = "value",names_to = "sample_id") %>%
    drop_na() %>%
    left_join(temp_sample_info) %>%
    mutate(
      value = log2(value)
    ) %>%
    arrange(injection.order,key) %>%
    ggplot(data = .,mapping = aes(x = sample_id,y = value,color = key))+
    geom_boxplot()+
    ylab("log2(Raw peak area)")+
    xlab("")+
    labs(color =colby) +
    theme_bw()+
    theme1()+
    theme(
      axis.text.x = element_text(angle = 90,vjust = 1,hjust = 1),
      legend.position = "right",
    )
  if(type == "plot") {
    return(plt_batch)
  } else if (type == "plotly") {
    return(plotly::ggplotly(plt_batch))
  }

}
