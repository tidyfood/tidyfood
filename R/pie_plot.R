#' pie plot for classification
#'
#' Pie plot.
#' @return A ggplot object of missing value.
#' @param x a MDAtoolkits classyfire result.
#' @param tag level of classyfire superclass class subclass
#' @param cut cut-off of small category
#' @importFrom dplyr select group_by summarise n case_when ungroup
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 aes geom_bar coord_polar geom_text position_stack theme_void
#' @references see massdataset::show_sample_missing_values
#'
#' @noRd
#' @export
#'

pie_plot <- function(x, tag,cut){
  temp = x %>%
    dplyr::select(tag) %>%
    drop_na() %>%
    setNames("key") %>%
    group_by(key) %>%
    summarise(n = n()) %>%
    mutate(new_tag = case_when(
      n < cut ~ 'other',
      TRUE ~ key
    )) %>%
    ungroup() %>%
    group_by(new_tag) %>%
    summarise(sum = sum(n)) %>%
    ungroup()

  p =  ggplot(temp, aes(x="", y=sum, fill=new_tag)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    geom_text(aes(label=sum), position=position_stack(vjust=0.5)) +
    labs(fill=tag) +
    theme_void()
  return(p)
}

#' pie plot for classification plotly version
#'
#' Pie plot - plotly.
#' @return A ggplot object of missing value.
#' @param x a MDAtoolkits classyfire result.
#' @param tag level of classyfire superclass class subclass
#' @param cut cut-off of small category
#' @importFrom dplyr select group_by summarise n case_when ungroup
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 aes geom_bar coord_polar geom_text position_stack theme_void
#' @importFrom plotly plot_ly layout
#' @references see massdataset::show_sample_missing_values
#'
#' @noRd
#' @export
#'

pie_plot_plotly <- function(x, tag, cut){
  temp = x %>%
    dplyr::select(tag) %>%
    drop_na() %>%
    setNames("key") %>%
    group_by(key) %>%
    summarise(n = n()) %>%
    mutate(new_tag = case_when(
      n < cut ~ 'other',
      TRUE ~ key
    )) %>%
    ungroup() %>%
    group_by(new_tag) %>%
    summarise(sum = sum(n)) %>%
    ungroup()

  plot_ly(temp, labels = ~new_tag, values = ~sum, type = 'pie', textinfo = 'percent+value',
          textposition = 'inside', insidetextorientation = 'radial', hoverinfo = 'label+percent+value') %>%
    layout(title = paste('Pie Chart of', tag),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}
