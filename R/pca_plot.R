#' PCA plot for massdataset object
#'
#' PCA visulization for massdataset by PCAplot package and plotly.
#' @return A ggplot object of missing value.
#' @param object a mass_dataset class
#' @param colby pca color based on sample information
#' @param center see PCAplot
#' @param removeVar see PCAplot
#' @param interactive output plot format, 'plot or plotly'
#' @importFrom massdataset extract_expression_data extract_sample_info
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom dplyr mutate select pull
#' @importFrom PCAtools pca biplot
#' @importFrom plotly ggplotly plot_ly add_trace layout
#' @references see massdataset::show_sample_missing_values
#'
#' @noRd
#' @export
#'

pca_plot = function(object,colby,center = T,scale = T,removeVar = .1,interactive = F) {

  tbl_for_pca <-
    object %>%
    extract_expression_data()

  temp_sample_name = colnames(object %>% extract_sample_info())


  if('batch'%in%temp_sample_name) {

    tbl_info <-
      object %>%
      extract_sample_info() %>%
        as_tibble() %>%
        column_to_rownames('sample_id') %>%
        mutate(batch = as.character(batch))

  } else {

    tbl_info <-
        object %>%
        extract_sample_info() %>%
          as_tibble() %>%
          column_to_rownames('sample_id')

  }

  col_colby = colnames(tbl_info)

  col = tbl_info %>% select(colby) %>% pull(colby)

  obj_tbl <- PCAtools::pca(
    mat  = tbl_for_pca %>% select(rownames(tbl_info)),
    metadata = tbl_info,
    center = center,
    scale = scale,
    removeVar = removeVar
  )

  if (isTRUE(interactive)) {
    plt_pca = plot_ly() %>%
      add_trace(
        x = obj_tbl$rotated$PC1,y = obj_tbl$rotated$PC2, z = obj_tbl$rotated$PC3,
        type = 'scatter3d',mode = 'markers',color = col,hovertext = rownames(tbl_info)
      ) %>%
      layout(
        title = list(text = "3D PCA"),
        legend = list(title = list(text = colby)),
        scene = list(
          xaxis = list(title = "PC1"),
          yaxis = list(title = "PC2"),
          zaxis = list(title = "PC3")
        )
      )
  } else {
    plt_pca = PCAtools::biplot(
      pcaobj = obj_tbl,
      colby = colby,
      ellipse = T,
      lab = "",
      legendPosition = 'top'
    )
  }
  return(plt_pca)
}
