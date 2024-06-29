#' rsd plot for mass_dataset
#'
#' rsd plot for mass_dataset, before and after normalization.
#' @return A ggplot object of missing value.
#' @param obj_old a mass_dataset class of unnormalized
#' @param obj_new a mass_dataset class of normalized
#' @param QC_tag column tags of QC samples
#' @importFrom massdataset extract_expression_data extract_sample_info activate_mass_dataset
#' @importFrom tibble as_tibble column_to_rownames rownames_to_column
#' @importFrom dplyr mutate select pull filter contains group_by summarise case_when inner_join n
#' @importFrom tidyr pivot_longer
#' @importFrom PCAtools pca
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual geom_vline geom_hline geom_abline geom_label xlim ylim labs theme_bw theme element_line element_rect element_text
#'
#' @noRd
#' @export
#'

rsd_plot = function(obj_old,obj_new,QC_tag){

  # calculate rsd for old object and new object.
  rsd_before <-
    obj_old %>%
    activate_mass_dataset("sample_info") %>%
    dplyr::filter(class == QC_tag) %>%
    extract_expression_data() %>%
    rownames_to_column("ID") %>%
    pivot_longer(contains(QC_tag),names_to = "tag",values_to = "value") %>%
    select(-tag) %>%
    group_by(ID) %>%
    summarise(
      raw.rsd = (sd(value,na.rm = T)/mean(value,na.rm = T))*100
    )

  rsd_after <-
    obj_new %>%
    activate_mass_dataset("sample_info") %>%
    dplyr::filter(class == QC_tag) %>%
    extract_expression_data() %>%
    rownames_to_column("ID") %>%
    pivot_longer(contains(QC_tag),names_to = "tag",values_to = "value") %>%
    select(-tag) %>%
    group_by(ID) %>%
    summarise(
      norm.rsd = (sd(value,na.rm = T)/mean(value,na.rm = T))*100
    )

  join_rsd <- inner_join(rsd_before,rsd_after,by = "ID") %>%
    mutate(
      norm_tag = case_when(
        norm.rsd <= 30 ~ "RSD <= 30",
        TRUE ~ "RSD > 30 "
      ),
      raw_tag = case_when(
        raw.rsd <= 30 ~ "RSD <= 30",
        TRUE ~ "RSD > 30 "
      )
    )

 # n1 = join_rsd %>% group_by(norm_tag) %>% summarise(num = n())

  rsd_plt = ggplot(data = join_rsd,mapping = aes(x = raw.rsd,y = norm.rsd,color = norm_tag)) +
    geom_point(size = 1.2,alpha =.8)+
    scale_color_manual(values = c("RSD <= 30" = "salmon","RSD > 30" = "grey30"))+
    geom_vline(xintercept = 30,linetype = "dashed",color = "red")+
    geom_hline(yintercept = 30,linetype = "dashed",color = "red")+
    geom_abline(slope = 1,linetype = "dashed",color = "red")+
    # geom_label(
    #   data = data.frame(
    #     x = c(120,110),
    #     y = c(15,110),
    #     label = paste0("n=",c(n1[2,2],n1[1,2])),
    #     color = (c("RSD <= 30","RSD > 30"))
    #   ),
    #   mapping = aes(x = x,y = y,label = label,color= color),alpha = .8
    # )+
    xlim(0,150)+
    ylim(0,150)+
    labs(x = "RSD of raw peak area",y = "RSD of svr normalized peak area")+
    theme_bw()+
    theme(
      line = element_line(size = 1,color = "black"),
      rect = element_rect(size = 1, color = "black"),
      axis.text = element_text(size = 14, color = "black"),
      axis.title = element_text(size = 16,color = "black",face = "bold"),
      panel.border = element_rect(linewidth = 1)

    )

  out = list(
    rsd_tbl = join_rsd,
    plot = rsd_plt
  )

  return(out)
}
