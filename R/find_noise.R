#' find noise
#'
#' Detected errors or batch effects of your untargetd metabolomics data.
#' @return A ggplot object of missing value.
#' @param object a mass_dataset class
#' @param tag Remove noise based on column names of `sample_info` in object
#' @param qc_na_freq missing value ratio of QC sample
#' @param S_na_freq missing value ratio of tag groups
#' @importFrom massdataset extract_sample_info activate_mass_dataset mutate_variable_na_freq extract_variable_info
#' @importFrom magrittr %>%
#' @importFrom dplyr rename mutate case_when filter across left_join anti_join
#' @references based on  massdataset::mutate_variable_na_freq
#'
#' @noRd
#' @export
#'


find_noise = function(object,tag = "class",qc_na_freq = 0.2,S_na_freq = 0.5) {

  if(tag == "class") {
    temp_sample_info = object %>%
      extract_sample_info() %>%
      mutate(key = class)
  } else {
    temp_sample_info = object %>%
      extract_sample_info() %>%
      dplyr::rename("tag" = tag) %>%
      dplyr::rename("xx" = "class") %>%
      dplyr::mutate(key = case_when(
        xx == "QC" ~ "QC",
        TRUE ~ tag
      ))
  }

  temp_keys = temp_sample_info %>% pull(key) %>% unique()

  object <-
    object %>%
    activate_mass_dataset("sample_info") %>%
    left_join(temp_sample_info %>% select(sample_id,key),by = "sample_id")
  #> na_freq
  for (i in 1:length(temp_keys)) {
    temp_id = object %>%
      activate_mass_dataset(what = "sample_info") %>%
      filter(key == temp_keys[i]) %>%
      pull(sample_id)
    if(i == 1) {
      object_mv <-
        object %>%
        mutate_variable_na_freq(according_to_samples = temp_id)
      object_mv =
        object_mv %>%
        activate_mass_dataset(what = "variable_info") %>%
        filter(na_freq <= qc_na_freq)
    } else {
      object_mv <-
        object_mv %>%
        mutate_variable_na_freq(according_to_samples = temp_id)
      colnames_vari = object_mv %>% extract_variable_info() %>% colnames()
      if('ions'%in%colnames_vari) {
        object_mv =
          object_mv %>%
          activate_mass_dataset(what = "variable_info") %>%
          filter(across(.cols = i+4,.fns = ~ . <= S_na_freq))
      } else {
        object_mv =
          object_mv %>%
          activate_mass_dataset(what = "variable_info") %>%
          filter(across(.cols = i+3,.fns = ~ . <= S_na_freq))
      }

    }
  }
  vari_ori <- object %>% extract_variable_info()
  vari_filter <- object_mv %>% extract_variable_info()
  vari_noisy = anti_join(vari_ori,vari_filter)
  out = list(
    noisy_tbl = vari_noisy,
    object_mv = object_mv
  )
  return(out)

}
