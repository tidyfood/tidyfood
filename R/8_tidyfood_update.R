##------------------------------------------------------------------------------
#' @title check_tidyfood_version
#' @description Check if there are packages in tidyfood can be updated.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param packages core or all packages in tidyfood. "core" means all the core
#' packages in tidyfood and "all" means all the packages in tidyfood.
#' @param from "gitlab", "github", or "gitee", or "tidyfood.org"
#' @export

check_tidyfood_version <-
  function(packages = c("core", "all"),
           from = c("gitlab", "github", "gitee", "shen")) {
    from <- match.arg(from)
    packages <- match.arg(packages)
    check_result <-
      c(
        "tidyfood",
        "massconverter",
        "massdataset",
        "massprocesser",
        "masscleaner",
        "massqc",
        "metid",
        "massstat",
        "metpath",
        "masstools"
      ) %>%
      lapply(function(x) {
        y <-
          tryCatch(
            check_github(pkg = paste0("tidyfood/", x)),
            error = function(e) {
              NULL
            }
          )
        if (is.null(y)) {
          y <-
            tryCatch(
              check_gitlab(pkg = paste0("tidyfood/", x)),
              error = function(e) {
                NULL
              }
            )
        }

        if (is.null(y)) {
          y <-
            tryCatch(
              check_gitee(pkg = paste0("tidyfood/", x)),
              error = function(e) {
                NULL
              }
            )
        }

        if (is.null(y)) {
          y <-
            tryCatch(
              check_tidyfood.org(pkg = x),
              error = function(e) {
                NULL
              }
            )
        }

        if (is.null(y)) {
          y <-
            c(
              package = paste0("tidyfood/", x),
              installed_version = "1.0.0",
              latest_version = "1.0.0",
              up_to_date = TRUE
            )
        }
        y$installed_version <-
          as.character(y$installed_version)
        unlist(y)
      })

    check_result <-
      do.call(rbind, check_result) %>%
      as.data.frame()

    check_result$package <-
      check_result$package %>%
      stringr::str_replace("tidyfood\\/", "")

    check_result$up_to_date <-
      check_result$installed_version ==
      check_result$latest_version

    check_result$up_to_date <-
      as.logical(check_result$up_to_date)

    if (packages == "core") {
      check_result <-
        check_result %>%
        dplyr::filter(!stringr::str_detect(package, "massconverter"))
    }

    if (all(check_result$up_to_date)) {
      message("No package to update.")
    } else{
      check_result <-
        check_result %>%
        dplyr::filter(!up_to_date)
      message("Use update_tidyfood() to update the following pacakges.")
      check_result
    }
  }


##------------------------------------------------------------------------------
#' @title update_tidyfood
#' @description Update packages in tidyfood.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param packages core or all packages in tidyfood. "core" means all the core
#' packages in tidyfood and "all" means all the packages in tidyfood.
#' @param from github, gitlab or gitee.
#' @param fastgit if install packages using fastgit. see
#' https://hub.fastgit.org/
#' @importFrom remotes install_github install_gitlab install_git
#' @importFrom masstools install_fastgit
#' @export
update_tidyfood <-
  function(packages = c("core", "all"),
           from = c("gitlab", "github", "gitee", "tidyfood.org"),
           fastgit = FALSE) {
    packages <- match.arg(packages)
    from <- match.arg(from)

    check_result <-
      check_tidyfood_version(packages = packages)

    if (!is.null(check_result)) {
      if (from == "github") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )
          if (fastgit) {
            masstools::install_fastgit(
              pkg = paste0("tidyfood/", i),
              from = from,
              upgrade = "never"
            )
          } else{
            remotes::install_github(repo = paste0("tidyfood/", i),
                                    upgrade = "never")
          }

        }
      }

      if (from == "gitlab") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )

          if (fastgit) {
            masstools::install_fastgit(
              pkg = paste0("tidyfood/", i),
              from = from,
              upgrade = "never"
            )
          } else{
            remotes::install_gitlab(repo = paste0("tidyfood/", i),
                                    upgrade = "never")
          }
        }
      }

      if (from == "gitee") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )
          if (fastgit) {
            masstools::install_fastgit(
              pkg = paste0("tidyfood/", i),
              from = from,
              upgrade = "never"
            )
          } else{
            remotes::install_git(url = paste0("https://gitee.com/tidyfood/", i),
                                 upgrade = "never")
          }
        }
      }

      if (from == "tidyfood.org") {
        for (i in check_result$package) {
          tryCatch(
            detach(name = paste0("package:", i)),
            error = function(e) {
              message(i, ".\n")
            }
          )
          install_tidyfood(which_package = i, from = "tidyfood.org")
        }
      }
    }
  }
