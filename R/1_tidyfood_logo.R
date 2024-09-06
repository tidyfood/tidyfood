#' @title Show the logo tidyfood
#' @description The tidyfood logo, using ASCII or Unicode characters
#' @author Xiaotao Shen
#' \email{xiaotao.shen@@outlook.com}
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#' on UTF-8 platforms.
#' @return A ASCII log of tidyfood
#' @export
#' @importFrom dplyr filter
#' @import tidyfood
#' @import massprocesser
#' @import masscleaner
#' @import massqc
#' @import metid
#' @import massstat
#' @import masstools
#' @importFrom utils packageVersion install.packages read.table
#' @examples
#' tidyfood_logo()
##https://onlineasciitools.com/convert-text-to-ascii-art

tidyfood_logo <-
  function(unicode = l10n_info()$`UTF-8`) {
    message(crayon::green("Thank you for using tidyfood!"))
    message(crayon::green("Version", tidyfood_version, "(", update_date, ')'))
    message(crayon::green("More information: tidyfood.org"))

    logo <-
      c(
        "  _   _     _       ______              _",
        " | | (_)   | |     |  ____|            | |",
        " | |_ _  __| |_   _| |__ ___   ___   __| |",
        " | __| |/ _` | | | |  __/ _ \\ / _ \\ / _` |",
        " | |_| | (_| | |_| | | | (_) | (_) | (_| |",
        "  \\__|_|\\__,_|\\__, |_|  \\___/ \\___/ \\__,_|",
        "               __/ |",
        "              |___/"
      )


    hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")
    if (unicode)
      hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]

    cols <- c(
      "red",
      "yellow",
      "green",
      "magenta",
      "cyan",
      "yellow",
      "green",
      "white",
      "magenta",
      "cyan"
    )

    col_hexa <- purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

    for (i in 0:9) {
      pat <- paste0("\\b", i, "\\b")
      logo <- sub(pat, col_hexa[[i + 1]], logo)
    }

    structure(crayon::blue(logo), class = "tidyfood_logo")
  }

#' @export

print.tidyfood_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}

tidyfood_version <-
  as.character(utils::packageVersion("tidyfood"))
update_date <-
  as.character(Sys.time())



# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# tidyfood_logo <-
#   c("  _   _     _       ______              _", " | | (_)   | |     |  ____|            | |",
#     " | |_ _  __| |_   _| |__ ___   ___   __| |", " | __| |/ _` | | | |  __/ _ \\ / _ \\ / _` |",
#     " | |_| | (_| | |_| | | | (_) | (_) | (_| |", "  \\__|_|\\__,_|\\__, |_|  \\___/ \\___/ \\__,_|",
#     "               __/ |", "              |___/")
# cat(tidyfood_logo, sep = "\n")
