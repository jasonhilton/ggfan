# run this file from the top-level package directory
# eg 'Rscript tools/make_github_readme.R'
library(ggplot2)
library(ggfan)
rmarkdown::render(input = "README.Rmd",
                  output_format = rmarkdown::github_document(html_preview = F))
