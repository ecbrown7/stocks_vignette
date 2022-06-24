#author: "Evan Brown"
#date: "6/23/2022"
#purpose: Render stocks_vignette.Rmd as a .md file called README.md for my repo.


rmarkdown::render(
  input="stocks_vignette.Rmd",
  output_format = "github_document",
  output_file = "README.md",
  runtime = "static",
  clean = TRUE,
  params = NULL,
  knit_meta = NULL,
  envir = parent.frame(),
  run_pandoc = TRUE,
  quiet = FALSE,
  encoding = "UTF-8"
)