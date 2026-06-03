library(quarto)

# 1. Get a list of all .qmd files in the current directory
files <- list.files(pattern = "\\.qmd$", full.names = TRUE)

# 2. Loop through and render each one
for (file in files) {
  message(paste("Rendering:", file))
  
  tryCatch({
    quarto_render(file)
    message(paste("Successfully rendered:", file))
  }, error = function(e) {
    message(paste("Failed to render:", file))
    message(e)
  })
}