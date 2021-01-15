# Script to compile the CSS and JavaScript for deployment
library(jsonlite)
library(readr)
# Load the js_parser function

# Compile CSS from Sass
sass::sass(
  sass::sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass::sass_options(output_style = 'compressed')
)

# Create full path using the wd
inputDir <- file.path(getwd(), 'assets/js')
outputDir <- file.path(getwd(), 'www')

# Load the manifest file
manifest <- read_json(file.path(inputDir, 'manifest.json'), simplifyVector = TRUE)

# Create an empty variable for the compiled JS
compiled <- ''

# For each file in in the manifest
# Read the file and append the content to the compiled variable
for (filename in manifest) {
  compiled <- paste0(compiled, read_file(file.path(inputDir, filename)))  
}

# Create a temporary file with the compiled JS
tmp <- tempfile()
write_file(compiled, tmp)

# Create the full path to the output file
outputFile = file.path(outputDir, 'nomisportal.js')

# Get the correct terser command depending on the OS
command <- 'terser'
if (Sys.info()["sysname"] == 'Windows') command <- paste0(command, '.cmd')

# Minify the compiled JS and saves it in the output file
processx::run(command = command, args = c(tmp, '-c', '-o', outputFile))

# Delete the temporary file
file.remove(tmp)