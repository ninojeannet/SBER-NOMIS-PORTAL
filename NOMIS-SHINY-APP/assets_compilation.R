# Script to compile the CSS and JavaScript for deployment
library(jsonlite)
# Load the js_parser function
source('./utils/helper_functions.R')

# Compile CSS from Sass
sass::sass(
  sass::sass_file('assets/sass/main.scss'), 
  output = 'www/main.css',
  options = sass::sass_options(output_style = 'compressed')
)

# Compile and minify JavaScript
js_parser()