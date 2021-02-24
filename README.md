# SBER NOMIS Portal

This Shiny app is meant to provide an easy interface for the NOMIS researchers to interact with their data. It can also be used as a data sharing portal accessible via the Web if wanted.

## Dependecies

Main dependencies:
- R version 4.0.3
- Node.js (tested version: 12.18.3)
- Terser.js (tested version: 5.3.0)

R packages:
- RMySQL_0.10.20
- sodium_1.1
- purrr_0.3.4
- readr_1.4.0
- sass_0.2.0
- rhandsontable_0.3.7
- DT_0.16
- pool_0.1.5
- DBI_1.1.0
- dplyr_1.0.2
- magrittr_1.5
- tidyr_1.1.2
- forcats_0.5.0
- lubridate_1.7.9
- data.table_1.13.2
- Cairo_1.5-12.2
- ggplot2_3.3.2
- stringr_1.4.0
- jsonlite_1.7.1
- shinycssloaders_1.0.0
- shinybusy_0.2.2
- shinyWidgets_0.5.4
- shinyjs_2.0.0
- shiny_1.5.0
- lambda.r_1.2.4
- rlist_0.4.6.1
- formattable_0.2.0.1
- DataCombine_0.2.21
- staRdom_1.1.14
- kableExtra_1.3.1          
- zip_2.1.1
- xlsx_0.5.6
- waiter_0.1.3
- hrbrthemes_0.8.0          

## Installation

### R and Rstudio
You need to install R and we recommend to use Rstudio as well. You can get the latest version of R or the recommend version forthis app on the CRAN website https://cran.r-project.org/ and Rstutio from their website https://rstudio.com/products/rstudio/download/.

### Node and Terser
To be able to parse and minify the cusom JavaScript files you will need to install Node.js and Terser.js as well.

To install Node please refer yourselfe to the documentation https://nodejs.org/en/download/.

Terser.js will need to be install globally to be accessible by the app. Once Node is installed run the following command:
```sh
npm install terser -g
```

## App oraganisation

### App.R
The main app script is the `App.R` file. It contains the basic app structure and logic to create and operate the main tabs and the initialization tasks.

### Modules
The app is organized and subdivised by tabs. Each tab and sub-tab is contained in a isolated module present in the `modules` directory. The structure of the `modules` directory should mirror the app structure. Some other reusable shiny components that aren't tabs or sub-tabs, but require both an UI and a server function, are also containerized in modules and located in a relevant place of the directory structure of the `modules` directory.

### Utils
Some reusable functions are organized by functionality in different files located in the `utils` directory.

| File                    | Description |
|:----------------------- |:----------- |
| dataframe_generator.R | Contains all the functions necessary for generating displayable dataframe in rhandsontable. |
| helper_database.R    | Contains all the functions used for interaction with the SQL database. |
| helper_dataframe.R        | Contains the functions used to process dataframe. |
| helper_download.R      | Contains all the functions for the download part of the Shiny App. |
| helper_expedition.R    | Contains the functions used to create ggplots. |
| helper_file.R    | Contains the functions used to manipulate files. |
| helper_functions.R    | Contains all the functions that does not fit in other files. |
| helper_log.R    | Contains all the functions to handle log. |
| helper_visualisation.R    | Contains functions used for visualisation. |
| shiny_extensions.R      | Contains the functions that extend _Shiny_ functionalities. |
| template_config.R    | Contains all the configuration informations. |

### Assets
The custom stylesheet in _SCSS_ format and _JavaScript_ are located in the `sass` and `js` directories, respectively. These directories are located in the `assets` directory.

During development, both _SCSS_ and _JavaScript_ files are compiled and minified in two files, `main.css` and `nomisportal.js`, which are saved in the `www` directory.

To deploy the app in **production**, the assets **must be compiled manually** by running the `assets_compilation.R` script file from the app folder.

#### _SCSS_
In the `sass` directory, the `main.scss` file is an index that is used to load in the correct order all the partial files organized in different thematic directories.

#### _JavaScript_
The `js` directory contains all the _JavaScript_ code organized in different files by functionnality. The `manifest.json` file is used to compile the files together in a predefined order.

### Other _R_ script files

#### app_config.R
A `app_config.R` file **is required** and should contains all sensible information, such as DB name or password. These information are saved in environment variables when the file is sourced during the app startup. More info at https://github.com/mclement18/METALP-Portal-server/tree/master/app_deployment#create-or-update-secretr-file

#### packages_installation.R
The `packages_installation.R` file contains instructions to install the _R_ packages with the correct version. To install them, just run the script file.

### Other directories

#### HTML components
The `html_components` directory contains all the HTML template used in the app.

#### www
The `www` directory is the public directory of the _Shiny_ app in which all the publicly accessible ressources must be put, such as favicon, images or assets.

#### Data
The `data` directory contains all the files saved from the application on the server.

#### DB backups
The `db_backups` directory **should be present** and will contains all the _SQL_ database backups made with the DB backup functionnality of the portal actions module. See `modules/portal_management/portal_actions.R` file for more information.

## App deployment
Detail information on how to deploy this app on an _Ubuntu_ server can be found here: https://github.com/mclement18/METALP-Portal-server/tree/master/app_deployment