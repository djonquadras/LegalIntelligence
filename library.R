libraries <- c(	"chron",     "openxlsx",     "dplyr",
		"lubridate", "ggplot2",      "shinycssloaders",
		"readxl",    "scales",       "tibble",
		"magrittr",  "stringr",      "plotly",
		"forecast",  "colourpicker", "shinythemes",
		"DT")

for(i in 1:length(libraries)){

  if(libraries[i] %in% rownames(installed.packages()) == FALSE){
    install.packages(libraries[i])
  }

  cat('Package' , libraries[i], 'is ready', '\n')

}
suppressMessages(sapply(libraries, require, character.only = T))

cat('\n')
cat('All required packages are loaded! :)', '\n')

rm(libraries)

