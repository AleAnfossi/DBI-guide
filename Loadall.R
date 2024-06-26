installed_packages <- installed.packages()[, "Package"] 
for (pkg in installed_packages) { suppressPackageStartupMessages(library(pkg, character.only = TRUE)) }

