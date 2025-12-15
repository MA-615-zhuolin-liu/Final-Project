# Final-Project
The Shiny application runs correctly in RStudio on my local machine.
However, deployment to BU's Posit Connect fails due to missing GDAL
system libraries required by the `terra` package (used indirectly by
the mapping code).The deployment error log is saved in `posit_deploy_error.txt` in this
repository in file"615 final", as suggested in the instructor's announcement.
