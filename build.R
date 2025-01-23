# function to test, build, install
tbi <- function() {
    devtools::check(error_on = "error", vignettes = FALSE)
    usethis::use_tidy_description()
    devtools::document()
    tgz <- devtools::build(vignettes = FALSE)
    drat::insertPackage(tgz, "/labs/rrockne/MHO")
    install.packages("hprcc", repo = "http://cgt.coh.org/MHO")
}

# Check the package
devtools::check(error_on = "error", vignettes = FALSE)

# Make a release on GitHub
## put fields in standard order and alphabetises dependencies
usethis::use_tidy_description()
# use_tidy_eval()
# use_version()

# build the package
tgz <- devtools::build(vignettes = FALSE)
# Publish to cgt.coh.org
drat::insertPackage(tgz, "/labs/rrockne/MHO")

# install locally
# devtools::install()
install.packages("hprcc", repo = "http://cgt.coh.org/MHO")

###############################
# Build the pkgdown site
################################
tbi()

# build the pkgdown site
pkgdown::clean_site()
devtools::document()
pkgdown::build_site()

# Draft a release for GitHub
usethis::use_github_release(publish = FALSE)

# Publish to cgt.coh.org
system("rsync -avz --delete _site/ domeally@cgt.coh.org:/labs/rrockne/MHO/hprcc-www")