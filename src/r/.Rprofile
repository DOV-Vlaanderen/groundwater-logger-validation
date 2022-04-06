cat("#########################################################\n");
cat("Hello!\n.Rprofile file is loading...\n");

# renv
source("renv/activate.R")

# RTools
local({
  rtools.path <- paste0(R.home(), '/../../../rtools/program/v3.5.0.4')
  if (dir.exists(rtools.path)) {
    Sys.setenv('PATH' = paste(paste0(rtools.path, '/mingw_64/bin'),
                            Sys.getenv('PATH'), sep = .Platform$path.sep))
    Sys.setenv('PATH' = paste(paste0(rtools.path, '/bin'),
                            Sys.getenv('PATH'), sep = .Platform$path.sep))
    # As of R 3.3 you need to set the path to the compiler using the BINPREF
    # variable. This is because we ship two separate versions of gcc, one
    # targeting win32 and one targeting win64. If you compile R packages you need both at
    # the same time. Hence the "$(WIN)" variable in the BINPREF.
    Sys.setenv('BINPREF' = paste0(rtools.path, '/mingw_64/bin/'))

    try(cat(paste('RTools installed:', pkgbuild::has_build_tools(debug = TRUE), '\n')))
  }
})

# Needed for rmarkdown which uses pdflatex.exe.
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "W:\\Tools\\miktex\\program\\miktex\\bin", sep=.Platform$path.sep));

# http://yihui.name/knitr/options/
# Global default chunk options: http://kbroman.org/knitr_knutshell/pages/Rmarkdown.html
try(knitr::opts_chunk$set(fig.pos = 'H', fig.path = 'figures/', warning = FALSE,
                          cache = FALSE, autodep = TRUE, cache.path = 'cache/', out.height = '150px'
))

# Load gwloggeR package
try(devtools::load_all('./../../gwloggeR', export_all = FALSE))
try(detach('package:gwloggeR'))

# Load gwloggeR.data package
options(logger.root.data.path = './../../data/raw')
try(devtools::load_all('./../../gwloggeR.data', export_all = FALSE))
try(detach('package:gwloggeR.data'))

# Might be dangerous in some cases, but improves portability of code.
# Sys.timezone(): default is usualy "Europe/Paris" or similar.
Sys.setenv(TZ = "UTC")

cat(".Rprofile file loaded for project.\n")
cat("#########################################################\n")
