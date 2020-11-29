cat("#########################################################\n");
cat(".Rprofile file is loading...\n");

# MiKTeX: Needed for rmarkdown which uses pdflatex.exe.
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "W:/Tools/miktex/program/miktex/bin", sep=.Platform$path.sep));
try(cat(paste("LaTeX installed:", pkgbuild::has_latex(), "\n")))

# Pandoc: Needed by pkgdown when run from console.
Sys.setenv(PATH = paste("W:/Tools/pandoc/program",
                        Sys.getenv("PATH"), sep=.Platform$path.sep))

# RTools
Sys.setenv(PATH = paste(normalizePath("W:/Tools/rtools/program/mingw_64/bin", winslash = "/"),
                        Sys.getenv("PATH"), sep=.Platform$path.sep))
Sys.setenv(PATH = paste(normalizePath("W:/Tools/rtools/program/bin", winslash = "/"),
                        Sys.getenv("PATH"), sep=.Platform$path.sep))
# As of R 3.3 you need to set the path to the compiler using the BINPREF
# variable. This is because we ship two separate versions of gcc, one
# targeting win32 and one targeting win64. If you compile R packages you need both at
# the same time. Hence the "$(WIN)" variable in the BINPREF.
Sys.setenv(BINPREF = normalizePath("W:/Tools/rtools/program/mingw_64/bin/", winslash = "/"))
try(cat(paste("RTools installed:", pkgbuild::has_build_tools(debug = TRUE), "\n")))

# QPDF, used by CRAN to compress pdf's
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "W:/Tools/pdf/qpdf/program/x64/bin", sep=.Platform$path.sep))

# Might be dangerous in some cases, but improves portability of code.
# Sys.timezone(): default is usualy "Europe/Paris" or similar.
Sys.setenv(TZ = "UTC")

cat(".Rprofile file loaded for project.\n")
cat("#########################################################\n")
