cat("#########################################################\n");
cat("Hello!\n.Rprofile file is loading...\n");

# Needed for rmarkdown which uses pdflatex.exe.
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "W:\\Tools\\miktex\\program\\miktex\\bin", sep=.Platform$path.sep));

# http://yihui.name/knitr/options/
# Global default chunk options: http://kbroman.org/knitr_knutshell/pages/Rmarkdown.html
knitr::opts_chunk$set(fig.pos = 'H', fig.path='figures/', warning=FALSE,
                      cache = FALSE, autodep = TRUE, cache.path = 'cache/', out.height='150px'
)

cat(".Rprofile file loaded for project.\n")
cat("#########################################################\n")
