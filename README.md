
Slides for talks around the book Empirical Software Engineering using R

http://asciidoc.org[Asciidoc] is used to create a slidy presentation.

To build the html files:

* You will need the program source-highlight (used by asciidoc)

* Configure (using ln -s) the hardcoded paths ESEUR in the Rnw files to the ../R directory.

* Start R and type:

 source("ascii-slides.R")
 quit()

* change directory to data-detective

* At the Linux command line:

 ./mkallslides.sh

* the html file now exists

