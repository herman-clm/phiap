#!/usr/bin/env sh

# Install packrat
Rscript -e 'install.packages("packrat")'

# Restore packrat snapshot
R CMD javareconf -e 'Rscript -e packrat::restore()'
