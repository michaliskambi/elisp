wget http://www.xsteve.at/prg/emacs/psvn.el --output-document=psvn.el

# patch from http://www.eaflux.com/
wget http://www.eaflux.com/psvn/psvn.el.diff --output-document=psvn.el.diff
patch < psvn.el.diff
