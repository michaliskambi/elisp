See http://stackoverflow.com/questions/20678847/cannot-load-cl-lib-at-emacs-startup:

    create new directory cl-lib in your .emacs.d directory
    put this file into this cl-lib directory
      https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/cl-lib.el

    add at top of your .emacs file these strings:
     (add-to-list 'load-path "/path_to_your.emacs.d/cl-lib/")
     (require 'cl-lib)

    for me:
      (add-to-list 'load-path (concat kambi-elisp-path "contrib/cl-lib-compat/"))
      (require 'cl-lib)

See also http://elpa.gnu.org/packages/cl-lib.html
