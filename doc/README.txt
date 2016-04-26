This is Michalis Kamburelis (aka Kambi) customization code for Emacs,
of course in EmacsLisp.

It contains a couple of things that you may find generally useful.
But it's also full of things that merely reflect my personal preference,
it even contains some hard-coded paths relevant to my projects.
So you probably only want to browse and copy & paste the interesting bits.

It's kept public in SVN repository on michalis.ii.uni.wroc.pl server.
See main server page http://michalis.ii.uni.wroc.pl/ for instructions,
SVN and WebSVN urls.

Usage: to include everything, you should require kambi-startup in your ~/.emacs.
kambi-startup does everything else (that can be done uotside ~/.emacs).
See sample_dot_emacs.el file for details.

------------------------------------------------------------------------------
Compatibility:

All Emacs >= 22 versions are supported. Generally, I want to always
keep compatibility at least with Emacs in current Debian stable,
as I administer on some servers where I realy want to stay on
Debian stable (not upgrade to testing).
Older Emacs and XEmacs versions used to work, but are no longer tested.

Should work on various operating systems, Unix and Windows too.
For both normal and --batch operation.

eof --------------------------------------------------------------------------
