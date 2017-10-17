This is the Emacs configuration of Michalis Kamburelis ( http://michalis.ii.uni.wroc.pl/~michalis/ ). I use this repo to synchronize my Emacs configuration between many systems.

Some of it is old, some of it is new. Some of it is more generally-useful, some of it is very specific to my personal needs and preferences.

Reuse as you like:)

My typical setup instructions (assuming you put this in ~/elisp, otherwise edit the path in sample_dot_emacs.el):

~~~~
cd $HOME
git clone https://github.com/michaliskambi/elisp

# If you create ~/tmp, deleting from Emacs will move files there.
mkdir -p ~/tmp

cat ~/elisp/doc/sample_dot_emacs.el >> .emacs

# If you have ~/bin on $PATH, you can place there the scripts "e" and "e-wait".
mkdir -p ~/bin
cd ~/elisp/script/
./install-e

# Note: the first Emacs run with this .emacs will install some packages,
# see kam-install-if-needed invocation in kambi-various-personal.el .
# Configure/upgrade then by paradox-list-packages.
~~~~

There are no _required_ dependencies on external programs, everything should work regardless of the state of the system. But you may find it useful to install some software:

- [ripgrep](https://github.com/BurntSushi/ripgrep), my favourite grep-like tool. The Linux binaries for ripgrep are now hosted in this repository, to make it instantly available on new systems, so I only need to clone this repository to have the setup I like.

    Previously I used [ag (silver searcher)](https://github.com/ggreer/the_silver_searcher), installable by `sudo apt-get install silversearcher-ag` on Linux.

    And previously I used simple grep. Code to fallback to them can still be found in this elisp code.

- On Windows, the Emacs will be automatically integrated with [cygwin](https://www.cygwin.com/), so it will run Cygwin's shell, understand Cygwin's paths.
