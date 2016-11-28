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

# Install extra Emacs stuff:
# - sudo apt-get install magit silversearcher-ag
# - M-x list-packages or
#   M-x package-install
#     ag
#     helm-projectile (with core helm and core projectile pulled by deps)
#     auto-complete
#     adoc-mode
~~~~
