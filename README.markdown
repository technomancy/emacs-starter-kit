# Emacs Starter Kit

This should provide a saner set of defaults than you get normally with
Emacs. It's intended for beginners, but it should provide a reasonable
working environment for anyone using Emacs for dynamic languages. The
main advantage of the Starter Kit is that it provides better default
settings and bundles many useful libraries.

## Learning

This won't teach you Emacs, but it'll make it easier to get
comfortable. To access the tutorial, press control-h followed by t.

You may also find the [PeepCode Meet Emacs
screencast](http://peepcode.com/products/meet-emacs) helpful.

## Installation

1. Install Emacs (at least version 22)
   Use your package manager if you have one.
   Otherwise, Mac users should get it [from Apple](http://www.apple.com/downloads/macosx/unix_open_source/carbonemacspackage.html).
   Windows users can get it [from GNU](http://ftp.gnu.org/gnu/emacs/windows/emacs-22.3-bin-i386.zip).
2. Move the directory containing this file to ~/.emacs.d
3. Launch Emacs!

If you are missing some autoloads after an update (should manifest
itself as "void function: foobar" errors) try M-x regen-autoloads.

If you want to keep your regular ~/.emacs.d in place and just launch a
single instance using the starter kit, try the following invocation:

  $ emacs -q -l ~/src/emacs-starter-kit/init.el

## ELPA

Libraries from ELPA (http://tromey.com/elpa) are preferred when
available since dependencies are handled automatically, and the burden
to update them is removed from the user. In the long term, ideally
everything would be installed via ELPA, and only package.el would need
to be distributed with the starter kit. (Or better yet, package.el
would come with Emacs...) See starter-kit-elpa.el for a list of
libraries that are pending submission to ELPA.

## Contributing

If you know your way around Emacs, please try out the starter kit as a
replacement for your regular dotfiles for a while. If there's anything
you just can't live without, add it or let me know so I can add
it. Take a look at what happens in init.el to get started.

Also: see the file TODO. Helping submit new libraries to ELPA is the
easiest way to help out.

The latest version is at http://github.com/technomancy/emacs-starter-kit/
