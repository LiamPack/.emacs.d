# just another .emacs.d
Just my emacs config. To see the deprecated literate-programming
document form of my configuration, see [config.org](config.org), which
may or may not be out of date now. Lisp and configuration is defined
in [lisp/](lisp/).

I emphasize simple utilities and packages. I rely on many of the
wonderful, but perhaps not-as-well-explored, built-in packages
(=desktop=, =dabbrev= and =abbrev=, =xref=, =isearch=, etc.), and some
external-but-necessary packages (=embark=, =consult=, =eglot=,
=orderless=, programming-language specific, etc.). Simple packages
frequently permit composition: they generally promise that they won't
step on their neighbors' toes, which reduces the cognitive overhead of
evaluating, say, which search+jump function to use and whether it will
react well to the current environment.

More complex packages are no doubt useful and have their place
(e.g. Helm, Ivy). In the past, these packages were a godsend due to
how easy it was for me to set up Emacs and be immediately
effective. As time went on, however, I found that I didn't understand
my tools well enough; I always looked for a solution that was "install
this new package" rather than "fix the underlying problem yourself
with the tools you have."

Without discipline restricting me from installing new packages, I was
incessantly yak-shaving for the best possible package for increasingly
niche situations. Keeping a simple-and-preferred-built-in approach
forces me to think twice about whether I really need the new shiny
docker buffer editing package, or a SCSS-specific color-mapping
mode. An end-goal may be to someday rely on no external packages, but
that doesn't seem likely unless =orderless= somehow makes it into
=emacs.git=.

Inspiration is, as always, taken from those more experienced and
(much) more wise than myself. Credits to the (many) philosophies and
configurations I took inspiration from over the years which are linked
in a final section.
