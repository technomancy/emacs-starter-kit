<?php
/*
  '
 */
/*
  Problem updating comments during editing. To reproduce place cursor after the single x above and press RET and some other character.

  Or just edit here, fill the paragraph above etc. Seems like problem
  with mumamo-after-change which assumes that it is only called once
  before post-command-hook is called.

  BTW fill-paragraph does not work either ... - why does it run
  c-fill-paragraph in php-mode? (Mailed Alan about this. The only
  reason seems to be to support filladapt.el, but is that needed any
  more?

  It looks like more code (like filling) needs to be run under the
  correct syntax table etc. Implemented.
 */
?>
