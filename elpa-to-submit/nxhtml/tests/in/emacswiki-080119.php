<?php

  /* There was a problem reported 2008-01-19 on
     http://www.emacswiki.org/cgi-bin/wiki/NxhtmlMode. The word
     INCLUDE was highlighted with a different color. However this
     should now work and the word APP_INCLUDE below should all be in
     font-lock-warning-face.

     FIXME: When deleting the first star the first time only the first
     part of the comment is affected.
   */

define("APP_INCLUDE", "/home/app/include/"); include_once(APP_INCLUDE . "file.php");

?>
