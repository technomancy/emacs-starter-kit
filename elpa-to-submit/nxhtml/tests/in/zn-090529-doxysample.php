<?php
/**
 * @file   doxysample.php
 * @author Zoltán Nagy <abesto0@gmail.com>
 * @date   Fri May 29 15:28:06 2009
 *
 * @brief  Example file commented with Doxygen (via doxymacs)
 *
 * Longer description of what this file is
 * Possibly multiline
 */


/**
 * Echos the parameter
 *
 * @param text Text to echo
 *
 * @return EOL message
 */
function say($text)
{
  echo $text;
  return "I spoketh.";
}
?>

<div>
This is HTML, so the following has no special meaning:
/**
 * Fake comment
 *
 * @param whatever Foo
 *
 * @return bar
 */
</div>
