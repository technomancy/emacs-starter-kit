<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <title>Lab 2 - Layout Control - Task 2 - XHTML/CSS version</title>
  </head>
  <body>
    <?php
      // comment
      $thepage = $_GET['page'];

      if (empty($thepage)) {
        require('main-div-a.html');
      }
      else {
        if ($thepage != 'a' && $thepage != 'b') {
          print('You hacker you!');
        }
        else {
          require('main-div-'.$thepage.'.html');
        }
        for (;;) {
        }
      }
    ?>
  </body>
</html>
