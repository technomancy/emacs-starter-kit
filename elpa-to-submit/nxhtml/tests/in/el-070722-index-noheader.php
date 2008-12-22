<?php
/* We must use this header to be correct and for the css validator to
   find our stylesheet without us having to provide a fully qualified
   path (address) to it. */
header("Content-type:application/xhtml+xml; charset=utf-8");
echo '<'.'?xml version="1.0" encoding="utf-8"?'.'>';
?>
  <body>
    <div id="container">
      <div id="header">Top area</div>
      <div id="left-menu">
        <ul>
          <li><a href="index.php">Home</a></li>
          <li><a href="index.php?page=a">First Main Page</a></li>
          <li><a href="index.php?page=b">Second Main Page</a></li>
        </ul>
      </div>
      <div id="main">
        <?php
          if (isset($_GET["page"])) {
            $thepage = $_GET['page'];

            if ($thepage != 'a' && $thepage != 'b') {
              print('You hacker you!');
            }
            else {
              require('main-div-'.$thepage.'.html');
            }
          }
          else {
            require('main-div-a.html');
          }
        ?>
      </div>
      <div id="right-menu">Right area</div>
      <div id="footer">
        <p>
          <a href="http://validator.w3.org/check?uri=referer">
            <img src="valid-xhtml10.png" alt="Valid XHTML 1.0 Strict"></img>
          </a>
          <a href="http://jigsaw.w3.org/css-validator/check?uri=referer">
            <img src="vcss.png" alt="Valid CSS!"></img>
          </a>
        </p>
      </div>
    </div>
  </body>
</html>
