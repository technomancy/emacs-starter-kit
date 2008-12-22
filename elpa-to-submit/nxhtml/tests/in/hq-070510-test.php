
<?php
require_once("include/utils.php");
?>

<div class="linkcontainer">
<?php
$_SESSION["linksfile"]="csv/links.csv";
include("displaylinks.php");
?>
</div>

