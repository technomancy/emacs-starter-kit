
<?php

/* Testing fill paragraph and friends, fill me fill me fill me fill me
   fill me fill me fill me fill me fill me fill me fill me fill me
   fill me fill me

   However coloring it differently than the top level (or level 1) php
   chunks may help detect nesting errors.  */

$name       = "Joe Smith";
$occupation = "Programmer";
echo <<<EOF

 This is a heredoc text-mode section.
 For more information talk to $name, your local $occupation.

EOF;

$toprint = <<< HTMLEOF
<!-- heredoc html-mode section -->
<style type="text/css">
.bugfix { color: red; }
</style>

<script type="text/javascript" language="javascript">

 function onEndCrop( coords, dimensions ) {
     alert("Test");
 }
</script>


<a href="javascript:void window.open('');" title="Something">
  <img   src="/administrator/images/imprimir.png"
  style="color:red;"
  border="0"
  alt="<?php echo _CMN_PDF;?>"
  onmouseover="this.src='images/imprimir_on.png';swap('imprimir',1);"
  onmouseout="this.src='images/imprimir.png'; swap('imprimir',0);"
  class="bot" id="imprimir"/>
  </a>

<?php

/* This inner php chunk is not useful (except for presentation of
   MuMaMo chunk dividing capabilities and deficiences...), since php
   normally seems to run only one pass...

   However coloring it differently than the top level (or level 1) php
   chunks may help detect nesting errors.  */

echo <<<ONEMORELEVEL
Just for testing the chunk background color...
ONEMORELEVEL;
?>

HTMLEOF;
echo strtolower($toprint);

?>
