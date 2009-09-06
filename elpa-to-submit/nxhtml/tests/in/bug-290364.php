<?php
include 'HTML/QuickForm.php';

echo '<script type="text/javascript" src="library/javascript/prototype.js" language="javascript"></script>';
echo '<script type="text/javascript" src="library/javascript/scriptaculous.js" language="javascript"></script>';
echo '<script type="text/javascript" src="library/cropper/cropper.js" language="javascript"></script>';



echo '      <img src="test.jpg" alt="Test image" id="testImage" width="500" height="333" />';
       
echo '          <script type="text/javascript" language="javascript">';

echo '      function onEndCrop( coords, dimensions ) {';
// echo '          $( "x1" ).value = coords.x1;';
// echo '          $( "y1" ).value = coords.y1;';
// echo '          $( "x2" ).value = coords.x2;';
// echo '          $( "y2" ).value = coords.y2;';
// echo '          $( "width" ).value = dimensions.width;';
// echo '          $( "height" ).value = dimensions.height;';
// echo 'console.log(coords.x1);';
// echo 'console.log(coords.x2);';
// echo ' style = clip:rect( + coords.x1 + "px " + coords.y1 + "px " + coords.x2 + "px " + coords.y2 + "px)\n"';
?> 
style = "clip:rect(0px 130px 130px 0px)";
$("hidden_0").value = new Array (coords.x1, coords.x2, coords.y1, coords.y2);
$("testImage").writeAttribute("style=" style);
}
function js_init () {
  Event.observe( "button_0", "click", function() {    } );
  //  $("file_0").writeAttribute("src", "")
  
  new Cropper.Img("testImage", {minWidth: 220,previewWrap: "previewWrap", onEndCrop: onEndCrop });
}



Event.observe( window, "load", function() {
    js_init ();
  } );
?>
echo '      </script>';

echo '<h1> Cropper votre image v0.1</h1>';

$label = 'file_default';

$form = new HTML_QuickForm ('cropper_form', "", "img-cropped.php", '', 'enctype=multipart/form-data');

$form->addElement ('file', 'file_0', $label);
$form->addElement ('hidden', 'hidden_0');
$form->addElement ('button', 'button_0');
$form->updateElementAttr(array('button_0'), array ('id' => 'button_0'));
$form->addElement ('submit', 'submit_0', 'Envoyer!');
$form->updateElementAttr(array('file_0'), array ('id' => 'file_0'));
$form->updateElementAttr(array('hidden_0'), array ('id' => 'hidden_0'));
echo $form->toHtml ();

if (isset ($_POST['file_0'])):
$image = $_POST['file_0'];
tmpfile ($image);
echo $_POST['file_0'];
endif;
echo sys_get_temp_dir ();
var_dump ($_POST);
?>