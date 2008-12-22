<?php

// some basic library functions
include_once 'lib.php';

$book = new Mybook($api_key, $secret);

if (isset($_POST['to'])) {
  $prints_id = (int)$_POST['to'];
  $prints = do_step($user, $prints_id);
} else {
  if (isset($_GET['to'])) {
    $prints_id = (int)$_GET['to'];
  } else {
    $prints_id = $user;
  }
  $prints = get_prints($prints_id);
}

?>
<div style="padding: 10px;">
  <h2>Hi <mb:name firstnameonly="true" uid="<?php=$user?>" useyou="false"/>!</h2><br/>
  <a href="<?= $book->get_add_url() ?>">Put prints in your profile</a>.
    <form method="post" action="http://my-domain.com/footprints/">
<?php
      if ($prints_id != $user) {
        echo '<input type="hidden" name="to" value="' . $prints_id . '"/>';
      } else {
        echo '<br/>';
      }
?>
      <input value="step" type="submit"/>
    </form>
  <hr/>
  These are <mb:name uid="<?= $prints_id ?>" possessive="true"/> Footprints:<br/>
  <?php echo render_prints($prints, 10); ?>
  <div style="clear: both;"/>
</div>
