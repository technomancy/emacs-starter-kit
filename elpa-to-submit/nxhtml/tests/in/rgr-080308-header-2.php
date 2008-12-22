<?php require_once("utils.php"); ?>

<?php echo '<?xml version="1.0" encoding="utf-8"/?/>'; ?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

<head>

  <link type="text/css" href="stylesheet.css" rel="stylesheet" title="Style Sheet" />
  <link type="text/css" href="custom.css" rel="stylesheet" title="Style Sheet" />

  <title><?php echoMessage("website.title")?></title>

  <meta name="keywords" content="irish bar,irish pub,finnegans wake , hamburg"/>
  <meta  name="description" content="Finnegans Wake Irish Pub"/>

  <style type="text/css">@import url(calendar-win2k-1.css);</style>
  <script type="text/javascript" src="jscalendar-1.0/calendar.js"></script>
  <script type="text/javascript" src="jscalendar-1.0/lang/calendar-en.js"></script>
  <script type="text/javascript" src="jscalendar-1.0/calendar-setup.js"></script>


</head>

<body  class="body" title="Irish Bar">

  <div class="webcontainer">

    <div class="headerbanner">
      <img src="images/banner.gif" alt="Irish Bar"/>
    </div>

    <div class="logo">
      <img src="images/logo.gif" alt="Irish Pubs"/>
    </div>

    <div>
      <?php include("navigation.php");?>
    </div>

    <?php
      if(isDemo()){
	      echo '<div class="demomode">';
	      echoMessage("admin.demo");
	      echo "</div>";
      }

      include("address.php");
      include("sportpreview.php");

    ?>

    <div class="centercontainer" title="Irish Pubs Germany">
