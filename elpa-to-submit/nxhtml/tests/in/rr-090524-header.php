<?php require_once("utils.php");?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
            "http://www.w3.org/TR/html4/strict.dtd">

<html>

<?php
   if ($_SESSION["CurrentLanguage"]==0){
     $_SESSION["CurrentLanguage"]=$Default_Lang;
   }
?>

  <head>
    
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

    <style type="text/css">@import url(calendar-win2k-1.css);</style>
    <script type="text/javascript" src="jscalendar-1.0/calendar.js"></script>
    <script type="text/javascript" src="jscalendar-1.0/lang/calendar-en.js"></script>
    <script type="text/javascript" src="jscalendar-1.0/calendar-setup.js"></script>
    
    <link type="text/css" href="stylesheet.css" rel="stylesheet" title="Style Sheet" >
    <style type="text/css"><?php echoMessage("admin.css.source");?> </style>

    <link type="text/css" href="custom.css" rel="stylesheet" title="Style Sheet">
    <link type="text/css" href="local.css" rel="stylesheet" title="Style Sheet">

    <link type="text/css" href="print.css" rel="stylesheet" media="print">
    
    <meta name="keywords" content='<?php echoMessage("header.keywords");?>'>
    <meta name="description" content='<?php echoMessage("header.description");?>'>

    <title><?php echoMessage("website.title")?></title>
    
    
  </head>
  
  <body  class="body">
    <div class="wc">
      <table class="language">
<tr>
  <td>
    <?php generateLanguageForm(true);?>
  </td>
  <td><a  href="cyclelanguage"><img class="languageflag" src="flags/<?php echo getCurrentLanguage();?>" alt="<?php echoMessage("language.selectnext");?>" title="<?php echoMessage("language.selectnext");?>"></a></td>
</tr>
      </table>
      
      <?php include("navigation.php");?> 
  <div class="ical">
    <?php  echoMessage("index.ical");?>
  </div>

    <div class="wb">

      <?php 

	if(adminMode()){
	  echo '<div class="admincontrolcontainer clearfix">';
	  createNavigationLinks("navlinks",1);
	  echo '<div class="clear"></div>';
	  echo '</div>';
	}
	
	if(isDemo()){
	  echo '<div class="demomode">';
	  echoMessage("admin.demo");
	  echo "</div>";
	}

      ?>
      
      <?php include("previews.php");?>
      
      <div class="centercontainer clearfix">
