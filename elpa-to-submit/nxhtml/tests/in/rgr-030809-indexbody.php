
<h1><div class="pagetitle"><?php echoMessage("website.title");?></div></h1>

<img class="indeximage1"  src="images/indeximage1.jpg"/>

<div class="textcontainer latestnewscontainer">

  <div class="indexspecialevent">
    <?php $numrows=displaySpecialEvent("nextevent");?>
  </div>

  <div class="pubquizpreview">
    <div>
      <h3>
	<?php echo createLink("pubquiz","pubquiz.comingnext");?>
      </h3>
    </div>
    <div>
      <?php previewEvents("pubquiz");?>
    </div>
  </div>

  <div class="latestnewstitle">
    <?php
      echoMessage("index.latest.title");
    ?>
  </div>
  <div class="latestnewsbody">
    <?php echoMessage("index.latest.body");?>
    <?php include("menu.php");?>
  </div>
</div>

<div class="textcontainer" title="English Books Hamburg">

  <div>
    <?php echoMessage("index.about1");?>
  </div>

  <div>
    <h2>
      <?php echo createLink("general","index.whatsnew");?>
    </h2>
  </div>

  <div>
    <img class="newsimage" src="images/newsimage1.jpg"/>
  </div>

  <div>
    <?php echoMessage("index.news.n1");?>
  </div>
  <div>
    <?php echoMessage("index.news.about2");?>
  </div>

</div>
