<?php
// hotproperty.html.php
/**
 * Something's Presentation Code
 *
 * @package Something
 * @copyright (C) 2004 Lee Cher Yeong
 * @url http://www.somewhere.com/
 * @author Lee Cher Yeong <cy@somewhere.com>
 **/

defined( '_VALID_MOS' ) or die( 'Direct Access to this location is not allowed.' );

class hotproperty_HTML {

	/***
	 * Include CSS file
	 ***/
	function include_CSS() {
		global $hp_css;
	?>
   <link rel="stylesheet" href="components/com_hotproperty/css/<?php echo $hp_css; ?>.css" type="text/css">
	<?php
	}

	function include_Container_Start() {
	?>
<div id="con_global">
<div class="componentheading"><!--<?php echo _HP_COM_TITLE; ?>--></div>
	<?php
	}
	function include_Container_End() {
	?>
</div>


	<?php
	}


	/***
	 * Front Page
	 ***/
	function show_Frontpage(&$campos, &$featured, &$featured_fields_caption, &$featured_total, &$types, &$types_hotproperty, &$types_total) {
          global $hp_fp_show_featured, $hp_fp_show_search, $mainframe, $hp_css, $mosConfig_live_site, $my;

          $mainframe->setPageTitle( _HP_COM_TITLE );?>

          <link rel="stylesheet" href="components/com_hotproperty/css/ppal_<?php echo $hp_css; ?>.css" type="text/css"/>

          <div id="total_alojamientos">
             <span class="saludo"><?= _HP_HOLA ?></span> <?= _HP_TENEMOS ?>
             <?php hotproperty_HTML::show_ResumenTipos() ?>
          </div>

          <div id="buscar_alojamiento">
          </div>
          <div id="incluir_alojamiento">
          </div>

          <br clear="all"/>

<?php

/*		if ($hp_fp_show_featured && count($featured) > 0 ){
			hotproperty_HTML::show_fp_Featured($featured, $featured_fields_caption, $featured_total);
			echo '<br class="clearboth" />';
		}

		if ($hp_fp_show_search) {
			hotproperty_HTML::show_Search($types);
			echo '<br class="clearboth" />';
		}

		hotproperty_HTML::show_Types($types, $types_hotproperty, $types_total);*/
	}

	/***
	 * Featured Listing
	 ***/
	function show_fp_Featured(&$prop, &$caption, $featured_total) {
		global $hp_fp_featured_count, $Itemid;
	?>
	<div id="con_featured1">
	<div id="heading_Featured"><?php echo _HP_FEATURED_TITLE; ?></div>
		<div id="list_featured">
		<?php 	hotproperty_HTML::list_properties($prop, $caption); ?>
		<br class="clearboth" />
		<?php if ($featured_total > $hp_fp_featured_count) {
			echo "<a href=\"". sefRelToAbs("index.php?option=com_hotproperty&task=viewfeatured&Itemid=$Itemid") ."\">"._HP_MOREFEATURED."</a>";
		} ?>
		</div>
	</div>
	<?php
	}

	/***
	 * List Types
	 ***/
	function show_Types(&$types, &$types_hotproperty, $types_total) {
		global $Itemid;
	?>
	<div id="con_types1">
	<div id="heading_Types"><?php echo _HP_TYPES_TITLE; ?></div>
		<div id="con_types2">
		<?php
			foreach($types AS $t) {
				if ($types_total[$t->id]->total > 0) {
				?>
				<div class="con_types3">
				<a class="types_title" href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewtype&id=$t->id&Itemid=$Itemid"); ?>"><?php echo $t->name ."</a>(".$types_total[$t->id]->total.")"; ?><br />
				<div class="types_desc">
					<?php echo $t->desc; ?>
					<ul class="types_hp">
					<?php
						foreach($types_hotproperty[$t->id] AS $t_hp) {
							if ($t_hp->name <> "" && $t_hp->id <> "") {
						?>
						 <li><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=view&id=$t_hp->id&Itemid=$Itemid"); ?>"><?php echo $t_hp->name; ?></a></li>
						<?php
							}
						}
					?>
					</ul>
					<?php
						if ($types_total[$t->id]->total > 3) { ?>
					<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewtype&id=$t->id&Itemid=$Itemid"); ?>"><?php echo _HP_MORE; ?></a>
					<?php
					}
					?>
				</div>
				</div>
				<?php
				}
			}
		?>
		</div>
	</div>
	<?php
	}

	/***
	 * Search Facility
	 ***/
	function show_Search(&$types) {
		global $Itemid, $hp_use_advsearch, $mosConfig_live_site;
		/*
		global $custom404, $mosConfig_sef, $sufix;

		# Using Built in SEF feature in Mambo
		if ( !isset($custom404) && $mosConfig_sef ) {
			$onclickCmd = "document.location.href= '$mosConfig_live_site/component/option,com_hotproperty/task,search/Itemid,$Itemid/type,0/search,' + encodeURI(document.searchfrm.hp_search.value) + '/'";
		} elseif ( $mosConfig_sef && isset($custom404) && !empty($sufix) ) {

			global $custom_comp, $hp_default_limit_search;
			$hotproperty = "hotproperty";
			if (in_array("hotproperty", $custom_comp)) {
				$hotproperty = array_search($component_name, $custom_comp);
			}

			$onclickCmd = "document.location.href='" . $hotproperty . "/" . _HP_SEF_SEARCH . "/0/".$hp_default_limit_search."/0/" . "' + encodeURI(document.searchfrm.hp_search.value)";

		} else {
			# Using SEF advance or no SEF at all
			$onclickCmd = "document.location.href='" . sefRelToAbs("index.php?option=com_hotproperty&task=search&Itemid=$Itemid&type=0&search=' + encodeURI(document.searchfrm.hp_search.value)");
		}
		*/
	?>
	<div id="con_search1">
	<div id="heading_Search"><?php echo _HP_SEARCH_TITLE; ?></div>
		<div id="con_search2">
			<form action="index.php" method="POST" name="searchfrm">
			<strong><?php echo _HP_SEARCH_TEXT; ?></strong>
			<input type="text" name="search" class="inputbox" />
			<input type="submit" class="button" value="<?php echo _HP_SEARCH; ?>" /><?php
			if ($hp_use_advsearch == '1') {
			?>&nbsp;<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=advsearch&Itemid=$Itemid"); ?>"><?php echo _HP_SEARCH_ADV_TITLE; ?></a><?php
			}
			?>
			<input type="hidden" name="option" value="com_hotproperty" />
			<input type="hidden" name="task" value="search" />
			<input type="hidden" name="Itemid" value="<?php echo $Itemid;?>" />
			</form>
		</div>
	</div>
	<?php
	}

	/***
	 * Advanced Search Facility
	 ***/
	function show_AdvSearch($fields, $tipos_renta) {
		global $Itemid, $mainframe;

		$mainframe->setPageTitle( _HP_SEARCH_ADV_TITLE );
	?>
        <script language="javascript">
           /* Selecciona por defecto España como país para la búsqueda */
           function select_idioma(id) {
                  select=document.getElementById(id);
                  select.options[62].selected="1";
           }
           /* Muestra o oculta la búsqueda por disponibilidad y el precio según el tipo de oferta */
           function swap_tipo_oferta(id) {
                  select=document.getElementById(id);
                  disp=document.getElementById('disp_busq_av');
                  precio=document.getElementById('precio_busq_av');
                  if (select.options[1].selected) { // Alquiler
                      disp.style.display = "block";
                      precio.style.display = "none";
                  }
                  if (select.options[2].selected) { // Venta
                      disp.style.display = "none";
                      precio.style.display = "block";
                  }
                  if (select.options[0].selected) { // Cualquiera
                      disp.style.display = "block";
                      precio.style.display = "block";
                  }


           }
           /* Muestra o oculta una capa, y cambia el texto del enlace lanzador */
           function swap(id, llamador)
           {
             id=document.getElementById(id);
             if (id.style.display == "none" || id.style.display == "") {
                id.style.display = "block";
                llamador.innerHTML = "<?= _HP_OCULTAR ?>";
             }
             else {
                id.style.display = "none";
                llamador.innerHTML = "<?= _HP_MOSTRAR ?>";
             }
           }
           /* Selecciona todos los alojamientos O sólo las viviendas en un combobox con los tipos de alojamiento */
           function selecc_vivienda(select)
           {
               select = document.getElementById(select);
               if (select.selectedIndex == 1)
               {
                 select.options[select.selectedIndex].selected = "";
                  for (i=2; i <= select.length-1; i++)
                      if (i != 11 && i!= 13) select.options[i].selected="1";
               }
           }
           /* Muestra u oculta los campos de búsqueda por disponibilidad */
           function swap_disp(input,id) {
               if (input.checked)
                   document.getElementById(id).style.display="block";
               else
                   document.getElementById(id).style.display="none";
           }
        </script>
	<div id="heading_AdvSearch"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo _HP_SEARCH_ADV_TITLE; ?></div>
                <div id="con_asearch1">
			<form action="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=asearch&Itemid=$Itemid"); ?>" method="POST" name="searchfrm">
                          <div class="cabecera_busq_avzda">
                             <div class="titulo_cabecera_busq_avzda"><?= _HP_DATOS_GENERALES ?></div>
                          </div><br class="clearboth"/>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['type']->caption; ?>:</div>
                             <?= $fields['type']->input; ?>
                          </div>
                          <div class="cont_form">
                             <div class="campo_con_ayuda">
                               <div class="titulo_campo"><?= $fields['Tipo_Alojamiento']->caption; ?>:</div>
                               <?= $fields['Tipo_Alojamiento']->input; ?>
                             </div>
                             <div class="msj_ayuda"><?= _HP_AYUDA_TIPO_ALOJ ?></div>
                          </div>
                          <br class="clearboth"/>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['Superficie_habitable']->caption; ?>:</div>
                             <?php echo $fields['Superficie_habitable']->input; ?>
                             <?php echo $fields['Superficie_habitable']->append_text; ?>
                          </div>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['Num_plazas']->caption; ?>:</div>
                             <?= $fields['Num_plazas']->input; ?>
                          </div>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['Numero_dormitorios']->caption; ?>:</div>
                             <?= $fields['Numero_dormitorios']->input; ?>
                          </div>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['country']->caption; ?>:</div>
                             <?= $fields['country']->input; ?>
                             <script language="javascript">
                                select_idioma('idioma');
                             </script>
                          </div>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['state']->caption; ?>:</div>
                             <?= $fields['state']->input; ?>
                          </div>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['suburb']->caption; ?>:</div>
                             <?= $fields['suburb']->input; ?>
                          </div>
                          <div class="cont_form">
                             <div class="titulo_campo"><?= $fields['postcode']->caption; ?>:</div>
                             <?= $fields['postcode']->input; ?>
                          </div>
                          <div id="precio_busq_av">
                            <div class="cont_form">
                              <div class="titulo_campo"><?= $fields['price']->caption; ?>:</div>
                              <?= $fields['price']->input; ?>
                            </div>
                          </div>
                          <div id="disp_busq_av">
                            <div class="cont_form">
                             <input type="checkbox" name="busq_disp" onclick="swap_disp(this,'campos_disp');"><?= _HP_BUSQ_POR_DISP ?></input>
                             <div id="campos_disp">
                               <div class="titulo_campo"><?= _HP_BUSQ_DISP ?> </div>
                               <div class="flota_izq interl_doble">
                                 <?php hotproperty_HTML::seleccion_fecha("desde"); ?>
                                 <?php hotproperty_HTML::seleccion_fecha("hasta"); ?>
                               </div>
                               <div class="campo_avl">
                                 <?= _HPAVL_RENTA; ?>
				 <select size="1" class="campo_dcha inputbox" name="renta">
                                   <option value="0"><?= _HP_SEARCH_ALLTYPES ?></option>
				   <?php foreach ($tipos_renta AS $tipo_renta) { ?>
				   <option value="<?php echo $tipo_renta->id; ?>"><?php echo $tipo_renta->nombre; ?></option>
                                   <?php } ?>
				 </select>
                               </div>
                             </div>
                            </div>
                           </div>
                          <!-- Otros datos -->
                          <div class="cabecera_busq_avzda">
                             <div class="titulo_cabecera_busq_avzda"><?= _HP_OTROS_DATOS ?></div>
                             <div class="botonera_dcha">
                             <a href="#" class="enlace_blanco" onclick="swap('otros_datos', this);"><?= _HP_MOSTRAR; ?></a>
                             </div>
                          </div><br class="clearboth"/>
                          <div id="otros_datos">
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['MetrosConstruidos']->caption; ?>:</div>
                               <?= $fields['MetrosConstruidos']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Superficie_parcela']->caption; ?>:</div>
                               <?= $fields['Superficie_parcela']->input; ?>
                               <?= $fields['Superficie_parcela']->append_text; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Numero_dormitorios']->caption; ?>:</div>
                               <?= $fields['Numero_dormitorios']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Cuartos_banio_con_duchas']->caption; ?>:</div>
                               <?= $fields['Cuartos_banio_con_duchas']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Aseos']->caption; ?>:</div>
                               <?= $fields['Aseos']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['AnioConstruccion']->caption; ?>:</div>
                               <?= $fields['AnioConstruccion']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['UltimaReforma']->caption; ?>:</div>
                               <?= $fields['UltimaReforma']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Amueblado']->caption; ?>:</div>
                               <?= $fields['Amueblado']->input; ?>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Dispone']->caption; ?>:</div>
                               <?= $fields['Dispone']->input; ?>
                               <br class="clearboth"/>
                             </div>
                             <div class="cont_form">
                               <div class="titulo_campo"><?= $fields['Zona']->caption; ?>:</div>
                               <?= $fields['Zona']->input; ?>
                               <br class="clearboth"/>
                             </div>
                          </div>

                          <br/>
                          <div class="centro">
                          <input type="submit" value="Buscar" class="button" >
                          </div>

                 </div>

	<?php
	}

	/***
	 * Empty Advanced Search Results
	 ***/
	function show_advSearchResults_error($msg) {
		global $Itemid, $mainframe;
		$mainframe->setPageTitle( _HP_SEARCH_ADV_TITLE );
	?>
	<div id="con_asearch1">
	<div id="heading_AdvSearch"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo _HP_SEARCH_ADV_TITLE; ?></div>
		<p />
		<?php echo $msg; ?>
		<p />
		<a href=""><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=advsearch&Itemid=$Itemid"); ?>"><?php echo _HP_SEARCH_TRYAGAIN; ?></a>
	</div>
	<?php
	}

	/***
	 * Advanced Search Results
	 ***/
	function show_advSearchResults(&$search_id, &$prop, &$caption, &$pageNav, &$searchString) {
		global $Itemid, $mainframe;
		$mainframe->setPageTitle( _HP_SEARCH_ADV_TITLE );
	?>
	<div id="con_asearch1">
	<div id="heading_AdvSearch"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo _HP_SEARCH_ADV_TITLE; ?></div>
	<!-- <div id="hp_searchresult_con">
		<div id="hp_search_pagecounter_top">
			<div class="right"><?php echo $pageNav->writePagesCounter(); ?></div>
			<br class="clearboth" />
			<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=asearch&Itemid='.$Itemid.'&search_id='.$search_id); ?>
		</div>
	</div> -->
	<div id="list_searchresults">
		<?php 	hotproperty_HTML::list_properties($prop, $caption); ?>
	</div>
	</div>
	<br class="clearboth" />
			<div id="hp_search_pagecounter_bottom">
			<div class="dcha"><?php echo $pageNav->writePagesCounter(); ?></div>
		<br class="clearboth" />
			<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=asearch&Itemid='.$Itemid.'&search_id='.$search_id); ?>
	</div>

	<?php
	}

	/***
	 * Search Results
	 ***/
	function show_SearchResults(&$types, &$prop, &$caption, &$pageNav, &$searchString) {
		global $Itemid, $hp_use_advsearch, $mosConfig_live_site, $mainframe;
		global $custom404, $mosConfig_sef, $sufix;
		$mainframe->setPageTitle( _HP_SEARCH_RESULT_TITLE );

		# Using Built in SEF feature in Mambo
		/*
		if ( !isset($custom404) && $mosConfig_sef ) {

			$onclickCmd = "document.location.href= '$mosConfig_live_site/component/option,com_hotproperty/task,search/Itemid,$Itemid/type,' + document.searchfrm.type.options[document.searchfrm.type.selectedIndex].value + '/search,' + encodeURI(document.searchfrm.hp_search.value) + '/'";

		} elseif ( $mosConfig_sef && isset($custom404) && !empty($sufix) ) {

			global $custom_comp, $hp_default_limit_search;
			$hotproperty = "hotproperty";
			if (in_array("hotproperty", $custom_comp)) {
				$hotproperty = array_search($component_name, $custom_comp);
			}

			$onclickCmd = "document.location.href='" . $hotproperty . "/" . _HP_SEF_SEARCH . "/' + document.searchfrm.type.options[document.searchfrm.type.selectedIndex].value + '/".$hp_default_limit_search."/0/" . "' + encodeURI(document.searchfrm.hp_search.value)";

		} else {

		# Using SEF advance or no SEF at all
			$onclickCmd = "document.location.href='" . sefRelToAbs("index.php?option=com_hotproperty&task=search&Itemid=$Itemid&type=' + document.searchfrm.type.options[document.searchfrm.type.selectedIndex].value + '&search=' + encodeURI(document.searchfrm.hp_search.value)");
		}
		*/
	?>
	<div id="con_search1">
	<form action="index.php" method="POST" name="searchfrm">
	<div id="heading_Search"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN ." "._HP_ARROW." "; ?></a><?php echo _HP_SEARCH_RESULT_TITLE; ?></div>
		<div id="hp_searchresult_con">
			<strong><?php echo _HP_SEARCH_TEXT; ?></strong>
			<input type="text" name="search" class="inputbox" value="<?php echo $searchString->search; ?>" /> <?php echo _HP_IN; ?>
			<select name="type" class="inputbox" size="1">
				<option value="0"><?php echo _HP_SEARCH_ALLTYPES; ?></option>
			<?php
			foreach($types AS $t) { ?>
				<option value="<?php echo $t->id; ?>"<?php echo (($searchString->type==$t->id) ? " selected" : ""); ?>><?php echo $t->name; ?></option>
			<?php	}
		?></select>
			&nbsp;
			<input type="submit" class="button" value="<?php echo _HP_SEARCH; ?>" /><?php
			if ($hp_use_advsearch == '1') {
			?>&nbsp;<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=advsearch&Itemid=$Itemid"); ?>"><?php echo _HP_SEARCH_ADV_TITLE; ?></a><?php
			}
			?>
			<div id="hp_search_pagecounter_top">
			<div class="right"><?php echo $pageNav->writePagesCounter(); ?></div>
				<br class="clearboth" />
				<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=search&Itemid='.$Itemid.'&type='.$searchString->type.'&search='.$searchString->search); ?>
			</div>
		</div>
		<input type="hidden" name="option" value="com_hotproperty" />
		<input type="hidden" name="task" value="search" />
	</form>
	</div>

	<div id="list_searchresults">
		<?php 	hotproperty_HTML::list_properties($prop, $caption); ?>
	</div>
	<br class="clearboth" />
			<div id="hp_search_pagecounter_bottom">
			<div class="right"><?php echo $pageNav->writePagesCounter(); ?></div>
		<br class="clearboth" />
		<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=search&Itemid='.$Itemid.'&type='.$searchString->type.'&search='.$searchString->search); ?>
	</div>

	<?php
	}

	/***
	 * List Properties for a particular Type
	 ***/
	function show_Type($prop, $type, $caption, $pageNav, $sortby_sort, $sortby_order) {
		global $hp_use_diplaynum, $hp_use_sort_name, $hp_use_sort_agent, $hp_use_sort_price, $hp_use_sort_suburb, $hp_use_sort_state, $hp_use_sort_country, $hp_use_sort_type, $hp_use_sort_modified, $hp_use_sort_hits;

		global $Itemid, $database, $mainframe;

		$mainframe->setPageTitle( $type->name );
		?>
	<div id="con_type1">
           <?php if ($type != '') { ?>
		<div id="heading_Type"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo $type->name; ?></div>
           <?php } ?>
		<?php
			if ($hp_use_diplaynum == '1' || !empty($hp_use_sort_name) || !empty($hp_use_sort_agent) || !empty($hp_use_sort_price) || !empty($hp_use_sort_suburb) || !empty($hp_use_sort_state) || !empty($hp_use_sort_country) || !empty($hp_use_sort_type) || !empty($hp_use_sort_modified) || !empty($hp_use_sort_hits))	{
		?>
		<div id="con_sort">
			<?php if ($hp_use_diplaynum == '1') { ?>
			<div id="con_sort1">
				<?php echo _PN_DISPLAY_NR; ?>
				<?php echo $pageNav->writeLimitBox('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort='.$sortby_sort.'&order='.$sortby_order.'&Itemid='.$Itemid); ?>
			</div>
			<?php
						}
						if (!empty($hp_use_sort_name) || !empty($hp_use_sort_agent) || !empty($hp_use_sort_price) || !empty($hp_use_sort_suburb) || !empty($hp_use_sort_state) || !empty($hp_use_sort_country) || !empty($hp_use_sort_type) || !empty($hp_use_sort_modified) || !empty($hp_use_sort_hits)) {
			?>
			<div id="con_sort2">&nbsp;
				<?php echo _HP_SORT_BY; ?>
				|

				<?php if (!empty($hp_use_sort_name)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=name&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>" title="<?php echo _HP_SORT_A_Z; ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo _HP_SORT_AZ; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=name&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>" title="<?php echo _HP_SORT_Z_A; ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_agent) && !empty($caption['agent']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=agent&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['agent']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=agent&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_price) && !empty($caption['price']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=price&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>" title="<?php echo _HP_SORT_LOWEST_PRICE; ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['price']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=price&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>" title="<?php echo _HP_SORT_HIGHEST_PRICE; ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_suburb) && !empty($caption['suburb']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=suburb&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['suburb']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=suburb&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_state) && !empty($caption['state']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=state&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['state']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=state&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_country) && !empty($caption['country']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=country&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['country']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=country&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_type) && !empty($caption['type']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=type&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['type']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=type&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_modified) && !empty($caption['modified']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=modified&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['modified']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=modified&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				<?php if (!empty($hp_use_sort_hits) && !empty($caption['hits']->caption)) { ?>
				<a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=hits&order=asc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_ASC; ?> </a> <?php echo $caption['hits']->caption; ?> <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort=hits&order=desc&limit='.$pageNav->limit.'&limitstart='.$pageNav->limitstart.'&Itemid='.$Itemid); ?>"> <?php echo _HP_SORT_DESC; ?> </a> |
				<?php } ?>

				</div>
			<?php } ?>
		</div>
		<?php } ?>
		<div id="list_properties">
			<?php 	hotproperty_HTML::list_properties($prop, $caption); ?>
		</div>


           <?php if ($type != '') { ?>
		<div id="hp_pagecounter_bottom">
			<div align="right"><?php $pageNav->writePagesCounter(); ?></div>
			<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=viewtype&id='.$prop[0]->typeid.'&sort='.$sortby_sort.'&order='.$sortby_order.'&Itemid='.$Itemid); ?>
		</div>
                <?php    } ?>
        </div>
		<?php
	}

	/***
	 * List Featured Properties
	 ***/
	function show_Featured($prop, $caption, $pageNav) {
		global $Itemid, $mainframe;
		$mainframe->setPageTitle( _HP_FEATURED );
		?>
	<div id="con_type1">
		<div id="heading_Featured"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN ." "._HP_ARROW." "; ?></a><?php echo _HP_FEATURED; ?></div>
		<div id="list_properties">
			<?php 	hotproperty_HTML::list_properties($prop, $caption); ?>
		</div>
		<br class="clearboth" />
		<div id="hp_pagecounter_bottom">
			<div align="right"><?php echo $pageNav->writePagesCounter(); ?></div>
			<br class="clearboth" />
			<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=viewfeatured'); ?>
		</div>
	</div>
		<?php
	}

	/***
	 * Display Company's contact details and list all agents under it.
	 ***/
	function show_Company(&$company, &$agent, &$prop, &$caption, $pageNav) {
		global $mosConfig_live_site, $hp_imgdir_agent, $mainframe, $Itemid;
		$mainframe->setPageTitle( $company[0]->name );
		?>

		<div id="hp_view_agent_title_nav"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em> <?php echo _HP_CO_TITLE; ?></div>
		<div id="hp_view_co_con">
			<?php hotproperty_HTML::show_CoInfo($company, $agent) ?>
		</div>
		<br class="clearboth" />

		<div id="heading_Agent"><span class="flecha_big">› </span><?= _HP_PROPBYAGENT.$company[0]->name; ?></div>
		<div id="list_properties">
		<?php 	hotproperty_HTML::list_properties($prop, $caption); ?>
		</div>

		<br class="clearboth" />
		<div id="hp_pagecounter_bottom">
			<div align="right"><?php echo $pageNav->writePagesCounter(); ?></div>
		<br class="clearboth" />
		<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=viewco&id='.$company[0]->id); ?>
		</div>

		<?php
	}

	/***
	 * Display Agent's information and list all properties under it.
	 ***/
	 function show_Agent($prop_alq, $prop_vta, $caption, $agent, $types, $pageNav) {
		 global $mosConfig_live_site, $hp_imgdir_agent, $Itemid, $mainframe;
		$mainframe->setPageTitle( $agent->name );
		?>

		<div id="hp_view_agent_title_nav"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo $agent->name; ?></div>

		<div id="hp_view_agent_con">
		<?php hotproperty_HTML::show_AgentInfo($agent) ?>
		</div>
		<br class="clearboth" />

		<div id="heading_Agent"><span class="flecha_big">› </span><?= _HP_PROPBYAGENT.$agent->name; ?></div>
                   <div class="cabecera_ver_ofertas"><?= _OFER_ALQ; ?></div>
		       <?php hotproperty_HTML::show_Type($prop_alq, "", $caption, $pageNav, "desc", "modified"); ?>
		   <br class="clearboth" />
                   <div class="cabecera_ver_ofertas"><?= _OFER_VEN; ?></div>
                       <?php hotproperty_HTML::show_Type($prop_vta, "", $caption, $pageNav, "desc", "modified"); ?>


<!--		<div id="hp_pagecounter_bottom">
			<div align="right"><?php echo $pageNav->writePagesCounter(); ?></div>
		<br class="clearboth" />
		<?php echo $pageNav->writePagesLinks('index.php?option=com_hotproperty&task=viewagent&id='.$agent->id); ?>
		</div>-->
		<br class="clearboth" />
		<?php
	}

	/***
	 * Display Company's information and an enquiry form.
	 ***/
	function show_CoEmail($company) {
		global $Itemid, $mainframe;
		$mainframe->setPageTitle( _HP_CO_CONTACT .' - '.$company[0]->name );

		?>
		<div id="heading_Co"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo _HP_CO_TITLE; ?></div>
		<div id="hp_view_co_con">
			<?php hotproperty_HTML::show_CoInfo($company) ?>
		</div>
		<br class="clearboth" />
		<div id="heading_Co_Contact"><?php echo _HP_CO_CONTACT; ?></div>
		<div id="hp_emailform_con">
			<?php hotproperty_HTML::show_EmailForm('company',$company[0]->id) ?>
		</div>

		<?php
	}

	/***
	 * Display Agent's information and an enquiry form.
	 ***/
	function show_AgentEmail($agent) {
		global $Itemid, $mainframe;
		$mainframe->setPageTitle( _HP_VIEW_AGENT_CONTACT .' - '.$agent[0]->name );
		?>
		<!--<div id="hp_view_agent_title_nav"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?><em><?php echo _HP_ARROW; ?></em></a> <a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewco&id=".$agent[0]->companyid."&Itemid=".$Itemid); ?>"><?php echo $agent[0]->company; ?></a><em><?php echo _HP_ARROW; ?></em><?php echo _HP_VIEW_AGENT_TITLE; ?></div> -->
<!--		<div id="hp_view_agent_con">
			<?php hotproperty_HTML::show_AgentInfo($agent) ?>
		</div> -->
		<div id="hp_view_agent_contact"><span class="flecha_big"><?= _HP_ARROW ?></span><?php echo _HP_VIEW_AGENT_CONTACT; ?></div>
		<div id="hp_emailform_con">
                <?php hotproperty_HTML::show_EmailForm('agent',$agent[0]->id) ?>
		</div>

		<?php
	}

	/***
	 * Show Property
	 ***/
	function show_Prop(&$prop, &$caption, &$images, &$agent, $num_periodos) {
		global $Itemid, $my, $mosConfig_live_site, $mosConfig_absolute_path, $pop, $mainframe;
		global $hp_imgdir_thumb, $hp_imgdir_standard, $hp_currency, $hp_imgsize_thumb, $hp_img_noimage_thumb, $hp_imgdir_agent, $hp_show_agentdetails, $hp_show_enquiryform, $hp_thousand_sep, $hp_dec_point, $hp_link_open_newwin, $hp_show_moreinfo, $hp_use_companyagent, $hp_dec_string, $hp_thousand_string;
		global $hp_show_pdficon, $hp_show_printicon, $hp_show_emailicon;

		$mainframe->appendMetaTag( 'description', $prop[0]->metadesc );
		$mainframe->appendMetaTag( 'keywords', $prop[0]->metakey );
		$mainframe->setPageTitle( $prop[0]->name );
                if ($pop == '') $pop = 0;
		?>
        <script language="javascript">
           // Si es una pop-up, es la ventana de impresión: imprime.
           if (<?= $pop ?> == 1 ) { window.print(); }
         </script>
         <script src="components/com_hotproperty/js/resalte.js" type="text/javascript"></script>

	<div id="con_hp1">
            <?php if (!$pop) { ?>
      	    <div id="heading_Prop">
              <div class="obj_con_botonera">
               <div class="nombre_objeto">
		<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&Itemid=$Itemid"); ?>"><?php echo _HP_COM_PATHWAY_MAIN; ?></a><em><?php echo _HP_ARROW; ?></em><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewtype&id=".$prop[0]->typeid."&Itemid=$Itemid"); ?>"><?php echo $prop[0]->type; ?> </a> <em><?php echo _HP_ARROW; ?></em> <?php echo $prop[0]->name; ?>
                </div>
             <?php } ?>
                <div class="botonera_dcha">
                <div class="mini_botones">

		<?php

		# Show edit icon for authorized agent
		if (!$pop && $prop[0]->user == $my->id && $prop[0]->user > 0 && $my->id > 0) { ?>
                 <div class="mini_boton">
                   <a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=editprop&id=". $prop[0]->id ."&Itemid=$Itemid"); ?>" title="<?= _E_EDIT; ?>"><img src="administrator/images/editar.png" alt="<?= _E_EDIT ?>" title="<?= _E_EDIT ?>"  onmouseover="this.src='administrator/images/editar_on.png';swap_resalte('editar',1);" onmouseout="this.src='administrator/images/editar.png';swap_resalte('editar',0);" class="bot_gestionar" id="img_editar"/></a>
                </div>
                <div class="texto_mini_boton"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=editprop&id=". $prop[0]->id ."&Itemid=$Itemid"); ?>" onmouseover="swap_resalte('editar',1);" onmouseout="swap_resalte('editar',0);" title="<?= _E_EDIT; ?>" id="a_editar"><?= _E_EDIT ?></a></div>
                 <?php } ?>

              <?php
		if ($hp_show_pdficon && !$pop) {
		?>
               <div class="mini_boton">
		<a href="javascript:void window.open('<?php echo $mosConfig_live_site; ?>/components/com_hotproperty/pdf.php?id=<?php echo $prop[0]->id; ?>', 'win2', 'status=no,toolbar=no,scrollbars=yes,titlebar=no,menubar=no,resizable=yes,width=640,height=480,directories=no,location=no');" title="<?php echo _CMN_PRINT;?>">
                 <img src="<?php echo $mosConfig_live_site;?>/administrator/images/imprimir.png" border="0" alt="<?php echo _CMN_PDF;?>" onmouseover="this.src='administrator/images/imprimir_on.png';swap_resalte('imprimir',1);" onmouseout="this.src='administrator/images/imprimir.png'; swap_resalte('imprimir',0);" class="bot_gestionar" id="img_imprimir"/></a>
               </div>
               <div class="texto_mini_boton">
		<a href="javascript:void window.open('<?php echo $mosConfig_live_site; ?>/components/com_hotproperty/pdf.php?id=<?php echo $prop[0]->id; ?>', 'win2', 'status=no,toolbar=no,scrollbars=yes,titlebar=no,menubar=no,resizable=yes,width=640,height=480,directories=no,location=no');" onmouseover="swap_resalte('imprimir',1);" onmouseout="swap_resalte('imprimir',0);" class="enlace" id="a_imprimir"><?php echo _CMN_PDF;?></a>
               </div>
		<?php
		} // End of if $hp_show_pdficon

		if ($hp_show_printicon && !$pop) { ?>
                    <div class="mini_boton">
		       <a href="javascript:void window.open('<?php echo $mosConfig_live_site; ?>/index2.php?option=com_hotproperty&amp;task=view&amp;id=<?php echo $prop[0]->id; ?>&amp;pop=1', 'win2', 'status=no,toolbar=no,scrollbars=yes,menubar=no,resizable=yes,width=940,height=480,directories=no,location=no');" title="<?php echo _CMN_PRINT;?>" onmouseover="swap_resalte('imprimir',1);">
		       <img src="administrator/images/imprimir.png" alt="<?= _CMN_PRINT ?>" title="<?= _CMN_PRINT ?>"  onmouseover="this.src='administrator/images/imprimir_on.png';" onmouseout="this.src='administrator/images/imprimir.png'; swap_resalte('imprimir',0);" class="bot_gestionar" id="img_imprimir"/></a>
                     </div>
                     <div class="texto_mini_boton">
                       <a href="javascript:void window.open('<?php echo $mosConfig_live_site; ?>/index2.php?option=com_hotproperty&amp;task=view&amp;id=<?php echo $prop[0]->id; ?>&amp;pop=1', 'win2', 'status=no,toolbar=no,scrollbars=yes,titlebar=no,menubar=no,resizable=yes,width=940,height=480,directories=no,location=no');" title="<?php echo _CMN_PRINT;?>" onmouseover="swap_resalte('imprimir',1);" onmouseout="swap_resalte('imprimir',0);" class="enlace" id="a_imprimir">
                       <?= _CMN_PRINT ?></a> </div>

		<?php } // End of if $hp_show_printicon

		if ($hp_show_emailicon && !$pop) { ?>
                   <div class="mini_boton">
                      <a href="javascript:void window.open('<?php echo $mosConfig_live_site; ?>/index2.php?option=com_hotproperty&amp;task=emailform&amp;id=<?php echo $prop[0]->id; ?>', 'win2', 'status=no,toolbar=no,scrollbars=no,titlebar=no,menubar=no,resizable=yes,width=400,height=285,directories=no,location=no');" title="<?php echo _CMN_EMAIL;?>"><img src="administrator/images/correo.png" alt="<?= _CMN_EMAIL ?>" title="<?= _CMN_EMAIL ?>"  onmouseover="this.src='administrator/images/correo_on.png';swap_resalte('correo',1);" onmouseout="this.src='administrator/images/correo.png'; swap_resalte('correo',0);" class="bot_gestionar" id="img_correo"/></a>
                   </div>
                   <div class="texto_mini_boton">
                      <a href="javascript:void window.open('<?php echo $mosConfig_live_site; ?>/index2.php?option=com_hotproperty&amp;task=emailform&amp;id=<?php echo $prop[0]->id; ?>', 'win2', 'status=no,toolbar=no,scrollbars=no,titlebar=no,menubar=no,resizable=yes,width=400,height=285,directories=no,location=no');" title="<?php echo _CMN_EMAIL;?>" onmouseover="swap_resalte('correo',1);" onmouseout="swap_resalte('correo',0);" id="a_correo">
                      <?= _HP_EMAIL_AMIGO ?></a> </div>
		<?php  } ?>
           </div>
          </div>
         </div> <!-- fin div.mini_botones -->
        </div>

            <br class="clearboth">
            <br class="clearboth">

<?php
		 // Copyright Â© 2006, Michael Rice
        // License: GPL but you must email when you use it to let me know and this copyright MUST remain intact.
        // Email: meikeric {at] gmail [dot} com
        // You can donate to my Paypal and request hacks for HotProperty.  Every little bit helps.
        // Paypal: meikeric {at] gmail [dot} com

        ?>
        <?php $iCount = count($images); ?>
       <script language="JavaScript" type="text/javascript">
        //for image viewer slide show

        myCount = 0;

        function UpdateCounter( currentCount )
        {
	        myCount = currentCount;
        }

        function loadImgArray( strList ){
	        myImgList = strList.split(",");
        }
        function NextSlideShow( maxCount ){
	        if(myCount >= maxCount){
		        myCount = 0;
	        }else{
		        myCount++;
	        }
	        //alert( myImgList[myCount] );
	        show('MainPhoto',myImgList[myCount]);
        }
        function PrevSlideShow( maxCount ){
	        if(myCount <= 0){
		        myCount = maxCount;
	        }else{
		        myCount--;
	        }
	        //alert( myImgList[myCount] );
	        show('MainPhoto',myImgList[myCount]);
        }

        function fillLabel(inField,inValue){
            if(inValue == ''){
	        inValue = myCount + 1;
            }
            if(document.layers)    //NN4+
            {
               document.layers[inField].innerHTML = inValue;
            }
            else if(document.getElementById)      //gecko(NN6) + IE 5+
            {
                var obj = document.getElementById(inField);
                obj.innerHTML = inValue;
            }
            else if(document.all)       // IE 4
            {
                document.all[inField].innerHTML = inValue;
            }
        }

        function show(name,src) {
            if (document.images)
                document.images[name].src = src;
        }
        </script>


        <div id="hp_view_standard_photo_con1">
                                <a href='javascript:NextSlideShow( <?php echo $iCount-1; ?>);'><img src='<?php echo $mosConfig_live_site.$hp_imgdir_standard.$images[0]->standard; ?>' name='MainPhoto' alt='Click here to view the next image' border='0' class='search01'/></a>
								<br/>
            <?php if (!$pop) { ?>
				<a href='javascript:PrevSlideShow( <?php echo $iCount-1; ?> );'>< <?php echo _CMN_PREV;?></a> &nbsp;&nbsp;&nbsp;&nbsp;
                                <a href='javascript:NextSlideShow( <?php echo $iCount-1; ?> );'><?php echo _CMN_NEXT;?> ></a>
            <?php } ?>

		<script language='Javascript' type="text/javascript">
                loadImgArray( '<?php
                $i = 1;
                foreach($images AS $image) {
                    echo $mosConfig_live_site.$hp_imgdir_standard.$image->standard;
                    if($iCount > 1 && $i != $iCount) {
                        echo ',';
                    }
                    $i++;
                }
                ?>' );
                </script>
        </div>

		<?php foreach($prop AS $p) {
				?>
<?php
					/* Escupe los campos de manera poco flexible pero práctica (igual que en listado) */

					echo '<div id="datos_inmueble">';


					// Name (Titulo)
					echo '<div id="titulo_inmueble">'.$p->name.'.</div>';

					// Referencia propia
					echo '<div class="linea_inmueble">';
					echo '<span class="hp_caption">&nbsp;›&nbsp;'._OFER_REF."</span>: ";
					if ($p->id <> "") echo $p->id.".<br />"; else echo _OFER_NO_DEF;
					echo '</div>';


					// Type (Tipo de oferta)
					echo '<div class="linea_inmueble">';
					if (!$caption['type']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['type']->caption."</span>: ";
					echo $p->type.".<br/>";
					echo '</div>';

					// Tipo_Alojamiento
					echo '<div class="linea_inmueble">';
					if (!$caption['Tipo_Alojamiento']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Tipo_Alojamiento']->caption."</span>: ";
					if ($p->Tipo_Alojamiento<>"") echo $p->Tipo_Alojamiento.".<br/>"; else echo _OFER_NO_DEF;
					echo '</div>';

					// Address y postcode (Dirección y CP)
					echo '<div class="linea_inmueble">';
					if (!$caption['address']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['address']->caption."</span>: ";
					if ($p->address<>"") echo $p->address." ".$p->postcode."<br/>"; else echo _OFER_NO_DEF;
					echo '</div>';


					// Barrio
					if ( $p->Barrio <> "")
					{
						echo '<div class="linea_inmueble">';
						if (!$caption['Barrio']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Barrio']->caption."</span>: ";
						echo $p->Barrio;
						echo '</div>';
					}

					// Suburb y State (Población y Provincia)
					echo '<div class="linea_inmueble">';
					if (!$caption['suburb']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['suburb']->caption."</span>: ";
					if ($p->suburb<>"") echo $p->suburb." (".$p->state.")"."<br/>"; else echo _OFER_NO_DEF;
					echo '</div>';

					// Precio
					if ($p->typeid == 2) // Venta
					{
						echo '<div class="linea_inmueble">';
						if (!$caption['price']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['price']->caption."</span>: ";
						if ($p->price<>"") echo number_format($p->price,2,',','.')." ".$caption['price']->append_text."<br />"; else echo _OFER_NO_DEF;
						echo '</div>';
					}


					// Num_plazas
					if ($p->typeid == 1) // Alquiler
					{
						echo '<div class="linea_inmueble">';
						if (!$caption['Num_plazas']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Num_plazas']->caption."</span>: ";
						if ($p->Num_plazas<>"") echo $p->Num_plazas.".<br />"; else echo _OFER_NO_DEF;
						echo '</div>';
					}

					// Numero_dormitorios
					echo '<div class="linea_inmueble">';
					if ($p->typeid == 1) // Alquiler
					{
						if (!$caption['Numero_dormitorios']->hideCaption)
						{
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Numero_dormitorios']->caption."</span>: ";
							}
					}
					else
							echo '<span class="hp_caption">&nbsp;›&nbsp;Nº habitaciones: </span>';

					if ($p->Numero_dormitorios<>"") echo $p->Numero_dormitorios.".<br />"; else echo _OFER_NO_DEF;
					echo '</div>';


					// Cuartos_banio_con_duchas
					echo '<div class="linea_inmueble">';
					if (!$caption['Cuartos_banio_con_duchas']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Cuartos_banio_con_duchas']->caption."</span>: ";
					if ($p->Cuartos_banio_con_duchas<>"") echo $p->Cuartos_banio_con_duchas.". "; else echo _OFER_NO_DEF."  ";

					// Aseos
					if ($p->Aseos <> "")
					{
						if (!$caption['Aseos']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Aseos']->caption."</span>: ";
						if ($p->Aseos<>"") echo $p->Aseos.".<br />"; else echo _OFER_NO_DEF;
					}
					echo '</div>';

					// Dispone
					echo '<div class="linea_inmueble">';
					if (!$caption['Dispone']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Dispone']->caption."</span>: ";
					if ($p->Dispone<>"") echo str_replace("|",", ",$p->Dispone).".<br />"; else echo _OFER_NO_DEF;
					echo '</div>'; ?>

           </div> <!-- fin capa datos_inmueble -->

			<div class="hp_view_details">
			<div id="mas_datos">
			<div class="titulito_inmueble">Características del inmueble</div>
			<?php
					// Año Construcción
					if ($p->typeid == 2) // Venta
					{
						echo '<div class="linea_inmueble">';
						if (!$caption['AnioConstruccion']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['AnioConstruccion']->caption."</span>: ";
						if ($p->AnioConstruccion <> "") echo $p->AnioConstruccion.".  "; else echo _OFER_NO_DEF;
						if (!$caption['UltimaReforma']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['UltimaReforma']->caption."</span>: ";
						if ($p->UltimaReforma <> "") echo $p->UltimaReforma.".  "; else echo _OFER_NO_DEF;
						echo '</div>';
					}

					// Metros construidos
					echo '<div class="linea_inmueble">';
					if ($p->typeid == 2) // Venta
					{
						if (!$caption['MetrosConstruidos']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['MetrosConstruidos']->caption."</span>: ";
						if ($p->MetrosConstruidos <> "") echo $p->MetrosConstruidos." ".$caption['Superficie_habitable']->append_text.".  "; else echo _OFER_NO_DEF;
					}

					if (!$caption['Superficie_habitable']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Superficie_habitable']->caption."</span>: ";
					echo $p->Superficie_habitable." ".$caption['Superficie_habitable']->append_text.".  ";

					// Superficie_parcela
					if ($p->Superficie_parcela <> "")
					{
						if (!$caption['Superficie_parcela']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Superficie_parcela']->caption."</span>: ";
						echo $p->Superficie_parcela." ".$caption['Superficie_parcela']->append_text.".";
					}
					echo "<br />";
					echo '</div>';


					if ($p->Camas_dobles <> 0 ||  $p->Camas_individuales <> 0 || $p->Camas_supletorias <> 0 || $p->Camas_litera <> 0)
					echo '<div class="linea_inmueble">';


				if ($p->typeid == 1) // Alquiler
				{
					// Camas_dobles
					if ($p->Camas_dobles <> 0)
					{
							if (!$caption['Camas_dobles']->hideCaption)
								echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Camas_dobles']->caption."</span>: ";
							echo $p->Camas_dobles.".&nbsp;&nbsp;";
					}

					// Camas_individuales
					if ($p->Camas_individuales <> 0)
					{
						if (!$caption['Camas_individuales']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Camas_individuales']->caption."</span>: ";
						echo $p->Camas_individuales.".&nbsp;&nbsp;";
					}

					// Camas_supletorias
					if ($p->Camas_supletorias <> 0)
					{
						if (!$caption['Camas_supletorias']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Camas_supletorias']->caption."</span>: ";
						echo $p->Camas_supletorias.".&nbsp;&nbsp;";
					}

					// Camas_litera
					if ($p->Camas_litera <> 0)
					{
						if (!$caption['Camas_litera']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Camas_litera']->caption."</span>: ";
						echo $p->Camas_litera.".&nbsp;&nbsp;";
					}
					if ($p->Camas_dobles <> 0 ||  $p->Camas_individuales <> 0 || $p->Camas_supletorias <> 0 || $p->Camas_litera <> 0)
					{
						echo "<br/>";
						echo "</div>";
					}

					// Sofa_cama_individual
					if ($p->Sofa_cama_individual<>"" && $p->Sofa_cama_individual<>"")
					{
						echo '<div class="linea_inmueble">';
						// Sofa_cama_individual
						if ( $p->Sofa_cama_individual <> "")
						{
							if (!$caption['Sofa_cama_individual']->hideCaption)
								echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Sofa_cama_individual']->caption."</span>: ";
							echo $p->Sofa_cama_individual.".&nbsp;&nbsp;";
						}

					// Sofa_cama_doble
						if ( $p->Sofa_cama_doble <> "")
						{
							if (!$caption['Sofa_cama_doble']->hideCaption)
								echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Sofa_cama_doble']->caption."</span>: ";
							echo $p->Sofa_cama_doble.".&nbsp;&nbsp;";
						}
						echo "</div>";
					}

					// Superficie_terraza_solarium
					if ($p->Superficie_terraza_solarium <> "")
					{
						echo '<div class="linea_inmueble">';
						if (!$caption['Superficie_terraza_solarium']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Superficie_terraza_solarium']->caption."</span>: ";
						echo $p->Superficie_terraza_solarium." ".$caption['Superficie_terraza_solarium']->append_text.".<br />";
						echo "</div>";
					}
				}
					// Enseres
					echo '<div class="linea_inmueble">';
					if (!$caption['Enseres']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Enseres']->caption."</span>: ";
					echo str_replace("|",", ",$p->Enseres).".<br />";
					echo '</div>';

			  		// Animales
			  		if ($p->typeid == 1) // Alquiler
					{
						echo '<div class="linea_inmueble">';
						if (!$caption['Animales']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Animales']->caption."</span>: ";
						echo $p->Animales.".<br />";
						echo '</div>';
					}


					// *** Características de la zona **
					echo '<div class="titulito_inmueble">Características de la zona</div>';

					// Zona
					echo '<div class="linea_inmueble">';
					if (!$caption['Zona']->hideCaption)
						echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Zona']->caption."</span>: ";
					echo str_replace("|",", ",$p->Zona).".<br />";
					echo '</div>';

					// ActividadesDeportivas
					if ($p->typeid == 1) // Alquiler
					{
						if($caption['ActividadesDeportivas'] != '')
						{
							echo '<div class="linea_inmueble">';
							if (!$caption['ActividadesDeportivas']->hideCaption)
								echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['ActividadesDeportivas']->caption."</span>: ";
							echo str_replace("|",", ",$p->ActividadesDeportivas).".<br />";
							echo '</div>';
						}
					}

					// Distancia_a_la_playa
					echo '<div class="linea_inmueble">';
					if ($p->Distancia_a_la_playa <> "")
					{
						if (!$caption['Distancia_a_la_playa']->hideCaption)
							echo '<span class="hp_caption">&nbsp;›&nbsp;'.$caption['Distancia_a_la_playa']->caption."</span>: ";
						echo $p->Distancia_a_la_playa." ".$caption['Distancia_a_la_playa']->append_text.".<br />";
					}
					echo '</div>';

					if ($p->A_minutos_andando <> "")
					{
						echo '<div class="linea_inmueble">';
						echo '<span class="hp_caption">';
						if (!$caption['A_minutos_andando']->hideCaption)
							echo '&nbsp;›&nbsp;'.$caption['A_minutos_andando']->caption.' ';
						echo $p->A_minutos_andando." ".$caption['A_minutos_andando']->append_text." ";
						if (!$caption['de_andando']->hideCaption)
							echo ' '.$caption['de_andando']->caption.' ';
						echo $p->de_andando.".</span><br />";
						echo '</div>';
					}

					if ($p->A_minutos_coche <> "")
					{
						echo '<div class="linea_inmueble">';
						echo '<span class="hp_caption">';
						if (!$caption['A_minutos_coche']->hideCaption)
							echo '&nbsp;›&nbsp;'.$caption['A_minutos_coche']->caption.' ';
						echo $p->A_minutos_coche." ".$caption['A_minutos_coche']->append_text." ";
						if (!$caption['de_coche']->hideCaption)
							echo ' '.$caption['de_coche']->caption.' ';
						echo $p->de_coche.".</span><br />";
						echo '</div>';
						echo '<br/>';
					}

					// *** Descripción detallada ***
					echo '<div class="titulito_inmueble">'.$caption['full_text']->caption.'</div>';
					echo '<div class="linea_inmueble">';
					echo '<div class="descripcion">';
					echo $p->full_text;
					echo '</div>';
					echo '</div>';

					// *** Disponibilidad ***
                                        if ($p->typeid == 1 && $num_periodos > 0) // Alquiler
					{
					echo '<div class="titulito_inmueble">'.$caption['intro_text']->caption.'</div>';
					echo '<div class="linea_inmueble">'; ?>
					<iframe src="index2.php?option=com_hp_avl&task=ext_show_year&lang=<?php echo $_GET['lang']?>&property_id=<?php echo $p->id; ?>" name="com_hp_avl" id="com_hp_avl" width="97%" height="<?php echo (($num_periodos * 20)+580); ?>" marginwidth="0" marginheight="0" align="top" scrolling="no" frameborder="0" hspace="0" vspace="0" background="white"></iframe>
					<div id="combo_num_anios">
					<script language="javascript">
						function RecargaCalendario(id,alto,num_anios,lang,property_id)
						{
							iframe=document.getElementById(id);

							iframe.src="index2.php?option=com_hp_avl&task=ext_show_year&num_anios="+num_anios+"&lang=" +lang+"&property_id="+property_id;
							if (num_anios > 1)
								iframe.height=alto * 0.9 * num_anios;
							else
								iframe.height=alto;
						}
					</script>
				<form action="" method="GET" name="calendarioForm">
					<p>Mostrar <select name="num_anios" size="1" onChange="RecargaCalendario('com_hp_avl',<?php echo (($num_periodos * 20)+580); ?>,this.value,'<?php echo mosGetParam( $_GET, 'lang',0);?>',<?php echo $p->id; ?>);">
								<option value="1" label="1" <?php if ($num_years==1) echo "selected"; ?>>1</option>
								<option value="2" label="2" <?php if ($num_years==2) echo "selected"; ?>>2</option>
							</select>
				años.
					<input type="hidden" name="option" value="<?php echo $option; ?>" />
					<input type="hidden" name="task" value="ext_show_year" />
					<input type="hidden" name="property_id" value="<?php echo $property_id; ?>" />
				</form>
			         </div>
			      </div>
			</div>
                            <?php       }
                        } ?>
                <br class="clearboth" />

		<?php if ($hp_show_agentdetails && $hp_use_companyagent) { ?>
		<div id="hp_view_agent_title"><span class="flecha_big">›&nbsp;</span><?php echo _HP_VIEW_AGENT_TITLE; ?></div>
		<div id="hp_view_agent_con">
		<?php hotproperty_HTML::show_AgentInfo($agent) ?>
		</div>
		<?php } ?>
		<?php if ($hp_show_enquiryform && !$pop) { ?>
		<br class="clearboth" />
		<div id="hp_view_agent_contact"><span class="flecha_big">›&nbsp;</span><?php echo _HP_VIEW_AGENT_CONTACT; ?></div>
		<div id="hp_emailform_con">
		<?php hotproperty_HTML::show_EmailForm('property',$prop[0]->id,$prop[0]->name,$prop[0]->typeid); ?>
		</div>
		<?php }
		if ($pop) {
		?>
		<center><a href='javascript:window.close();'><span class="small"><?php echo _PROMPT_CLOSE;?></span></a></center>
		<?php } ?>
	</div>
		<?php
	}

	/***
	 * Common Routine to display Agent's Info
	 **/
	function show_AgentInfo($agent) {
		global $mosConfig_live_site, $hp_imgdir_agent, $task, $Itemid, $my;

		if (empty($agent)) {
			echo _HP_AGENT_ERROR_EMPTY;
		} else { ?>
                <script src="components/com_hotproperty/js/resalte.js" type="text/javascript"></script>
		<div class="hp_view_agent">
			<div id="hp_view_agent_details">
			<?php if (!empty($agent->photo)) { ?>
			 <div id="hp_view_agent_photo">
				<?php if ($task <> "viewagent") { ?>
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagent&id=$agent->id&Itemid=$Itemid"); ?>">
				<img border="0" src="<?php echo $mosConfig_live_site.$hp_imgdir_agent.$agent->photo; ?>" alt="<?php echo $agent->name; ?>" />
				</a>
				<?php } else { ?>
				<img border="0" src="<?php echo $mosConfig_live_site.$hp_imgdir_agent.$agent->photo; ?>" alt="<?php echo $agent->name; ?>" />
				<?php } ?>
			</div>
			<?php } ?>
                        <div class="obj_con_botonera">
                            <div class="nombre_objeto">
				<span id="hp_caption_agentname"><?php if ($task <> "viewagent") { ?><a id="hp_caption_agentname" href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagent&id=$agent->id&Itemid=$Itemid"); ?>"><?php echo $agent->name; ?></a><?php } else { ?><?php echo $agent->name; ?><?php } ?></span>
                            </div>
                            <div class="botonera_dcha">
				<?php
				# Muestra el icono para modificar el perfil si somos el usuario adecuado
				if ($agent->user == $my->id && $agent->user > 0 && $my->id > 0) { ?>
                                <div class="mini_boton">
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=editagent&Itemid=$Itemid"); ?>" title="<?= _HP_AGENT_MODIFY ?>"><img src="administrator/images/modif_perfil.png" alt="<?= _HP_AGENT_MODIFY ?>" title="<?= _HP_AGENT_MODIFY ?>"  onmouseover="swap_resalte('modif_perfil',1);" onmouseout="swap_resalte('modif_perfil',0);" class="bot_gestionar" id="img_modif_perfil"/></a>
                                </div>
                                <div class="texto_mini_boton">
                                <a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=editagent&Itemid=$Itemid"); ?>" title="<?= _HP_AGENT_MODIFY ?>" id="a_modif_perfil" onmouseover="swap_resalte('modif_perfil',1);" onmouseout="swap_resalte('modif_perfil',0);"> <?= _HP_AGENT_MODIFY ?> </a>
                                </div>
				<?php } ?>

				<?php
				# Muestra icono enviar email si estamos en viewagent|viewco y somos un usuario distinto
				if( ($task == "viewagent" || $task == "viewco") && !empty($agent->email)  &&  $agent->user != $my->id) { ; ?>
                                <div class="mini_boton">
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagentemail&id=$agent->id&Itemid=$Itemid"); ?>"><img src="administrator/images/correo.png" alt="<?= _HP_AGENT_SENDEMAIL ?>" title="<?= _HP_AGENT_SENDEMAIL ?>"  onmouseover="this.src='administrator/images/correo_on.png';swap_resalte('correo',1);" onmouseout="this.src='administrator/images/correo.png';swap_resalte('correo',0);" class="bot_gestionar" id="img_correo"/></a>
                                </div>
                                <div class="texto_mini_boton">
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagentemail&id=$agent->id&Itemid=$Itemid"); ?>" id="a_correo" onmouseover="swap_resalte('correo',1);" onmouseout="swap_resalte('correo',0);"><?= _HP_AGENT_SENDEMAIL ?></a>
                                </div>
                                <?php } ?>
				<?php
				# Muestra enlace ver todas las ofertas si estamos en página detalle
				if( $task == "view" ) {  ?>
                                <div class="mini_boton">
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagent&id=$agent->id&Itemid="); ?>"><img src="administrator/images/ver.png" alt="<?= _HP_VER_OFERTAS ?>" title="<?= _HP_VER_OFERTAS ?>"  onmouseover="swap_resalte('ver',1);" onmouseout="swap_resalte('ver',0);" class="bot_gestionar" id="img_ver"/></a>
                                </div>
                                <div class="texto_mini_boton">
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagent&id=$agent->id&Itemid="); ?>" id="a_ver" onmouseover="swap_resalte('ver',1);" onmouseout="swap_resalte('ver',0);"><?= _HP_VER_OFERTAS ?></a>
                                </div>
                                <?php } ?>

                            </div>
                         </div>
                         <br class="clearboth"/>
                         <br class="clearboth"/>

				<?php if (!empty($agent->desc)) { ?>
				<div id="hp_view_agent_desc"><?php echo $agent->desc; ?></div>
				<?php } ?>

                                <?php
				# Display Mobile number if not empty
			      if (!empty($agent->mobile)) { ?>
				<span class="hp_caption"><?= _HP_AGENT_MOBILE; ?>:</span> <?= $agent->mobile; ?>.<br/>
			        <?php # Muestra disponibiblidad teléfono móvil
				if (!empty($agent->disp_mov_from)) { ?>
                                     <span class="hp_caption"><?=  _HP_AGENT_DISP; ?></span> de <?= dia_semana($agent->disp_mov_from); ?> a <?= dia_semana($agent->disp_mov_to); ?> de <?= $agent->disp_mov_from_hora; ?> a <?= $agent->disp_mov_to_hora; ?>.<br/>
                               <?php }
			       } ?> <!-- fin movil -->

				<?php
				# Muestra el teléfono fijo si existe
			      if (!empty($agent->fijo)) { ?>
				<span class="hp_caption"><?php echo _HP_AGENT_PHONE; ?>:</span> <?php echo $agent->fijo; ?>.
                                <br/>
				<?php
                                # Muestra disponibilidad teléfono fijo
                                if (!empty($agent->disp_fijo_from)) { ?>
                                <span class="hp_caption"><?= _HP_AGENT_DISP ?></span> de <?= dia_semana($agent->disp_fijo_from); ?> a <?= dia_semana($agent->disp_fijo_to); ?> de <?= $agent->disp_fijo_from_hora; ?> a <?= $agent->disp_fijo_to_hora; ?>.<br />
                                <?php } ?> <!-- fin disponibilidad -->

                              <?php } ?> <!-- fin fijo -->

                              <?php
				# Muestra los idiomas hablados si no está vacío
			      if (!empty($agent->idiomas_hablados)) { ?>
				<span class="hp_caption"><?php echo _HP_AGENT_IDIOMAS; ?>:</span> <?= str_replace("|",", ",$agent->idiomas_hablados) ?>.
                                <br />
			      <?php } ?>
                      </div>
		</div>
		<?php
		} // End If
	}

	/***
	 * Common Routine to display Company's Info
	 **/
	function show_CoInfo($companies, $agent) {
		global $mosConfig_live_site, $hp_imgdir_company, $task, $Itemid, $my;

		foreach($companies AS $co) {
	?>
	<div class="hp_view_co">
		<?php if (!empty($co->photo)) { ?>
		<div id="hp_view_co_photo"><img src="<?php echo $mosConfig_live_site.$hp_imgdir_company.$co->photo; ?>" alt="<?php echo $co->name; ?>" /></div>
		<?php } ?>
       <div class="obj_con_botonera">
             <div class="nombre_objeto_corto">
		<div id="hp_view_co_details">
			<span id="hp_caption_coname">
			<?php if ($task <> "viewco") { ?><a id="hp_caption_coname" href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewco&id=$co->id&Itemid=$Itemid"); ?>"><?php echo $co->name; ?></a><?php } else {
				echo $co->name;
			} ?></span>
                 </div>
                 <br/>
 		 <div id="hp_co_addr">
 		         <?php
				if (trim($co->address)!="") {
					echo "$co->address <br />";
				}
				if ((trim($co->suburb)!="") && (trim($co->state)!="") && (trim($co->postcode)!="")) {
					echo "$co->suburb, $co->state, $co->postcode <br />";
				 } elseif ((trim($co->suburb)!="") && (trim($co->state)!="")) {
					echo "$co->suburb, $co->state <br />";
				} elseif ((trim($co->suburb)!="") && (trim($co->postcode)!="")) {
					echo "$co->suburb, $co->postcode <br />";
				} elseif ((trim($co->state)!="") && (trim($co->postcode)!="")) {
					echo "$co->state, $co->postcode <br />";
				} elseif ((trim($co->state)!="")) {
					echo "$co->state <br />";
				} elseif ((trim($co->suburb)!="")) {
					echo "$co->suburb <br />";
				} elseif ((trim($co->postcode)!="")) {
					echo "$co->postcode <br />";
				}
				if (trim($co->country)!="") {
					echo "$co->country <br />";
				}
			?></div>
             </div>
             <div class="botonera_dcha">
				<?php
				# Show an edit icon to allow user to edit their own profile
				if ($agent->user == $my->id && $agent->user > 0 && $my->id > 0) { ?>
				&nbsp;
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=editagent&Itemid=$Itemid"); ?>" title="<?= _HP_AGENT_MODIFY ?>"><img src="administrator/images/editar.png" alt="<?= _HP_AGENT_MODIFY ?>" title="<?= _HP_AGENT_MODIFY ?>"  onmouseover="this.src='administrator/images/editar_on.png';" onmouseout="this.src='administrator/images/editar.png'" class="bot_gestionar"/></a>
				<?php } ?>

				<?php
				# Display "Send email link" if user at viewagent or viewco
				if( ($task == "viewagent" || $task == "viewco") && !empty($agent->email) ) { ; ?>
				<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagentemail&id=$agent->id&Itemid=$Itemid"); ?>"><img src="administrator/images/correo.png" alt="<?= _HP_AGENT_SENDEMAIL ?>" title="<?= _HP_AGENT_SENDEMAIL ?>"  onmouseover="this.src='administrator/images/correo_on.png';" onmouseout="this.src='administrator/images/correo.png'" class="bot_gestionar"/></a><br />
				<?php } ?>
              </div> <!-- FIN div.botonera_dcha -->
         </div> <!-- FIN div.obj_con_botonera -->

                        <br class="clearboth"/>
                        <div id="hp_view_co_other_data">
                        <br/>

                        <!-- Teléfono fijo -->
			<?php if (!empty($co->telephone)) { ?>
			<span class="hp_caption"><?php echo _HP_CONTACTNUMBER; ?>: </span><?php echo $co->telephone; ?>.
                        <br/>
                        <?php
                        # Muestra disponibilidad teléfono fijo
                                if (!empty($co->disp_fijo_from)) { ?>
                                <span class="hp_caption"><?= _HP_AGENT_DISP ?></span> <?= dia_semana($co->disp_fijo_from); ?> a <?= dia_semana($co->disp_fijo_to); ?> de <?= $co->disp_fijo_from_hora; ?> a <?= $co->disp_fijo_to_hora; ?>.<br />
                                <?php } ?> <!-- fin disponibilidad -->
			<?php }?> <!-- fin teléfono fijo -->

                        <!-- Teléfono móvil -->
			<?php if (!empty($co->mobile)) { ?>
			<span class="hp_caption"><?php echo _HP_AGENT_MOBILE; ?>: </span><?php echo $co->mobile; ?><br />
                        <?php
                        # Muestra disponibilidad teléfono móvil
                                if (!empty($co->disp_mov_from)) { ?>
                                <span class="hp_caption"><?= _HP_AGENT_DISP ?></span> <?= dia_semana($co->disp_mov_from); ?> a <?= dia_semana($co->disp_mov_to); ?> de <?= $co->disp_mov_from_hora; ?> a <?= $co->disp_mov_to_hora; ?>.<br />
                                <?php } ?> <!-- fin disponibilidad -->
			<?php }?> <!-- fin teléfono móvil -->

			<?php if (!empty($co->website)) { ?>
			<span class="hp_caption"><?php echo _HP_CO_WEBSITE; ?> </span><a href="<?php echo $co->website; ?>" target="_blank"><?php echo $co->website; ?></a><br />
			<?php }?>
			<?php if (!empty($co->desc)) { ?>
			<p />
			<?php echo $co->desc; ?>
			<?php } ?>
                        </div>
                        <?php
	              	      # Muestra los idiomas hablados si no está vacío
			      if (!empty($co->idiomas_hablados)) { ?>
				<span class="hp_caption"><?php echo _HP_AGENT_IDIOMAS; ?>:</span> <?= str_replace("|",", ",$co->idiomas_hablados) ?>.
                                <br/>
		      <?php } ?>

	</div>
	<br class="clearboth" />
	<?php
		}
	}

	/***
	 * Display Email form
	 ***/
	function show_EmailForm($subject, $id, $titulo=null, $tipo_oferta=null) {
		global $Itemid;

		if ($subject <> "agent" && $subject <> "property" && $subject <> "company") return false;
	?>
		<form method="POST" action="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=sendenquiry&id=".$id."&Itemid=".$Itemid); ?>">
                <input type="hidden" name="titulo" value="<?= $titulo ?>">
                <input type="hidden" name="tipo_oferta" value="<?= $tipo_oferta ?>">

		<div class="cont_form"><div class="agent_text"><?php echo _CMN_NAME; ?>*:</div>
		<input type="text" class="inputbox" name="hp_name" size="24" /></div>

		<div class="cont_form"><div class="agent_text"><?php echo _CMN_EMAIL; ?>*:</div>
		<input type="text" class="inputbox" name="hp_email" size="30" /></div>

		<div class="cont_form"><div class="agent_text"><?php echo _HP_CONTACTNUMBER; ?>*:</div>
		<input type="text" class="inputbox" name="hp_contactnumber" size="30" /></div>

		<div class="cont_form ">
			<span class="agent_text"><?= _HP_AGENT_DISP ?><?= _HP_AGENT_DISP_DE ?></span>
                           <select name="disp_mov_from" class="inputbox">
                          <?php $valor=$row->disp_mov_from; ?>
					<option value='0'><?= _HP_DIA; ?></option>
					<option value ='1'><?= _HP_L; ?></option>
					<option value ='2'><?= _HP_M; ?></option>
					<option value ='3'><?= _HP_X; ?></option>
					<option value ='4'><?= _HP_J; ?></option>
					<option value ='5'><?= _HP_V; ?></option>
					<option value ='6'><?= _HP_S; ?></option>
					<option value ='7'><?= _HP_D; ?></option>
				</select>
				<?= _HP_AGENT_DISP_A ?>
				<select name="disp_mov_to" class="inputbox">
					<?php $valor=$row->disp_mov_to; ?>
				   	<option value='0'><?php echo _HP_DIA; ?></option>
					<option value ='1'><?php echo _HP_L; ?></option>
					<option value ='2'><?php echo _HP_M; ?></option>
					<option value ='3'><?php echo _HP_X; ?></option>
					<option value ='4'><?php echo _HP_J; ?></option>
					<option value ='5'><?php echo _HP_V; ?></option>
					<option value ='6'><?php echo _HP_S; ?></option>
					<option value ='7'><?php echo _HP_D; ?></option>
				</select>
                                <?= _HP_AGENT_DISP_DE ?>
				<select name="disp_mov_from_hora" class="inputbox">
					<?php $valor=$row->disp_mov_from_hora; ?>
				   	<option value='0' ><?= _HP_HORA ?></option>
					<option value ='06:00'>06:00</option>
					<option value ='07:00'>07:00</option>
					<option value ='08:00'>08:00</option>
					<option value ='09:00'>09:00</option>
					<option value ='10:00'>10:00</option>
					<option value ='11:00'>11:00</option>
					<option value ='12:00'>12:00</option>
					<option value ='13:00'>13:00</option>
					<option value ='14:00'>14:00</option>
					<option value ='15:00'>15:00</option>
					<option value ='16:00'>16:00</option>
					<option value ='17:00'>17:00</option>
					<option value ='18:00'>18:00</option>
					<option value ='19:00'>19:00</option>
					<option value ='20:00'>20:00</option>
					<option value ='21:00'>21:00</option>
					<option value ='22:00'>22:00</option>
					<option value ='23:00'>23:00</option>
					<option value ='00:00'>00:00</option>
				</select>
				<?= _HP_AGENT_DISP_A ?>
				<select name="disp_mov_to_hora" class="inputbox">
					<?php $valor=$row->disp_mov_to_hora; ?>
				   	<option value='0' ><?= _HP_HORA ?></option>
					<option value ='06:00'>06:00</option>
					<option value ='07:00'>07:00</option>
					<option value ='08:00'>08:00</option>
					<option value ='09:00'>09:00</option>
					<option value ='10:00'>10:00</option>
					<option value ='11:00'>11:00</option>
					<option value ='12:00'>12:00</option>
					<option value ='13:00'>13:00</option>
					<option value ='14:00'>14:00</option>
					<option value ='15:00'>15:00</option>
					<option value ='16:00'>16:00</option>
					<option value ='17:00'>17:00</option>
					<option value ='18:00'>18:00</option>
					<option value ='19:00'>19:00</option>
					<option value ='20:00'>20:00</option>
					<option value ='21:00'>21:00</option>
					<option value ='22:00'>22:00</option>
					<option value ='23:00'>23:00</option>
					<option value ='00:00'>00:00</option>
			</select>
			</div>


                <div class="cont_form"><div class="agent_text"><?php echo _HP_SPOKEN_LANG; ?>:</div>
                        <?php

                            $idiomas = explode("|",_HP_IDIOMAS);
                            $hablados = explode("|",$row->idiomas_hablados);

                            foreach($hablados as $hablado)
                              $idiomas_hablados[$hablado]=true;


                            foreach($idiomas as $idioma) { ?>
                              <input type='checkbox' value='<?= $idioma ?>' name='habla[]'/><?= $idioma ?>
                            <?php } ?>
                </div>


                        <?php if ( $tipo_oferta==1 ) /* Alquiler */ { ?>
			<div class="cont_form"><div class="agent_text"><?php echo _HP_DIA_LLEGADA ?>:</div>
			<input class="inputbox" type="text" name="dia_llegada" size="10" maxlength="10">
                        </div>
			<div class="cont_form"><div class="agent_text"><?php echo _HP_DIA_SALIDA ?>:</div>
			<input class="inputbox" type="text" name="dia_salida" size="10" maxlength="10">
                        </div>
                        <?php  } ?>

		<div class="cont_form"><div class="agent_text"><?php echo _ENQUIRY; ?>*:</div>
		<textarea rows="4" cols="40" class="inputbox" name="hp_enquiry"></textarea>
		<br/>
		<input type="hidden" name="sbj" value="<?php echo $subject; ?>" />
		<input class="button" type="submit" value="<?php echo _HP_SENDENQUIRY; ?>" /></div>

		</form>
	<?php
	}

	function sendEmailForm($id, $title) {
		global $mosConfig_sitename;
?>
<script language="javascript" type="text/javascript">
	function submitbutton() {
		var form = document.frontendForm;

		// do field validation
		if (form.email.value == "" || form.youremail.value == "") {
			alert( '<?php echo addslashes( _EMAIL_ERR_NOINFO ); ?>' );
			return false;
		}
		return true;
	}
        function textCounter(field, maxlimit) {
                if (field.value.length > maxlimit) // if too long...trim it!
                   field.value = field.value.substring(0, maxlimit);
        }
	</script>
<title><?php echo $mosConfig_sitename; ?> :: <?php echo $title; ?></title>
<body class="contentpane">
<form action="index2.php?option=com_hotproperty&task=emailsend" name="frontendForm" method="POST" onSubmit="return submitbutton();">
    <br/>
    <div class="titulo"><span class="flecha_big">› </span><?php echo _EMAIL_FRIEND; ?></div>
    <br/>
    <div class="cont_form">
     <div class="agent_text"><?php echo _EMAIL_FRIEND_ADDR; ?></div>
     <input type="text" name="email" class="inputbox" size="25">
    </div>
    <div class="cont_form">
     <div class="agent_text"><?php echo _EMAIL_YOUR_NAME; ?></div>
     <input type="text" name="yourname" class="inputbox" size="25">
    </div>
    <div class="cont_form">
     <div class="agent_text"><?php echo _EMAIL_YOUR_MAIL; ?></div>
     <input type="text" name="youremail" class="inputbox" size="25">
    </div>
    <div class="cont_form">
     <div class="agent_text"><?php echo _EMAIL_YOUR_MESS; ?></div>
     <textarea name="yourmess" class="inputbox" cols="23" rows="4" onkeydown="textCounter(this.form.yourmess, 250);" onkeyup="textCounter(this.form.yourmess, 250);"></textarea>
    </div>
    <br/>
    <div class="centro">
      <input type="submit" name="submit" class="button" value="<?php echo _BUTTON_SUBMIT_MAIL; ?>">
      <input type="button" name="cancel" value="<?php echo _BUTTON_CANCEL; ?>" class="button" onClick="window.close();"></td>
     </div>
     <input type="hidden" name="id" value="<?php echo $id; ?>">
</form>
<?php
	}

	function emailSent( $to ) {
		global $mosConfig_sitename;
?>
<br />
<?php echo _EMAIL_SENT; ?>
<br />
<br />
<?php if (!$hide_js) { php?>
 <a href='javascript:window.close();'>
		<span class="small"><?php echo _PROMPT_CLOSE;?></span>
 </a>
<?php
		}
	}
	/***
	 * Common Routine to display properties
	 ***/
	function list_properties(&$prop, &$caption) {
		global $Itemid, $task, $my, $mosConfig_live_site, $mosConfig_absolute_path;
		global $hp_imgdir_thumb, $hp_currency, $hp_imgsize_thumb, $hp_img_noimage_thumb, $hp_thousand_sep, $hp_dec_point, $hp_link_open_newwin, $hp_show_thumb, $hp_dec_string, $hp_thousand_string;

		if(empty($prop)) {
			?>
			<div id="hp_error_empty">
				<?php echo _HP_PROP_ERROR_EMPTY; ?>
			</div>
			<?php
		} else {
			foreach($prop AS $p) {
				if ($p->thumb <> '') {
					$thumb_imgsize = GetImageSize ($mosConfig_absolute_path.$hp_imgdir_thumb.$p->thumb);
				} else {
					$thumb_imgsize = GetImageSize ($mosConfig_absolute_path.$hp_imgdir_thumb.$hp_img_noimage_thumb);
				}
			?>
		<div class="hp_prop">
				<div class="hp_details">
						<?php if ($hp_show_thumb) { ?>
					<div class="img_thumb"><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=view&id=$p->id&Itemid=$Itemid"); ?>"><img <?php echo $thumb_imgsize[3]; ?> border="0" src="<?php
						if ($p->thumb <> '') echo $mosConfig_live_site.$hp_imgdir_thumb.$p->thumb;
						else echo $mosConfig_live_site.$hp_imgdir_thumb.$hp_img_noimage_thumb;
					?>" alt="<?php echo $p->thumb_title ?>" /></a></div>
					<?php } ?>
					<div class="datos_oferta">
                                          <div class="nombre_objeto">
                        			<a class="hp_title" href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=view&id=$p->id&Itemid=$Itemid"); ?>"><?php echo $p->name; ?></a>
                                          </div>
                                          <div class="botonera_dcha">
                                                    <?php
							# Show an edit icon to allow user to edit the property
							if ($p->user == $my->id && $p->user > 0 && $my->id > 0) { ?>
							&nbsp;
							<a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=editprop&id=$p->id&Itemid=$Itemid"); ?>" title="<?php echo _E_EDIT; ?>"><img src="administrator/images/editar.png" alt="<?= _E_EDIT ?>" title="<?= _E_EDIT ?>"  onmouseover="this.src='administrator/images/editar_on.png';" onmouseout="this.src='administrator/images/editar.png'" class="bot_gestionar"/></a>
							<?php }
							?>
                                         </div>
                                         <br class="clearboth"/>
						      <?php
								/* Escupe los campos de manera poco flexible pero práctica */
								echo '<div class="columna_listado_izq">';
								// Población
								if (!$caption['suburb']->hideCaption)
									echo '<span class="hp_caption">'.$caption['suburb']->caption."</span>: ";
								echo $p->suburb.".<br />";
								// Provincia
								if (!$caption['state']->hideCaption)
									echo '<span class="hp_caption">'.$caption['state']->caption."</span>: ";
								echo $p->state.".<br />";

								// Barrio
								if (!$p->Barrio=="")
								{
									if (!$caption['Barrio']->hideCaption)

										echo '<span class="hp_caption">'.$caption['Barrio']->caption."</span>: ";
									echo $p->Barrio.".<br />";
								}


								// Nº plazas
								if ($p->typeid == 1 && $p->Num_plazas != "")
								{
									if (!$caption['Num_plazas']->hideCaption)
										echo '<span class="hp_caption">'.$caption['Num_plazas']->caption."</span>: ";
									echo $p->Num_plazas.".<br />";
								}

								// Metros construidos
								if ($p->MetrosConstruidos != "") // Venta
								{
									if (!$caption['MetrosConstruidos']->hideCaption)
										echo '<span class="hp_caption">'.$caption['MetrosConstruidos']->caption."</span>: ";
									echo $p->MetrosConstruidos." ".$caption['MetrosConstruidos']->append_text.".<br />";
								}

								// Año construccion
								if ($p->AnioConstruccion != "") // Venta
								{
									if (!$caption['AnioConstruccion']->hideCaption)
										echo '<span class="hp_caption">'.$caption['AnioConstruccion']->caption."</span>: ";
									echo $p->AnioConstruccion."<br />";
								}

								// Número de dormitorios
								if ($p->Numero_dormitorios != "") // Alquiler
								{
									if (!$caption['Numero_dormitorios']->hideCaption)
										echo '<span class="hp_caption">'.$caption['Numero_dormitorios']->caption."</span>: ";
									echo $p->Numero_dormitorios.".<br/>";
								}

								// Precio
								if ($p->price != "0") // Venta
								{
									if (!$caption['price']->hideCaption)
										echo '<span class="hp_caption">'.$caption['price']->caption."</span>: ";
									echo number_format($p->price,2,',','.')." ".$caption['price']->append_text."<br/>";
								}

								echo '</div>';

								echo '<div class="columna_listado_dcha">';
								// Cuartos de baño
								if (!$caption['Cuartos_banio_con_duchas']->hideCaption)
									echo '<span class="hp_caption">'.$caption['Cuartos_banio_con_duchas']->caption."</span>: ";
								if ($p->Cuartos_banio_con_duchas <> "")
									echo $p->Cuartos_banio_con_duchas.".  ";
								else
									echo _OFER_NINGUNO."  ";
								// Aseos
								if (!$caption['Aseos']->hideCaption)
									echo '<span class="hp_caption">'.$caption['Aseos']->caption."</span>: ";

								if ($p->Aseos <> "")
									echo $p->Aseos.".<br />";
								else
									echo _OFER_NINGUNO."<br />";
								// Dispone
								if (!$caption['Dispone']->hideCaption)
									echo '<span class="hp_caption">'.$caption['Dispone']->caption."</span>: ";

								echo str_replace("|",", ",$p->Dispone).".<br />";
								echo '</div>';
                                      ?>
                                      <?php
								echo '<br class="clearboth">';
								// Resumen
								# echo $p->Resumen."<br/><br/>";


								/* Antiguo código que escupÃ­a los campos */
								/*foreach($p as $key => $value) {
									if ( array_key_exists($key,$caption) && ($caption[$key]->name <> 'name' && $caption[$key]->name <> 'thumb' && $caption[$key]->name <> 'thumb_title' && $caption[$key]->name <> '' && $value <> "") )
										# Replace '|' with a comma for checkbox and select multiple fields
										if ($caption[$key]->field_type == "checkbox" || $caption[$key]->field_type == "selectmultiple") {
											if (!$caption[$key]->hideCaption) echo '<span class="hp_caption">'.$caption[$key]->caption."</span>: ";
											echo str_replace("|",", ",$value).".<br />";
										# Web Link
										} elseif ($caption[$key]->field_type == "link") {

											// Evaluate mambot style data
											$value = str_replace( '{property_id}', $p->id, $value );
											$value = str_replace( '{type_id}', $p->typeid, $value );
											$value = str_replace( '{agent_id}', $p->agentid, $value );
											$value = str_replace( '{company_id}', $p->companyid, $value );
											$value = str_replace( '{Itemid}', $Itemid, $value );

											if (!$caption[$key]->hideCaption) {
											?><span class="hp_caption"><?php echo $caption[$key]->caption; ?></span>: <?php }
											echo $caption[$key]->prefix_text;
											$link = explode("|",$value);
											if (count($link) == 1 && ( substr(trim($link[0]),0,4) == "http" || substr(trim($link[0]),0,5) == "index" ) ) {
												?><a <?php echo ($hp_link_open_newwin) ? 'target="_blank" ': ''; ?>href="<?php echo $link[0]; ?>"><?php echo $link[0]; ?></a><?php
											} elseif (count($link) > 1 && ( substr(trim($link[1]),0,4) == "http" || substr(trim($link[1]),0,5) == "index" ) ) {
												?><a <?php echo ($hp_link_open_newwin) ? 'target="_blank" ': ''; ?>href="<?php echo $link[1]; ?>"><?php echo $link[0]; ?></a><?php
											} else {
												echo $value;
											}
											echo $caption[$key]->prefix_text."<br />";

										} else {
												# Do not display agent field when viewing agent's properties
												# Do not display type field when viewing type's properties
												if ( !($key == "agent" && $task == "viewagent") && !($key =="type" && $task == "viewtype") ) {
													# Show agent link
													if ($key == "agent") {
														if (!$caption[$key]->hideCaption) {
														?><span class="hp_caption"><?php echo $caption[$key]->caption; ?></span>: <?php
														}
														?><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewagent&id=$p->agentid&Itemid=$Itemid"); ?>"><?php echo $caption[$key]->prefix_text.$value.$caption[$key]->append_text; ?></a><br /><?php
													# Show company link
													} elseif ($key == "company") {
														if (!$caption[$key]->hideCaption) {
														?><span class="hp_caption"><?php echo $caption[$key]->caption; ?></span>: <?php }
														?><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewco&id=".$p->companyid."&Itemid=$Itemid"); ?>">	<?php echo $caption[$key]->prefix_text.$value.$caption[$key]->append_text; ?></a><br /> <?php
													# Show type link
													} elseif ($key == "type") {
														if (!$caption[$key]->hideCaption) {
														?><span class="hp_caption"><?php echo $caption[$key]->caption; ?></span>: <?php }
														?><a href="<?php echo sefRelToAbs("index.php?option=com_hotproperty&task=viewtype&id=".$p->typeid."&Itemid=$Itemid"); ?>">	<?php echo $caption[$key]->prefix_text.$value.$caption[$key]->append_text; ?></a><br /> <?php
													# Show Price with proper formating
													} elseif ($key == "price") {
														if (!$caption[$key]->hideCaption) {
														?><span class="hp_caption"><?php echo $caption[$key]->caption; ?></span>:<?php }
														?><span class="hp_price"><?php echo $caption[$key]->prefix_text.$hp_currency." ".number_format($value, $hp_dec_point, $hp_dec_string, ($hp_thousand_sep) ? $hp_thousand_string:'').$caption[$key]->append_text; ?></span><br /> <?php
													# Show Featured as Yes/No instead of 1/0
													} elseif ($key == "featured") {
														if (!$caption[$key]->hideCaption) {
															echo '<span class="hp_caption">'.$caption[$key]->caption."</span>: ";
														}
														echo $caption[$key]->prefix_text
															.	( ($value == '1') ? _CMN_YES : _CMN_NO )
															.	$caption[$key]->append_text
															.	"<br />";
													# Else, show normal 'caption: value'
													} else {
														if (!$caption[$key]->hideCaption) {
															echo '<span class="hp_caption">'.$caption[$key]->caption."</span>: ";
														}
														echo $caption[$key]->prefix_text
															.	( ($key=="price") ? $hp_currency." " : "" )
															.	$value
															.	$caption[$key]->append_text
															.	"<br />";
													}
												}
										}
								}*/

							?>
					</div>
				</div>

			</div>
			<?php
			} // End Foreach
		} // End If
	}

	function seleccion_fecha ( $name ) {
	?>
			<?php
				if ($name == "desde")
				{
					echo "<div class=\"titulo_fecha\">"._HPAVL_DESDE."&nbsp;</div>";
				}
				else
				{
					echo "<div class=\"titulo_fecha\">"._HPAVL_HASTA."&nbsp;&nbsp;</div>";
				}
			?>
			<!-- Día -->
			<select size="1" name="<?php echo $name."_dia"; ?>" class="campo_fecha inputbox">
			<?php
				for ($i=1; $i<=31; $i++)
					echo "<option value='".$i."'>".$i."</option>";
			?>
			</select>

			<!-- Mes -->
			<select size="1" name="<?php echo $name."_mes"; ?>" class="campo_fecha inputbox">
			<?php
				for ($i=1; $i<=12; $i++)
					echo "<option value='".$i."'>".$i."</option>";
			?>
			</select>
			<!-- Año -->
			<select size="1" name="<?php echo $name."_año"; ?>" class="campo_fecha inputbox">
			<?php
				$hoy=getdate();
				for ($i=$hoy['year']; $i<=2080; $i++)
					echo "<option value='".$i."'>".$i."</option>";
			?>
			</select>
			<br class="clearboth"/>
	<?php
	}


        function show_ResumenTipos() {
          global $database;

          # Select published types
          $database->setQuery( "SELECT * FROM #__hp_prop_types AS t"
	. "\nWHERE t.published='1'"
	. "\nORDER BY t.ordering ASC");
          $types = $database->loadObjectList();

          foreach($types AS $t) { ?>
              <a href="<?php echo sefRelToAbs('index.php?option=com_hotproperty&task=viewtype&id='.$t->id.'&Itemid='.$Itemid); ?>"><?= $t->name ?> ( <?= getNumOfertas($t->id) ?>)</a>
          <?php }
        }

        function getNumOfertas($type_id) {
          $database->setQuery( "SELECT id FROM #__hp_properties AS p"
          . "\nWHERE p.type=".$type_id);
          return $database->getNumRows();
        }
}


?>
