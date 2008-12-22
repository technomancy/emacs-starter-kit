<?php

Zend_Loader::loadClass("Category","application/models");
Zend_Loader::loadClass("CategoryDisplayName","application/models");

/** Zend_Controller_Action */
require_once 'Zend_Global_Controller_Action.php';

class CategoryController extends Zend_Global_Controller_Action
{
    public function indexAction($notices = array(), $errors = array())
    {

        $request = $this->GetRequest();
        blah, etc.
    }
}

