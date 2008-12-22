<?php

Zend_Loader::loadClass('Account','application/models');
Zend_Loader::loadClass('Device','application/models');
Zend_Loader::loadClass('Carrier','application/models');
Zend_Loader::loadClass('DistributionChannel','application/models');

/** Zend_Controller_Action */
require_once 'Zend_Global_Controller_Action.php';

class AccountController extends Zend_Global_Controller_Action
{
    public function indexAction($errors = array())
    {

     $this->view->assign('errors',$errors);
     $this->view->assign('body','account_index.htm');
echo $this->view->render('main_doc.htm');

    }
}
