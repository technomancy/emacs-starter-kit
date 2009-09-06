<?php

class Foo {
  public function foo() {
    $foo = <<<EOT_SQL;
    I am a heredoc
EOT_SQL
  }
}
