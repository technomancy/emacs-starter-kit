$text = 'Text from a Perl string.';
print <<HTML; 
<html>
<head>
<title>Here-Doc Example</title>
</head>
<body>
<h1>Here-Doc Example</h1>
<p>$text</p>
</body>
HTML
