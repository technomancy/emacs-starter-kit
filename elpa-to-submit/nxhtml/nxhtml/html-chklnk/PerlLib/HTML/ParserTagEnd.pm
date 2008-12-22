package HTML::ParserTagEnd;

# Author address: <gisle@aas.no>
### Modified for <tag />, Lennart

use strict;
use HTML::Entities ();

use vars qw($VERSION);
$VERSION = "2.23";  # $Date: 1999/06/09 10:27:16 $


sub new
{
    my $class = shift;
    my $self = bless { '_buf'            => '',
		       '_strict_comment' => 0,
		     }, $class;
    $self;
}


# A little note about the observed Netscape behaviour:
#
# It parse <xmp> in the depreceated 'literal' mode, i.e. no tags are
# recognized until a </xmp> is found.
# 
# <listing> is parsed like <pre>, i.e. tags are recognized.  <listing>
# are presentend in smaller font than <pre>
#
# Netscape does not parse this comment correctly (it terminates the comment
# too early):
#
#    <! -- comment -- --> more comment -->
#
# Netscape ignores '<!--' and '-->' within the <SCRIPT> and <STYLE> tag.
# This is used as a trick to make non-script-aware browsers ignore
# the scripts.


sub parse
{
    my $self = shift;
    my $buf = \ $self->{'_buf'};
    unless (defined $_[0]) {
	# signals EOF (assume rest is plain text)
	$self->text($$buf) if length $$buf;
	$$buf = '';
	return $self;
    }
    $$buf .= $_[0];
    my $netscape_comment = !$self->{'_strict_comment'};

    # Parse html text in $$buf.  The strategy is to remove complete
    # tokens from the beginning of $$buf until we can't deside whether
    # it is a token or not, or the $$buf is empty.

  TOKEN:
    while (1) {

	# First we try to pull off any plain text (anything before a "<" char)
	if ($$buf =~ s|^([^<]+)||) {
	    if (length $$buf) {
		$self->text($1);
	    } else {
		my $text = $1;
		# At the end of the buffer, we should not parse white space
		# but leave it for parsing on the next round.
		if ($text =~ s|(\s+)$||) {
		    $$buf = $1;
                # Same treatment for chopped up entites and words.
		# We must wait until we have it all.
		} elsif ($text =~ s|(\s*\S+)$||) {
		    $$buf = $1;
		};
		$self->text($text) if length $text;
		last TOKEN;
	    }

	# Netscapes buggy comments are easy to handle
	} elsif ($netscape_comment && $$buf =~ m|^<!\s*--|) {
	    if ($$buf =~ s|^<!\s*--(.*?)--\s*>||s) {
		$self->comment($1);
	    } else {
		last TOKEN;  # must wait until we see the end of it
	    }

	# Then, markup declarations (usually either <!DOCTYPE...> or a comment)
	} elsif ($$buf =~ s|^(<!)||) {
	    my $eaten = $1;
	    my $text = '';
	    my @com = ();  # keeps comments until we have seen the end
	    # Eat text and beginning of comment
	    while ($$buf =~ s|^(([^>]*?)--)||) {
		$eaten .= $1;
		$text .= $2;
		# Look for end of comment
		if ($$buf =~ s|^((.*?)--)||s) {
		    $eaten .= $1;
		    push(@com, $2);
		} else {
		    # Need more data to get all comment text.
		    $$buf = $eaten . $$buf;
		    last TOKEN;
		}
	    }
	    # Can we finish the tag
	    if ($$buf =~ s|^([^>]*)>||) {
		$text .= $1;
		$self->declaration($text) if $text =~ /\S/;
		# then tell about all the comments we found
		for (@com) { $self->comment($_); }
	    } else {
		$$buf = $eaten . $$buf;  # must start with it all next time
		last TOKEN;
	    }

        # Should we look for 'processing instructions' <? ...> ??
	#} elsif ($$buf =~ s|<\?||) {
	    # ...

	# Then, look for a end tag
	} elsif ($$buf =~ s|^</||) {
	    # end tag
	    if ($$buf =~ s|^([a-zA-Z][a-zA-Z0-9\.\-]*)(\s*>)||) {
		$self->end(lc($1), "</$1$2");
	    } elsif ($$buf =~ m|^[a-zA-Z]*[a-zA-Z0-9\.\-]*\s*$|) {
		$$buf = "</" . $$buf;  # need more data to be sure
		last TOKEN;
	    } else {
		# it is plain text after all
		$self->text("</");
	    }

	# Then, finally we look for a start tag
	} elsif ($$buf =~ s|^(<([a-zA-Z]+)>)||) {
	    # special case plain start tags for slight speed-up (2.5%)
	    ### mod Lennart
	    $self->start(lc($2), {}, 0, [], $1);

	} elsif ($$buf =~ s|^<||) {
	    # start tag
	    my $eaten = '<';

	    # This first thing we must find is a tag name.  RFC1866 says:
	    #   A name consists of a letter followed by letters,
	    #   digits, periods, or hyphens. The length of a name is
	    #   limited to 72 characters by the `NAMELEN' parameter in
	    #   the SGML declaration for HTML, 9.5, "SGML Declaration
	    #   for HTML".  In a start-tag, the element name must
	    #   immediately follow the tag open delimiter `<'.
	    if ($$buf =~ s|^(([a-zA-Z][a-zA-Z0-9\.\-]*)\s*)||) {
		$eaten .= $1;
		my $tag = lc $2;
		my %attr;
		my @attrseq;

		# Then we would like to find some attributes
                #
                # Arrgh!! Since stupid Netscape violates RCF1866 by
                # using "_" in attribute names (like "ADD_DATE") of
                # their bookmarks.html, we allow this too.
		while ($$buf =~ s|^(([a-zA-Z][a-zA-Z0-9\.\-_]*)\s*)||) {
		    $eaten .= $1;
		    my $attr = lc $2;
		    my $val;
		    # The attribute might take an optional value (first we
		    # check for an unquoted value)
		    if ($$buf =~ s|(^=\s*([^\"\'>\s][^>\s]*)\s*)||) {
			$eaten .= $1;
			$val = $2;
			HTML::Entities::decode($val);
		    # or quoted by " or '
		    } elsif ($$buf =~ s|(^=\s*([\"\'])(.*?)\2\s*)||s) {
			$eaten .= $1;
			$val = $3;
			HTML::Entities::decode($val);
                    # truncated just after the '=' or inside the attribute
		    } elsif ($$buf =~ m|^(=\s*)$| or
			     $$buf =~ m|^(=\s*[\"\'].*)|s) {
			$$buf = "$eaten$1";
			last TOKEN;
		    } else {
			# assume attribute with implicit value
			$val = $attr;
		    }
		    $attr{$attr} = $val;
		    push(@attrseq, $attr);
		}

		# At the end there should be a closing ">"
### Modified for <tag />, Lennart
		if ($$buf =~ s|^/>||) {
		    $self->start($tag, \%attr, 1, \@attrseq, "$eaten>");
		} elsif ($$buf =~ s|^>||) {
		#if ($$buf =~ s|^>||) {
		    $self->start($tag, \%attr, 0, \@attrseq, "$eaten>");
		} elsif (length $$buf) {
		    # Not a conforming start tag, regard it as normal text
		    $self->text($eaten);
		} else {
		    $$buf = $eaten;  # need more data to know
		    last TOKEN;
		}

	    } elsif (length $$buf) {
		$self->text($eaten);
	    } else {
		$$buf = $eaten . $$buf;  # need more data to parse
		last TOKEN;
	    }

	} else {
	    #die if length($$buf);  # This should never happen
	    last TOKEN; 	    # The buffer should be empty now
	}
    }

    $self;
}


sub eof
{
    shift->parse(undef);
}


sub parse_file
{
    my($self, $file) = @_;
    no strict 'refs';  # so that a symbol ref as $file works
    local(*F);
    unless (ref($file) || $file =~ /^\*[\w:]+$/) {
	# Assume $file is a filename
	open(F, $file) || die "Can't open $file: $!";
	$file = \*F;
    }
    my $chunk = '';
    while(read($file, $chunk, 512)) {
	$self->parse($chunk);
    }
    close($file);
    $self->eof;
}


sub strict_comment
{
    my $self = shift;
    my $old = $self->{'_strict_comment'};
    $self->{'_strict_comment'} = shift if @_;
    return $old;
}


sub netscape_buggy_comment  # legacy
{
    my $self = shift;
    my $old = !$self->strict_comment;
    $self->strict_comment(!shift) if @_;
    return $old;
}


sub text
{
    # my($self, $text) = @_;
}

sub declaration
{
    # my($self, $decl) = @_;
}

sub comment
{
    # my($self, $comment) = @_;
}

sub start
{
die "hie";
    # my($self, $tag, $attr, $attrseq, $origtext) = @_;
    # $attr is reference to a HASH, $attrseq is reference to an ARRAY
}

sub end
{
    # my($self, $tag, $origtext) = @_;
}

1;


__END__


=head1 NAME

HTML::Parser - SGML parser class

=head1 SYNOPSIS

 require HTML::Parser;
 $p = HTML::Parser->new;  # should really a be subclass
 $p->parse($chunk1);
 $p->parse($chunk2);
 #...
 $p->eof;                 # signal end of document

 # Parse directly from file
 $p->parse_file("foo.html");
 # or
 open(F, "foo.html") || die;
 $p->parse_file(\*F);

=head1 DESCRIPTION

The C<HTML::Parser> will tokenize an HTML document when the parse()
method is called by invoking various callback methods.  The document to
be parsed can be supplied in arbitrary chunks.

The external interface the an I<HTML::Parser> is:

=over 4

=item $p = HTML::Parser->new

The object constructor takes no arguments.

=item $p->parse( $string );

Parse the $string as an HTML document.  Can be called multiple times.
The return value is a reference to the parser object.

=item $p->eof

Signals end of document.  Call eof() to flush any remaining buffered
text.  The return value is a reference to the parser object.

=item $p->parse_file( $file );

This method can be called to parse text from a file.  The argument can
be a filename or an already opened file handle. The return value from
parse_file() is a reference to the parser object.

=item $p->strict_comment( [$bool] )

By default we parse comments similar to how the popular browsers (like
Netscape and MSIE) do it.  This means that comments will always be
terminated by the first occurrence of "-->".  This is not correct
according to the "official" HTML standards.  The official behaviour
can be enabled by calling the strict_comment() method with a TRUE
argument.

The return value from strict_comment() is the old attribute value.

=back



In order to make the parser do anything interesting, you must make a
subclass where you override one or more of the following methods as
appropriate:

=over 4

=item $self->declaration($decl)

This method is called when a I<markup declaration> has been
recognized.  For typical HTML documents, the only declaration you are
likely to find is <!DOCTYPE ...>.  The initial "<!" and ending ">" is
not part of the string passed as argument.  Comments are removed and
entities will B<not> be expanded.

=item $self->start($tag, $attr, $attrseq, $origtext)

This method is called when a complete start tag has been recognized.
The first argument is the tag name (in lower case) and the second
argument is a reference to a hash that contain all attributes found
within the start tag.  The attribute keys are converted to lower case.
Entities found in the attribute values are already expanded.  The
third argument is a reference to an array with the lower case
attribute keys in the original order.  The fourth argument is the
original HTML text.


=item $self->end($tag, $origtext)

This method is called when an end tag has been recognized.  The
first argument is the lower case tag name, the second the original
HTML text of the tag.

=item $self->text($text)

This method is called when plain text in the document is recognized.
The text is passed on unmodified and might contain multiple lines.
Note that for efficiency reasons entities in the text are B<not>
expanded.  You should call HTML::Entities::decode($text) before you
process the text any further.

A sequence of text in the HTML document can be broken between several
invocations of $self->text.  The parser will make sure that it does
not break a word or a sequence of spaces between two invocations of
$self->text().

=item $self->comment($comment)

This method is called as comments are recognized.  The leading and
trailing "--" sequences have been stripped off the comment text.

=back

The default implementation of these methods do nothing, i.e., the
tokens are just ignored.

There is really nothing in the basic parser that is HTML specific, so
it is likely that the parser can parse other kinds of SGML documents.
SGML has many obscure features (not implemented by this module) that
prevent us from renaming this module as C<SGML::Parser>.

=head1 EFFICIENCY

The parser is fairly inefficient if the chunks passed to $p->parse()
are too big.  The reason is probably that perl ends up with a lot of
character copying when tokens are removed from the beginning of the
strings.  A chunk size of about 256-512 bytes was optimal in a test I
made with some real world HTML documents.  (The parser was about 3
times slower with a chunk size of 20K).

=head1 SEE ALSO

L<HTML::Entities>, L<HTML::TokeParser>, L<HTML::Filter>,
L<HTML::HeadParser>, L<HTML::LinkExtor>

L<HTML::TreeBuilder> (part of the I<HTML-Tree> distribution)

=head1 COPYRIGHT

Copyright 1996-1999 Gisle Aas. All rights reserved.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut


