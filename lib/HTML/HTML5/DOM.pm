package HTML::HTML5::DOMutil::AutoDoc;

use 5.010;
use common::sense;
use mro 'c3';

use Capture::Attribute;

sub psay
{
	foreach my $line (@_)
	{
		say $line;
		say "";
	}
}

sub add
{
	my ($class, $package, $sub, $doc) = @_;
	my $docs = \%{"$package\::DOCUMENTATION"};
	$docs->{$sub} = $doc;
}

sub pod_for :Capture
{
	my ($class, $package) = @_;

	my ($interface) = ($package =~ m{ :: ([^:]+) $ }x);

	psay
		q{=head1 NAME},
		"$package - implementation of the $interface interface of the HTML DOM";
	psay
		q{=head1 DESCRIPTION},
		"$package is an implementation of the $interface interface of the HTML DOM. See L<HTML::HTML5::DOM> for a list of the conventions that have been used when translating the DOM to Perl.";

	foreach my $s (qw/_pod_elements _pod_isa _pod_methods/)
	{
		print $class->$s($package);
	}
	
	psay
		q{=head1 BUGS},
		q{L<http://rt.cpan.org/Dist/Display.html?Queue=HTML-HTML5-DOM>.},
		q{=head1 SEE ALSO},
		q{L<HTML::HTML5::DOM>.},
		q{=head1 AUTHOR},
		q{Toby Inkster E<lt>tobyink@cpan.orgE<gt>.},
		q{=head1 COPYRIGHT AND LICENCE},
		q{This software is copyright (c) 2012 by Toby Inkster.},
		q{This is free software; you can redistribute it and/or modify it under the same terms as the Perl 5 programming language system itself.},
		q{=head1 DISCLAIMER OF WARRANTIES},
		q{THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.},
		q{};	
}

sub _pod_elements :Capture
{
	my ($class, $package) = @_;

	psay
		q{=head2 HTML Elements},
		q{This class applies to the following elements.},
		q{=over};
	
	my $elements = \@{"$package\::ELEMENTS"};
	
	foreach my $element (sort @$elements)
	{
		psay "=item * C<< $element >>";
	}
	
	psay q{=back};
}

sub _pod_isa :Capture
{
	my ($class, $package) = @_;

	psay
		q{=head2 Inheritance},
		q{This class inherits methods from the following Perl classes.},
		q{=over};
	
	foreach my $parent (@{ mro::get_linear_isa($package) })
	{
		psay "=item * L<$parent>";
	}
	
	psay q{=back};
}

sub _pod_methods :Capture
{
	my ($class, $package) = @_;

	my $docs = \%{"$package\::DOCUMENTATION"};

	unless (keys %$docs)
	{
		psay
			q{=head2 Additional Methods},
			q{This class provides no additional methods over those it inherits.},
			q{It is mostly pointless, but its existance is required by the HTML DOM.};
		return;
	}

	psay
		q{=head2 Additional Methods},
		q{As well as its inherited methods, this class provides the following methods.},
		q{=over};
		
	foreach my $meth (sort keys %$docs)
	{
		psay
			"=item * C<< $meth >>",
			$docs->{$meth};
	}
	
	psay q{=back};
}

package HTML::HTML5::DOM;

use 5.010;
use common::sense;
use mro 'c3';

use constant XHTML_NS => 'http://www.w3.org/1999/xhtml';

my $me;
BEGIN { $me = bless {}, __PACKAGE__ }

use DateTime qw//;
use URI qw//;

sub getDOMImplementation
{
	return $me;
}

sub hasFeature
{
	my ($self, $feature, $version) = @_;
	
	my $have = [
		qr{ ^ (XML) \s (1\.0|2\.0) $ }xi,
		qr{ ^ (Core) \s (1\.0|2\.0) $ }xi,
		# Don't have all of HTML yet.
		];
	
	return ("${feature} ${version}" ~~ $have);
}

sub parseString
{
	my ($self, $string) = @_;
	my $dom = HTML::HTML5::Parser->new->parse_string($string);
	XML::LibXML::Augment->upgrade($dom);
	return $dom;
}

sub parse
{
	my ($self, $file) = @_;
	my $dom = (ref $file =~ /^IO\b/)
		? HTML::HTML5::Parser->new->parse_fh($file)
		: HTML::HTML5::Parser->new->parse_file($file);
	XML::LibXML::Augment->upgrade($dom);
	return $dom;
}

package HTML::HTML5::DOM::HTMLDocument;

use 5.010;
use common::sense;
use mro 'c3';

use XML::LibXML::Augment 0
	'-type'  => 'Document',
	'-names' => ['{'.HTML::HTML5::DOM->XHTML_NS.'}html'];

use Carp qw//;

foreach my $elem (qw/body head/)
{
	*{$elem} = sub
	{
		my ($self) = @_;
		my ($node1) = $self->getElementsByTagName($elem);
		return $node1;
	};
	
	HTML::HTML5::DOMutil::AutoDoc->add(
		__PACKAGE__,
		$elem,
		"Returns the document ${elem}.",
		);
}

{
	my @things = (
		[ images  => '//*[local-name()="img"]',    'images' ],
		[ embeds  => '//*[local-name()="embed"]',  'C<< <embed> >> elements' ],
		[ plugins => '//*[local-name()="embed"]',  'C<< <embed> >> elements' ],
		[ applets => '//*[(local-name()="applet") or (local-name()="object" and @codetype="application/java")]', 'C<< <applet> >> elements (and C<< <object codetype="application/java"> >> elements)' ],
		[ links   => '//*[(local-name()="a" or local-name()="area") and @href]', 'C<< <a> >> and C<< <area> >> elements with an "href" attribute' ],
		[ anchors => '//*[local-name()="a" and @name]', 'C<< <a> >> with a "name" attribute' ],
		[ forms   => '//*[local-name()="form"]',   'forms' ],
		[ scripts => '//*[local-name()="script"]', 'scripts' ],
		);
	foreach my $x (@things)
	{
		*{$x->[0]} = sub
		{
			my ($self) = @_;
			my (@nodes) = $self->findnodes($x->[1]);
			wantarray ? @nodes : HTML::HTML5::DOM::HTMLCollection->new(@nodes);
		};
		HTML::HTML5::DOMutil::AutoDoc->add(
			__PACKAGE__,
			$x->[0],
			"Returns all $x->[2] found in the document.",
			);
	}
}

sub compatMode
{
	my ($self) = @_;
	if (UNIVERSAL::can('HTML::HTML5::Parser', 'can'))
	{
		if (HTML::HTML5::Parser->can('compat_mode'))
		{
			my $mode = HTML::HTML5::Parser->compat_mode($self);
			return $mode if $mode;
		}
	}
	return;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'compatMode',
	"Returns the string 'quirks' or 'limited quirks' or undef.",
	);

sub URL
{
	my $self = shift;
	$self->setURI(shift) if @_;
	return URI->new($self->URI);
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'URL',
	"Get/set the document's URL.",
	);

sub domain
{
	(shift)->URL->host;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'domain',
	"The documents URL's host name.",
	);

*cookie = *referrer = *referer = sub { q() };

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'cookie',
	"Ostensibly returns cookies associated with the document, but in this implementation always returns an empty string.",
	);

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'referrer',
	"Ostensibly returns the HTTP referer for the document, but in this implementation always returns an empty string.",
	);

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'referer',
	"An alias for 'referrer' provided for the benefit of those who learnt to spell by reading HTTP RFCs.",
	);

sub lastModified
{
	return DateTime->now;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'lastModified',
	"Ostensibly returns the HTTP Last-Modified date for the document, but this implementation always returns the current date and time. Returns a L<DateTime> object.",
	);

{
	my $cs = HTML::HTML::Parser->can('charset');
	
	sub charset
	{
		my $self = @_;
		if (@_)
		{
			$self->setEncoding(@_);
		}
		return $self->encoding;
	}

	HTML::HTML5::DOMutil::AutoDoc->add(
		__PACKAGE__,
		'charset',
		"Getter/setter for the document encoding.",
		);

	sub defaultCharset
	{
		return 'utf-8';
	}

	HTML::HTML5::DOMutil::AutoDoc->add(
		__PACKAGE__,
		'defaultCharset',
		"Returns the string 'utf-8'.",
		);

	sub characterSet
	{
		return unless $cs;
		return $cs->( shift );
	}
	
	HTML::HTML5::DOMutil::AutoDoc->add(
		__PACKAGE__,
		'characterSet',
		"Returns the character set that the document was parsed as (if known). As C<charset> can be used as a setter, this is not necessarily the same as C<charset>.",
		);
}

sub readyState
{
	'complete';
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'lastModified',
	"Ostensibly returns the current document readiness, but this implementation always returns the string 'complete'.",
	);

sub title
{
	my ($self) = @_;
	my ($title) = $self->getElementsByTagName('title')->get_index(1)->textContent;
	$title =~ s/\s+/ /g;
	$title =~ s/(^\s|\s$)//g;
	return $title;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'title',
	"Returns the document's title, from its C<< <title> >> element, with a little whitespace canonicalisation.",
	);

our $AUTOLOAD;
sub AUTOLOAD
{
	my $self = shift;
	my ($func) = ($AUTOLOAD =~ m{ :: (\w+) $ }x);
	if ($func)
	{
		my $coderef = HTML::HTML5::DOM::HTMLHtmlElement->can($func);
		if ($coderef)
		{
			unshift @_, $self->documentElement;
			goto $coderef;
		}
	}
	Carp::croak "Method '$AUTOLOAD' could not be autoloaded";
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'AUTOLOAD',
	"See L<perlsub> if you don't know the significance of the AUTOLOAD function. HTML::HTML5::DOM::HTMLDocument will pass through unknown menthods to the document's root element. So for example, C<< $document->setAttribute >> will actually set an attribute on the document's root element.",
	);

package HTML::HTML5::DOM::HTMLCollection;

use parent qw/XML::LibXML::NodeList/;

package HTML::HTML5::DOM::HTMLElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/abbr address article aside b bdi bdo cite code
			dd dfn dt em figcaption figure footer header
			hgroup i kbd mark nav noscript rp rt ruby s samp
			section small strong sub summary sup u var wbr/;
}

use HTML::HTML5::Parser 0.110;
use HTML::HTML5::Writer 0.104;
use List::Util 0 qw//;
use Scalar::Util 0 qw//;
use XML::LibXML 1.91 qw/:all/;
use XML::LibXML::Augment 0 -names => [@ELEMENTS];
use XML::LibXML::QuerySelector 0;

sub _mk_attribute_accessors
{
	no strict 'refs';
	
	my ($class, @attribs) = @_;
	foreach (@attribs)
	{
		my ($subname, $xmlname, $type) = split /=/;
		$xmlname ||= $subname;
				
		if ($type eq 'LIST')
		{
			*{"$class\::$subname"} = sub
			{
				my $self = shift;
				my $i = 1;
				my %classes =
					reverse            # ensure that *first* copy of dupes is kept
					map { $_ => $i++ } # filter out duplicates while preserving order
					grep { length $_ } # ignore nulls
					split /\s+/,       # space separated list
					$self->getAttribute($xmlname);
				return
					sort { $classes{$a} <=> $classes{$b} } # restore order
					keys %classes;
			};
			HTML::HTML5::DOMutil::AutoDoc->add(
				$class,
				$subname,
				sprintf(
					'Splits C<< $elem->getAttribute("%s") >> into a list on whitespace.',
					$xmlname,
					),
				);
		}
		elsif ($type eq 'URI' || $type eq 'URL')
		{
			*{"$class\::$subname"} = sub
			{
				my $elem = shift;
				if (@_)
				{
					my $newval = shift;
					defined $newval ?
						$elem->setAttribute($xmlname, $newval) :
						$elem->removeAttribute($xmlname);
				}
				my $base = $elem->baseURI // $elem->ownerDocument->URI;
				return $base ?
					URI->new_abs($elem->getAttribute($xmlname), $base):
					URI->new($elem->getAttribute($xmlname));
			};
			HTML::HTML5::DOMutil::AutoDoc->add(
				$class,
				$subname,
				sprintf(
					'Called with no arguments, is a shortcut for C<< $elem->getAttribute("%s") >> but as a blessed L<URI> object. Called with a defined argument, acts as C<setAttribute>. Called with undef as an argument, acts as C<removeAttribute>.',
					$xmlname,
					),
				);
		}
		elsif ($type eq 'TEXT')
		{
			*{"$class\::$subname"} = sub
			{
				my $self = shift;
				if (@_)
				{
					$self->removeChildNodes;
					$self->appendText(join qq(\n), @_);
				}
				$self->textContent;
			};
			HTML::HTML5::DOMutil::AutoDoc->add(
				$class,
				$subname,
				sprintf(
					'Called with no arguments, acts as an alias for C<< $elem->textContent >>. Called with an arguments, sets the content for the element. Any existing content will be overwritten. If multiple arguments are provided, they\'ll be joined using "\n".',
					),
				);
		}
		elsif ($type eq 'boolean')
		{
			*{"$class\::$subname"} = sub
			{
				my $elem = shift;
				if (@_)
				{
					my $newval = shift;
					$newval ?
						$elem->setAttribute($xmlname, $xmlname) :
						$elem->removeAttribute($xmlname);
				}
				$elem->hasAttribute($xmlname)
			};
			HTML::HTML5::DOMutil::AutoDoc->add(
				$class,
				$subname,
				sprintf(
					'Called with no arguments, is a shortcut for C<< $elem->hasAttribute("%s") >>. If called with a true argument, will C<setAttribute>; if called with a false argument will C<removeAttribute>.',
					$xmlname,
					),
				);
		}
		else
		{
			*{"$class\::$subname"} = sub
			{
				my $elem = shift;
				if (@_)
				{
					my $newval = shift;
					defined $newval ?
						$elem->setAttribute($xmlname, $newval) :
						$elem->removeAttribute($xmlname);
				}
				$elem->{$xmlname}
			};
			HTML::HTML5::DOMutil::AutoDoc->add(
				$class,
				$subname,
				sprintf(
					'Called with no arguments, is a shortcut for C<< $elem->getAttribute("%s") >>. Called with a defined argument, acts as C<setAttribute>. Called with undef as an argument, acts as C<removeAttribute>.',
					$xmlname,
					),
				);
		}
	}
}

sub _mk_url_decomposition
{
	no strict 'refs';
	
	my ($class, $via, $bits) = @_;
	$via  ||= 'href';
	$bits ||= {
		protocol => 'scheme',
		host     => 'host_port',
		hostname => 'host',
		port     => 'port',
		pathname => 'path',
		search   => 'query',
		hash     => 'fragment',
		};
	foreach my $bit (keys %$bits)
	{
		my $method = $bits->{$bit};
		*{"$class\::$bit"} = sub { (shift)->$via->$method };
		HTML::HTML5::DOMutil::AutoDoc->add(
			$class,
			$bit,
			sprintf(
				'A shortcut for C<< $elem->%s->%s >>. (Does not act as a setter.)',
				$via,
				$method,
				),
			);
	}
}

sub _mk_labels_method
{
	no strict 'refs';
	
	my ($class, $subname) = @_;
	$subname ||= 'labels';
	
	*{"$class\::$subname"} = sub {
		my $self = shift;
		my @labels = grep
			{ $_->can('control') && $_->control eq $self }
			$self->ownerDocument->getElementsByTagName('label');
		return wantarray ? @labels : XML::LibXML::NodeList->new(@labels);
	};

	HTML::HTML5::DOMutil::AutoDoc->add(
		$class,
		$subname,
		'A list of C<< <label> >> elements which label this element.',
		);
}

sub _mk_follow_method
{
	no strict 'refs';
	
	my ($class, $subname, $via) = @_;
	$subname ||= 'wm_follow';
	$via     ||= 'href';
	
	*{"$class\::$subname"} = sub {
		my $self = shift;
		my $url  = $self->$via;
		return Web::Magic->new(GET => "$url");
	};

	HTML::HTML5::DOMutil::AutoDoc->add(
		$class,
		$subname,
		sprintf('Shortcut for C<< Web::Magic->new(GET => $elem->%s) >>', $via),
		);
}

sub _mk_form_methods
{
	no strict 'refs';
	
	my ($class, $todo) = @_;
	$todo ||= sub { 1 };
	
	if ('form' ~~ $todo)
	{
		*{"$class\::form"} = sub
		{
			my $self = shift;
			if ($self->hasAttribute('form'))
			{
				my $form = $self->documentElement->getElementById(
					$self->getAttribute('form'));
				return $form if $form;
			}
			return
				List::Util::first { $_->nodeName eq 'form' }
				$self->wm_ancestors;
		};
		HTML::HTML5::DOMutil::AutoDoc->add(
			$class,
			'form',
			'Returns the "form owner" for this element.',
			);
	}
	
	foreach my $x (qw/Action Enctype Method NoValidate Target/)
	{
		next unless lc("form$x") ~~ $todo;
		
		*{"$class\::form$x"} = sub
		{
			my $self = shift;
			if ($self->hasAttribute(lc "form$x"))
			{
				return $self->getAttribute(lc "form$x")
			}
			return $self->form->getAttribute(lc $x);
		};
		HTML::HTML5::DOMutil::AutoDoc->add(
			$class,
			'form',
			sprintf('Returns the "form%s" attribute for this element if it exists, or otherwise the "%s" attribute of this element\'s form owner.', lc $x, lc $x),
			);
	}
}

sub getElementById
{
	my ($self, $id) = @_;
	my @nodes = $self->findnodes("*[\@id=\"$id\"]");
	return $nodes[0];
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'getElementById',
	'The world-famous C<getElementById> method.',
	);

*getElementsById = \&getElementById; # common typo

sub getElementsByClassName
{
	my $self = shift;
	my $conditions = join q{ or },
		map { "contains(concat(' ', normalize-space(\@class), ' '), ' $_ ')" }
		@_;
	my @rv = $self->findnodes("*[$conditions]");
	return wantarray ? @rv : HTML::HTML5::DOM::HTMLCollection->new(@rv);
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'getElementsByClassName',
	'Given one or more class names, returns a list of elements bearing those classes.',
	);

sub outerHTML
{
	my $self = shift;
	
	if (@_)
	{
		my $parser = HTML::HTML5::Parser->new;
		if ($self->parentNode and $self->parentNode->nodeType==XML_ELEMENT_NODE)
		{
			my @nodes = $parser->parse_balanced_chunk(
				(join qq(\n), @_),
				{ within => $self->parentNode->nodeName, as => 'list' },
				);
			$self->parentNode->insertBefore($_, $self) for @nodes;
			$self->parentNode->removeChild($self);
		}
		else
		{
			$self
				-> ownerDocument
				-> setDocumentElement(
					$parser->parse_string(join qq(\n), @_)->documentElement
					);
		}
		return join qq(\n), @_;
	}
	
	my $writer = HTML::HTML5::Writer->new(markup => 'html', polyglot => 1);
	$writer->element($self);
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'outerHTML',
	'As per innerHTML, but includes the element itself. Can be used as a setter, but that\'s a bit of a weird thing to do.',
	);

sub innerHTML
{
	my $self = shift;
	if (@_)
	{
		my $parser = HTML::HTML5::Parser->new;
		my @nodes  = $parser->parse_balanced_chunk(
			(join qq(\n), @_),
			{ within => $self->nodeName, as => 'list' },
			);
		$self->removeChildNodes;
		$self->appendChild($_) for @nodes;
	}
	
	my $writer;
	join q{},
		map
		{ 
			$writer ||= HTML::HTML5::Writer->new(markup => 'html', polyglot => 1);
			
			if ($_->nodeType == XML_ELEMENT_NODE)
			{
				$writer->element($_)
			}
			elsif ($_->nodeType == XML_TEXT_NODE)
			{
				$writer->text($_)
			}
			else
			{
				$_->toString
			}
		}
		$self->childNodes	
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'innerHTML',
	'When called without arguments, serialises the contents of the element (but not the element itself) to a single string. When called with a string argument, parses the string as HTML and uses it to set the content of this element. When possible, attempts to use polyglot HTML (i.e. markup that works as HTML and XHTML).',
	);

sub wm_ancestors
{
	my ($self) = @_;
	my $x = $self->parentNode;
	my @rv;
	while (defined $x and Scalar::Util::blessed $x and $x->isa('XML::LibXML::Element'))
	{
		push @rv, $x;
		$x = $x->parentNode;
	}
	return wantarray ? @rv : HTML::HTML5::DOM::HTMLCollection->new(@rv);
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_ancestors',
	'Returns a (Perl or XML::LibXML::NodeList) list of this element\'s ancestors - i.e. the parentNode, the parentNode of the parentNode, etc.',
	);

sub wm_contains
{
	my ($self, $thing) = @_;
	my @results = grep {
		$_ == $self
	} $thing->ancestors;
	return 1 if @results;
	return;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_contains',
	'Given an argument, returns true if that argument is an element nested within this element.',
	);

__PACKAGE__->_mk_attribute_accessors(qw/
	id
	title lang translate==boolean dir className=class
	hidden==boolean tabIndex=tabindex accessKey=accesskey
	classList=class=LIST
	/);

sub dataset
{
	my $self = shift;
	my %rv;
	foreach my $attr ($self->attributes)
	{
		if ($attr->nodeName =~ /^data-[^A-Z]$/)
		{
			my $key = $1;
			$key =~ s{ \- ([a-z]) }{ uc('-'.$1) }gex;
			$rv{$key} = $attr->value;
		}
	}
	return \%rv;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'dataset',
	'Gets a hashref based on C<< data-foo >> attributes.',
	);

package HTML::HTML5::DOM::HTMLUnknownElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/*/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLAnchorElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/a/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/
	href==URI target rel rev media hreflang target type
	relList=rel=LIST revList=rev=LIST text==TEXT
	/);
__PACKAGE__->_mk_url_decomposition;
__PACKAGE__->_mk_follow_method;

package HTML::HTML5::DOM::HTMLAreaElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/area/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/
	alt coords shape href==URI target rel rev media hreflang type
	relList=rel=LIST revList=rev=LIST
	/);
__PACKAGE__->_mk_url_decomposition;
__PACKAGE__->_mk_follow_method;

package HTML::HTML5::DOM::HTMLAudioElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/audio/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLMediaElement'];

package HTML::HTML5::DOM::HTMLMediaElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw//;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/
	src==URI crossOrigin=crossorigin preload controls
	/);
__PACKAGE__->_mk_follow_method('src');

package HTML::HTML5::DOM::HTMLBaseElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/base/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/href==URI target/);
__PACKAGE__->_mk_url_decomposition;

package HTML::HTML5::DOM::HTMLQuoteElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/blockquote q/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/cite==URI/);

package HTML::HTML5::DOM::HTMLBodyElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/body/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLBRElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/br/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLButtonElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/button/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/autofocus disabled name type value/);
__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods;

package HTML::HTML5::DOM::HTMLCanvasElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/canvas/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/height width/);


package HTML::HTML5::DOM::HTMLTableCaptionElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/caption/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLTableColElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/col colgroup/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/span/);

package HTML::HTML5::DOM::HTMLCommandElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/command/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/
	type label icon==URI disabled==boolean checked==boolean radiogroup
	/);

package HTML::HTML5::DOM::HTMLDataListElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/datalist/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

sub options
{
	my ($self) = @_;
	return $self->getElementsByTagName('option');
}

package HTML::HTML5::DOM::HTMLModElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/del ins/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/cite==URI dateTime=datetime=datetime/);

package HTML::HTML5::DOM::HTMLDetailsElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/details/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/open==boolean/);

package HTML::HTML5::DOM::HTMLDivElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/div/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLDListElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/dl/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLEmbedElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/embed/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/src==URI type height width/);

package HTML::HTML5::DOM::HTMLFieldSetElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/fieldset/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/disabled==boolean name/);
__PACKAGE__->_mk_form_methods([qw/form/]);

sub type
{
	return 'fieldset';
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'type',
	'Returns the string "fieldset". Kinda useless, but it is part of the HTML5 DOM.',
	);

sub elements
{
	die "TODO";
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'elements',
	'@@TODO - should return a list of C<< <input> >>, C<< <select> >>, etc elements nested inside this fieldset.',
	);

package HTML::HTML5::DOM::HTMLFormElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/form/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/
	acceptCharset=accept-charset action==URI autocomplete enctype
	encoding method name noValidate=novalidate target
	/);

sub _get_elements
{
	my ($self, $allowed) = @_;
	my $return = $self
		-> ownerDocument
		-> getElementsByTagName('*')
		-> grep(sub {
				($_->nodeName ~~ $allowed)
				&& ($_->form == $self)
			})
		-> map(sub { XML::LibXML::Augment->rebless($_) });
	wantarray ?
		$return->get_nodelist :
		bless($return, 'HTML::HTML5::DOM::HTMLFormControlsCollection');
}

sub elements
{	
	my $self = shift;
	@_ = ($self, [qw/button fieldset input keygen object output select textarea/]);
	goto \&_get_elements;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'elements',
	'Returns a list of form-related elements which this form owns. In list context this is a normal Perl list. In scalar context it is a HTML::HTML5::DOM::HTMLFormControlsCollection.',
	);

sub wm_submittableElements
{
	my $self = shift;
	@_ = ($self, [qw/button input keygen object select textarea/]);
	goto \&_get_elements;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_submittableElements',
	'Returns a list of form-related elements which this form owns that can potentially cause name=value pairs to be added to the form submission. (e.g. not C<< <fieldset> >>.) In list context this is a normal Perl list. In scalar context it is a HTML::HTML5::DOM::HTMLFormControlsCollection.',
	);

sub length
{
	my $self = shift;
	return $self->elements->size;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'length',
	'The length of the C<elements> list.',
	);

sub submit
{
	my ($self, $hashref) = @_;

	my $method = (uc $self->method || 'GET');
	my $fields = $self->wm_submittableElements->wm_wwwFormUrlencoded($hashref);

	if ($method eq 'GET')
	{
		return Web::Magic->new($self->action.'?'.$fields)
	}

	return Web::Magic
		-> new($self->action)
		-> set_request_method($method)
		-> Content_Type($self->enctype || 'application/x-www-form-urlencoded')
		-> set_request_body($fields);
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'submit',
	'Submits the form based on the current values of its submittable elements. May be passed an optional hashref of name=>value pairs to override those values, but this is not always enough to do what you want, as HTML allows for multiple form elements of the same name to exist in a form.',
	);

package HTML::HTML5::DOM::HTMLFormControlsCollection;

use parent -norequire => qw/HTML::HTML5::DOM::HTMLCollection/;
use URI::Escape qw//;

sub namedItem
{
	my ($self, $name) = @_;
	my @items = $self->grep(sub {
		($_->hasAttribute('id') && $_->getAttribute('id') eq $name) ||
		($_->hasAttribute('name') && $_->getAttribute('name') eq $name)
	});
	return $items[0] if scalar @items == 1;
	return wantarray ? @items : HTML::HTML5::DOM::RadioNodeList->new(@items);
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'namedItem',
	'Given a name, returns a list of nodes of elements where the @id or @name attribute matches that name. In scalar context this can return a single element if there\'s only one match, or a HTML::HTML5::DOM::RadioNodeList if there is more than one - this is a kinda annoying feature, but it is required for DOM compliance. Best to just call it in list context.',
	);

sub wm_wwwFormUrlencoded
{
	my ($self, $hashref) = @_;
	my @pairs = $self->wm_wwwFormPairs($hashref);
	#use Data::Dumper; print Dumper \@pairs; exit;
	return
		join '&',
		map {
			sprintf('%s=%s', map { URI::Escape::uri_escape($_) } @$_)
		}
		@pairs;
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_wwwFormUrlencoded',
	'Returns a form-encoded (C<< foo=bar&quux=xyzzy >>) string for the elements on the list.',
	);

sub wm_wwwFormPairs
{
	my ($self, $hashref) = @_;
	my %remaining = my %HH = %{ $hashref || +{} };
	my $return = $self->map(sub {
		my @empty;
		return @empty unless $_->can('wm_wwwFormPair');
		my $pair = $_->wm_wwwFormPair;
		return @empty unless $pair;
		if (exists $hashref->{$pair->[0]})
		{
			delete $remaining{ $pair->[0] };
			$pair->[1] = $hashref->{$pair->[0]};
		}
		return $pair;
	});
	while (my @pair = each %remaining)
	{
		$return->push(\@pair);
	}
	wantarray ? @$return : (bless $return, 'XML::LibXML::NodeList');
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_wwwFormPairs',
	'Returns a list of C<< [$name => $value] >> tuples for the elements on the list.',
	);

package HTML::HTML5::DOM::RadioNodeList;

use parent qw/XML::LibXML::NodeList/;

sub value
{
	my ($self) = @_;
	my @items = $self->grep(sub { $_->hasAttribute('checked') });
	return unless @items;
	return unless $items[0]->hasAttribute('value');
	return $items[0]->getAttribute('value');
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_wwwFormPairs',
	'Returns the "value" attribute of the first element which has a "checked" attribute.',
	);

package HTML::HTML5::DOM::HTMLHeadElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/head/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

sub profile
{
	my ($self) = @_;
	return
		map { URI->new($_) }
		grep { length $_ }
		split /\s+/,
		$self->getAttribute('profile');
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_wwwFormPairs',
	'Splits the "profile" attribute on whitespace, and returns it as a list of L<URI> objects.',
	);

package HTML::HTML5::DOM::HTMLHeadingElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/h1 h2 h3 h4 h5 h6/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLHRElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/hr/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLHtmlElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/html/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/version/);

package HTML::HTML5::DOM::HTMLIFrameElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/iframe/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/src==URI srcdoc name sandbox seamless==boolean width height/
	);


package HTML::HTML5::DOM::HTMLImageElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/img/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/alt src==URI crossOrigin=crossorigin useMap=usemap isMap=ismap width height/
	);
__PACKAGE__->_mk_follow_method('src');

package HTML::HTML5::DOM::HTMLInputElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/input/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/accept alt max min multiple==boolean pattern placeholder
	required==boolean size src==URI step autocomplete type
	dirName=dirname autofocus==boolean checked==boolean width
	maxLength=maxlength height name readOnly=readonly=boolean/
	);
__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods;

*indeterminate =
*list =
*valueAsDate =
*valueAsNumber =
*stepUp =
*stepDown =
	sub { die 'TODO' };

sub wm_wwwFormPair
{
	my ($self) = @_;
	if ($self->getAttribute('type') =~ m{^ (checkbox|radio) $}ix)
	{
		return unless $self->hasAttribute('checked');
	}
	elsif ($self->getAttribute('type') =~ m{^ (submit|reset|button|image) $}ix)
	{
		return;
	}
	
	return [ $self->getAttribute('name'), $self->getAttribute('value') ];
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'wm_wwwFormPair',
	'Returns the C<< [$name => $value] >> that would be used when submitting this form element.',
	);

package HTML::HTML5::DOM::HTMLKeygenElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/keygen/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods([qw/form/]);

package HTML::HTML5::DOM::HTMLLabelElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/label/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_form_methods([qw/form/]);

sub control
{
	my ($self) = @_;
	
	my @controls;
	if ($self->hasAttribute('for'))
	{
		my $xpath = sprintf('//[@id="%s"]', $self->getAttribute('for'));
		@controls = $self->ownerDocument->findnodes($xpath);
	}
	else
	{
		@controls = grep { $_->can('labels') } $self->getElementsByTagName('*');
	}
	
	return $controls[0] if @controls;
	return;	
}

HTML::HTML5::DOMutil::AutoDoc->add(
	__PACKAGE__,
	'control',
	'Returns the control that this element acts as a label for.',
	);

package HTML::HTML5::DOM::HTMLLegendElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/legend/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_form_methods([qw/form/]);

package HTML::HTML5::DOM::HTMLLIElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/li/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

sub value
{
	die "TODO";
}

package HTML::HTML5::DOM::HTMLLinkElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/link/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/
	disabled==boolean href==URI rel rev media hreflang target type
	relList=rel=LIST revList=rev=LIST
	/);
__PACKAGE__->_mk_url_decomposition; # technically not part of HTML5 spec
__PACKAGE__->_mk_follow_method;

package HTML::HTML5::DOM::HTMLMapElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/map/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLMenuElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/menu/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/label type/);

package HTML::HTML5::DOM::HTMLMetaElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/meta/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/name httpEquiv=http-equiv content scheme/);

package HTML::HTML5::DOM::HTMLMeterElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/meter/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/value min max low high optimum/);
__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods([qw/form/]);

package HTML::HTML5::DOM::HTMLObjectElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/object/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/data==URI type typeMustMatch=typemustmatch name useMap=usemap width height/
	);
__PACKAGE__->_mk_form_methods([qw/form/]);
__PACKAGE__->_mk_follow_method('data');

package HTML::HTML5::DOM::HTMLOListElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/ol/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/reversed==boolean start type/);

package HTML::HTML5::DOM::HTMLOptGroupElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/optgroup/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/disabled==boolean label/);

package HTML::HTML5::DOM::HTMLOptionElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/option/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLOutputElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/output/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods([qw/form/]);

package HTML::HTML5::DOM::HTMLParagraphElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/p/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLParamElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/param/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/name value/);

package HTML::HTML5::DOM::HTMLPreElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/pre/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLProgressElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/progress/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/max position value/);
__PACKAGE__->_mk_labels_method;

package HTML::HTML5::DOM::HTMLScriptElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/script/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/src==URI async==boolean defer==boolean type charset text==TEXT/
	);

package HTML::HTML5::DOM::HTMLSelectElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/select/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods([qw/form/]);

package HTML::HTML5::DOM::HTMLSourceElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/source/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/src==URI type media/
	);
__PACKAGE__->_mk_follow_method('src');

package HTML::HTML5::DOM::HTMLSpanElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/span/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLStyleElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/style/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/disabled==boolean scoped==boolean type media/
	);


package HTML::HTML5::DOM::HTMLTableElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/table/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLTableSectionElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/tbody tfoot thead/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLTableCellElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw//;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLTableDataCellElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/td/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLTableCellElement'];

package HTML::HTML5::DOM::HTMLTableHeaderCellElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/th/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLTableCellElement'];

package HTML::HTML5::DOM::HTMLTableRowElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/tr/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLTextAreaElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/textarea/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_labels_method;
__PACKAGE__->_mk_form_methods([qw/form/]);

package HTML::HTML5::DOM::HTMLTimeElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/time/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

sub datetime
{
	die "TODO";
}

package HTML::HTML5::DOM::HTMLTitleElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/title/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

__PACKAGE__->_mk_attribute_accessors(qw/text==TEXT/);

package HTML::HTML5::DOM::HTMLTrackElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/track/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLUListElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/ul/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLElement'];

package HTML::HTML5::DOM::HTMLVideoElement;

use 5.010;
use common::sense;
use mro 'c3';

our @ELEMENTS;
BEGIN {
	@ELEMENTS = map { sprintf('{%s}%s', HTML::HTML5::DOM->XHTML_NS, $_) }
		qw/video/;
}

use XML::LibXML::Augment 0
	-names => [@ELEMENTS],
	-isa   => ['HTML::HTML5::DOM::HTMLMediaElement'];

__PACKAGE__->_mk_attribute_accessors(
	qw/poster==URI height width/
	);

1;
