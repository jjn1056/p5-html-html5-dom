use lib "lib";

package XML::LibXML::Element;
use Scalar::Util qw/blessed/;
use overload 'eq' => '_isSameNodeLax', '==' => '_isSameNodeLax';
sub _isSameNodeLax
{
	my ($self, $other) = @_;
	if (UNIVERSAL::can($other, 'isa')
	and $other->isa('XML::LibXML::Element'))
	{
		 return $self->isSameNode($other);
	}
	return;
}

package main;
use 5.010;
use Web::Magic;
use HTML::HTML5::DOM;
use HTML::HTML5::Parser;
use Data::Dumper;

my $dom = HTML::HTML5::Parser->load_html(IO => \*DATA);
XML::LibXML::Augment->rebless($dom);

say $dom->forms->[0]->elements->wm_wwwFormUrlencoded({
	q => 'Goodbye, cruel',
	x => 'lalala',
});
say $dom->links->[0]->host;
say $dom->getElementsByTagName('input')->[0]->labels->[0]->textContent;
say $dom->getElementsByTagName('input')->[1]->formMethod;


__DATA__

<title>Example</title>

<form action="http://www.google.com/search" method="get">
	<label>Search: <input name="q" value="hello"></label>
	<label>Mode: <input name="mode" value="world"></label>
	<input type="submit">
</form>

<a href="http://www.google.co.uk/next" rel=next>Next</a>
