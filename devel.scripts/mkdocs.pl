#!/usr/bin/perl

use HTML::HTML5::DOM;

my @packages;
{
	open my $module, '<', 'lib/HTML/HTML5/DOM.pm';
	while (defined (my $line = <$module>))
	{
		if ($line =~ m{ ^ \s* package \s* ( HTML::HTML5::DOM:: ([\S]+) ) \s* ; }x)
		{
			push @packages, [$1, $2];
		}
	}
}

my @sorted = sort { $a->[1] cmp $b->[1] } @packages;
foreach my $p (@sorted)
{
	my ($fullname, $shortname) = @$p;
	HTML::HTML5::DOMutil::AutoDoc::psay "=item * L<$fullname>";
	open my $pod, '>', "lib/HTML/HTML5/DOM/${shortname}.pod";
	print $pod HTML::HTML5::DOMutil::AutoDoc->pod_for($fullname);
}

