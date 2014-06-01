use Test::More skip_all => "this seems to have stopped working in recent Test::Pod::Coverage??";
use Test::Pod::Coverage;
use HTML::HTML5::DOM;

my @modules = qw(
	HTML::HTML5::DOM
);
push @modules, map {
	my $mod = $_;
	$mod =~ s{^lib/}{};
	$mod =~ s{\.pod$}{};
	$mod =~ s{/}{::}g;
	$mod;
} qw {
	lib/HTML/HTML5/DOM/HTMLAnchorElement.pod
	lib/HTML/HTML5/DOM/HTMLAreaElement.pod
	lib/HTML/HTML5/DOM/HTMLAudioElement.pod
	lib/HTML/HTML5/DOM/HTMLBaseElement.pod
	lib/HTML/HTML5/DOM/HTMLBodyElement.pod
	lib/HTML/HTML5/DOM/HTMLBRElement.pod
	lib/HTML/HTML5/DOM/HTMLButtonElement.pod
	lib/HTML/HTML5/DOM/HTMLCanvasElement.pod
	lib/HTML/HTML5/DOM/HTMLCollection.pod
	lib/HTML/HTML5/DOM/HTMLCommandElement.pod
	lib/HTML/HTML5/DOM/HTMLDataListElement.pod
	lib/HTML/HTML5/DOM/HTMLDetailsElement.pod
	lib/HTML/HTML5/DOM/HTMLDivElement.pod
	lib/HTML/HTML5/DOM/HTMLDListElement.pod
	lib/HTML/HTML5/DOM/HTMLDocument.pod
	lib/HTML/HTML5/DOM/HTMLElement.pod
	lib/HTML/HTML5/DOM/HTMLEmbedElement.pod
	lib/HTML/HTML5/DOM/HTMLFieldSetElement.pod
	lib/HTML/HTML5/DOM/HTMLFormControlsCollection.pod
	lib/HTML/HTML5/DOM/HTMLFormElement.pod
	lib/HTML/HTML5/DOM/HTMLHeadElement.pod
	lib/HTML/HTML5/DOM/HTMLHeadingElement.pod
	lib/HTML/HTML5/DOM/HTMLHRElement.pod
	lib/HTML/HTML5/DOM/HTMLHtmlElement.pod
	lib/HTML/HTML5/DOM/HTMLIFrameElement.pod
	lib/HTML/HTML5/DOM/HTMLImageElement.pod
	lib/HTML/HTML5/DOM/HTMLInputElement.pod
	lib/HTML/HTML5/DOM/HTMLKeygenElement.pod
	lib/HTML/HTML5/DOM/HTMLLabelElement.pod
	lib/HTML/HTML5/DOM/HTMLLegendElement.pod
	lib/HTML/HTML5/DOM/HTMLLIElement.pod
	lib/HTML/HTML5/DOM/HTMLLinkElement.pod
	lib/HTML/HTML5/DOM/HTMLMapElement.pod
	lib/HTML/HTML5/DOM/HTMLMediaElement.pod
	lib/HTML/HTML5/DOM/HTMLMenuElement.pod
	lib/HTML/HTML5/DOM/HTMLMetaElement.pod
	lib/HTML/HTML5/DOM/HTMLMeterElement.pod
	lib/HTML/HTML5/DOM/HTMLModElement.pod
	lib/HTML/HTML5/DOM/HTMLObjectElement.pod
	lib/HTML/HTML5/DOM/HTMLOListElement.pod
	lib/HTML/HTML5/DOM/HTMLOptGroupElement.pod
	lib/HTML/HTML5/DOM/HTMLOptionElement.pod
	lib/HTML/HTML5/DOM/HTMLOutputElement.pod
	lib/HTML/HTML5/DOM/HTMLParagraphElement.pod
	lib/HTML/HTML5/DOM/HTMLParamElement.pod
	lib/HTML/HTML5/DOM/HTMLPreElement.pod
	lib/HTML/HTML5/DOM/HTMLProgressElement.pod
	lib/HTML/HTML5/DOM/HTMLQuoteElement.pod
	lib/HTML/HTML5/DOM/HTMLScriptElement.pod
	lib/HTML/HTML5/DOM/HTMLSelectElement.pod
	lib/HTML/HTML5/DOM/HTMLSourceElement.pod
	lib/HTML/HTML5/DOM/HTMLSpanElement.pod
	lib/HTML/HTML5/DOM/HTMLStyleElement.pod
	lib/HTML/HTML5/DOM/HTMLTableCaptionElement.pod
	lib/HTML/HTML5/DOM/HTMLTableCellElement.pod
	lib/HTML/HTML5/DOM/HTMLTableColElement.pod
	lib/HTML/HTML5/DOM/HTMLTableDataCellElement.pod
	lib/HTML/HTML5/DOM/HTMLTableElement.pod
	lib/HTML/HTML5/DOM/HTMLTableHeaderCellElement.pod
	lib/HTML/HTML5/DOM/HTMLTableRowElement.pod
	lib/HTML/HTML5/DOM/HTMLTableSectionElement.pod
	lib/HTML/HTML5/DOM/HTMLTextAreaElement.pod
	lib/HTML/HTML5/DOM/HTMLTimeElement.pod
	lib/HTML/HTML5/DOM/HTMLTitleElement.pod
	lib/HTML/HTML5/DOM/HTMLTrackElement.pod
	lib/HTML/HTML5/DOM/HTMLUListElement.pod
	lib/HTML/HTML5/DOM/HTMLUnknownElement.pod
	lib/HTML/HTML5/DOM/HTMLVideoElement.pod
	lib/HTML/HTML5/DOM/RadioNodeList.pod
};
pod_coverage_ok($_, "$_ is covered")
	foreach @modules;
done_testing(scalar @modules);

