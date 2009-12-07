#
# This script generates html files from a list of scripts for
# documentation purposes. It uses the standard 'document' perl
# script; see that for details.
#

require "document.pl";

(@ARGV == 1) or die "usage: genhtml.pl directory\n";
 
$html = shift;

@files = ('debias.pl', 'fixhead.pl', 'reduce.pl', 'rotate.pl', 'makebias.csh',
	  'fixrun.csh', 'whtdiv.csh', 'repet.pl', 'renumber.pl', 'loganal.pl', 
	  'ultradas.csh', 'loopdisplay.csh', 'listformat.csh', 'uniqformat.csh',
	  'splitbysource.pl', 'lsnosdf.csh');

# run my standard documentation subroutine

foreach $file (@files){
    document($file,$html,'html','html','#');
}

# generate index file

open(INDEX, ">$html/SCRIPTS.html") or die "Could not open $html/SCRIPTS.html\n";
print INDEX <<END1;
<html>
<title>pamela script index</title>
<body bgcolor="#ffffff">
<h1>pamela script index</h1>

<p>
This provides links to html documentation of various scripts associated
with pamela data reduction.
<p>
There are a number of operations that commonly are required during pamela
reduction. Several of these have to be run repetitively on every file.
Others are required by the specific data involved. This distribution cannot
account for all such cases, but comes with a number of scripts which may
be useful. These can be found in the subdirectory 'scripts' of the pamela
distribution. html-based documentation can be accessed below. Shift-click
on "script file" in each case to download the script. The file name lists
should be a series of name in a column without the .sdf ending. You can
do this with commands such as 
<ul>
'ls r*.sdf | sed s/\.sdf// >! flist'
</ul>

<p>
Note that it should be possible to run these scripts using aliases
defined when you invoke pamela. In each the alias is just the first
part of the script e.g. 'rotate' etc.

<p>
Available scripts:
<p>
<table>
END1

foreach $item (sort keys %index){
    print INDEX "<tr><td><a href=\"$index{$item}->{file}\">$item</a></td>".
	"<td>&nbsp;-&nbsp;</td>".
	    "<td>$index{$item}->{description}</td>".
		"<td>(<a href=\"$index{$item}->{source}\">script</a>)".
		    "</td></tr>\n";
}

# Finish up

print INDEX "</table><p><hr>\n";
$time = localtime;
if(defined $author){
    print INDEX "<address>Author: $author, page generated: $time</address>\n";
}else{
    print INDEX "<address>Page generated: $time</address>\n";
}
print INDEX "</body></html>\n";
close(INDEX);

exit;











