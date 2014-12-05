#!/usr/bin/env perl

use strict;
use warnings;

# module to let you read whole file into one string
use File::Slurp;
use Term::ANSIColor;

# Read in filename and output name from command line.
my $filename = shift;
my $outname = shift;

# Open this file.
open (my $DOCFILE, $filename) or die "error opening $filename\n";

# Open temp file to write to
open (my $OUTPUTFILE, ">", $outname);

# Go through each line.
while (my $line=<$DOCFILE>)
  {
    # Change document class.
    if ($line =~/(.*?)documentclass(.*?)\{article\}(.*?)/)
      {
	$line = "$1documentclass$2\{starlink\}$3\n";
      }
    # Change star doc commands to new version.
    #if ($line=~/(.*?)newcommand\{\\stardoc(.*?)\}(.*)\{(.*?)\}/)
    #  {
#	$line = "$1\stardoc$2$3\{$4\}\n";
#      }

    if ($line=~/(.*?)\\begin{document}(.*?)/ )
      {
	$line = "$1\\begin{document}\n\\scfrontmatter\n$2";
      }


    print $OUTPUTFILE "$line";
  }

close $DOCFILE;
close $OUTPUTFILE;


#write out file
my $text = read_file($outname);
$text =~s/\\newcommand{\\stardoc(.*?)}(.*?){(.*?)}/\\stardoc$1 $2 {$3}/g;

$text =~s/\\newcommand{\\stardocabstract}/\\stardocabstract/g;
$text =~s/\\newcommand{\\stardoccopyright}/\\stardoccopyright/g;
# Replace \small\verbatim with terminalv
$text =~s/\\latex{\\small}\n\\begin{verbatim}/\\begin{terminalv}/g;
$text =~s/\\end{verbatim}\n\\latex{\\normalsize}/\\end{terminalv}/g;
# Replace \myquote\verbatim with terminalv
$text =~s/\\begin{myquote}\n\\begin{verbatim}/\\begin{terminalv}/g;
$text =~s/\\end{verbatim}\n\\end{myquote}/\\end{terminalv}/g;

# Replace verbatim with terminalv enivornment
$text =~s/\\begin{verbatim}/\\begin{terminalv}/g;
$text =~s/\\end{verbatim}/\\end{terminalv}/g;

# Replace \hyperref with \slhyperref
$text =~ s/\\hyperref{/\\slhyperref{/g;

# Remove pagestyle commands and markboth/mark right etc commands.
$text =~s/\\pagestyle{(.*?)}//g;
$text =~s/\\thispagestyle{(.*?)}//g;
$text =~s/\\renewcommand{\\thepage}{(.*?)}(.*?)\n//g;
$text =~s/\\markboth{(.*)}{(.*)}//g;
$text =~s/\\markleft{(.*)}//g;
$text =~s/\\markright{(.*)}//g;

# Remove html setstyle commands
$text =~s/\\htmlsetstyle(.*?)\n//g;

# Remove latexonlytoc
$text =~s/\\latexonlytoc//g;

# Remove setcounter
$text =~ s/\\setcounter{(.*)}{(.*)}//g;


# Remove table of contents
$text =~ s/\\tableofcontents//g;



$text =~ s/\\newlength{\\menuwidth}\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\\renewcommand{\\baselinestretch}(.*)}\n//g;

# Remove the refs environment?
$text =~ s/\\newenvironment{refs}(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\n(.*)\\end{list}}//g;

$text =~ s/\\newcommand{\\classitem}\[1\]{(.*)}//g;


# comment out any set length commands
$text =~s/\\setlength{(.*?)}{(.*?)}/\%\\setlength{$1}{$2}/g;

# most new commands are already provided, so convert to provide commands just to get it built...
$text=~s/\\newcommand/\\providecommand/g;

# Replace any blank lines with whitespace then closing or opening brace with just closing whitespace then closing brace
$text =~ s/\n(\s*?)\n(\s*?)}{(\s*?)\n/\n$2}{\n/g;

#$text =~ s/(^$)(\s*?)}/$2}/g;


# comment out use package lines (with options;
$text =~ s/(.*?)\\usepackage(.*?){(.*?)}(.*?)\n/\%$1\\usepackage$2{$3}$4\n/g;

# Remove old-style font modifiers: \em \sc \it \bf etc
my %stylemap = ( em => "emph",
                 sc => "textsc",
                 it => "textit",
                 tt => "texttt",
                 bf => "textbf" );
for my $old (keys %stylemap) {
  $text =~ s/\{\\$old\s+/\\$stylemap{$old}\{/g;
}

# Write out file again
write_file($outname, $text);

#
# read in file line by line, to remove certain sections
open (my $INFILE, $outname) or die "error opening $outname\n";

# Open output file to write to
open ($OUTPUTFILE, ">", "$outname.temp");


# Remove hyptertext definitions
my $echo = 1;
while (my $line=<$INFILE>)
{
  if ($line=~/\%  Hypertext definitions(.*)/)
    {
      print "Removing the following hypertext definitions:\n";
      $echo = 0;
    }
  if ($echo == 1)
      {
	print $OUTPUTFILE "$line";
      }
    else
      {
	print color("red"),"$line",color("reset");
      }
    if (($echo == 0) and ($line=~/\% ------------------------------------------(.*)/))
      {
	print "Turning echo back on \n\n";
	$echo = 1;
      }
  }


# close file handles.
close $INFILE;
close $OUTPUTFILE;


rename("$outname.temp","$outname") or die "Rename failed: $!";




# Remove latex title
open ($INFILE, "$outname") or die "error opening $outname\n";
open ($OUTPUTFILE, ">", "$outname.temp");
$echo = 1;
while(my $line=<$INFILE>)
  {
    if ($line=~/\%  Latex document header(.*)/)
      {
	print "Removing the following Latex title page :\n";
	$echo = 0;
      }
    if ($echo == 1)
      {
	print $OUTPUTFILE "$line";
      }
    else
      {
	print color("red"),"$line",color("reset");
      }
    if ($line=~/\%  HTML documentation header(.*?)/ )
      {
	print "End of title page hopefully: Turning echo back on \n\n";
	$echo = 1;
      }
  }
close $INFILE;
close $OUTPUTFILE;
rename ("$outname.temp", "$outname") or die "Rename failed: $!";


# Remove sst definitions.
open ($INFILE, "$outname") or die "error opening $outname\n";
open ($OUTPUTFILE, ">", "$outname.temp");
$echo = 1;
while(my $line=<$INFILE>)
  {
    if ($line=~/\%(\s*)SST.TEX(.*?)/)
      {
	print "Removing the following SST definitions:\n";
	$echo = 0;
      }
    if ($echo == 1)
      {
	print $OUTPUTFILE "$line";
      }
    else
      {
	print color("red"),"$line",color("reset");
      }
    if ($line=~/\%(\s*)End of "sst.tex/ )
      {
	print "Turning echo back on \n\n";
	$echo = 1;
      }
  }
close $INFILE;
close $OUTPUTFILE;
rename ("$outname.temp", "$outname") or die "Rename failed: $!";


# Remove htmlonly sections
open ($INFILE, "$outname") or die "error opening $outname\n";
open ($OUTPUTFILE, ">", "$outname.temp");
$echo = 1;
while(my $line=<$INFILE>)
  {
    if ($line=~/(.*?)\\begin{htmlonly}(.*?)/)
      {
	print "Removing htmlonly text:\n";
	print $OUTPUTFILE "$1\n";
	$echo = 0;
      }
    if ($echo == 1)
      {
	print $OUTPUTFILE "$line";
      }
    else
      {
	print color("red"),"$line",color("reset");
      }
    if ($line=~/(.*?)\\end{htmlonly}(.*?)/)
      {
	print "Turning echo back on\n\n";
	print $OUTPUTFILE "$2\n";
	$echo = 1;
      }
  }

close $INFILE;
close $OUTPUTFILE;
rename ("$outname.temp", "$outname") or die "Rename failed: $!";
# Remove menu item and classitem

#while($line=<FILE>)
#  {
#    if ($line=~/(.*?)\begin{


# Now need to gothrough whole file to remove:
# usepackage commands
# 1.Hypertext definitions
# 2. Debugging
# 3. menuitem
# 4. classitem
# 5. refs environment

# Keep! for now! hypertext links?

# SST definitions - remove

# title page -- remove all

# pagestyle commands -- remove all

# replace \begin{document} with \begin{document}\n\scfrontmatter

# remove table of contents (latexonlytoc)
# 



# replace \latex{\small}\n\begin{verbatim} with \begin{verbatim} and
# sim for end

# replace \begin{verbatim} ... \end{verbatim} with \begin{terminalv}...\end{terminalv}

# Move abstract to start

# summaries of commands: change begin/end description not to be html only?

# htmonly -- check manually. Some should now be incorporated in botht hte latex and the html, some should just be removed (e.g. somre are repeats of information in a latexonly box, some are information that was never deemed to work in a hardcopy format).


# commands that are now dealt with by style sheet and should usually be removed:
# markboth, clearpage, newpage

# Replace \hyperref with \slhyperref

# use of \html with \verb inside seems to break stuff. Problem in kappa.

# Blank lines with sst short description or example subsection break things...
# look out for use of \verb -- screws up line breaks.

# Similarly the use \htmlref...}~ -- replace }~ with }



# Needed for KAPA either before afterwards:
# move abstract into \stardocabstract.
# being/end description around menuitems.
# create section Release notes and move everything else down one level (replace section{ with subsection{ for rest of document


# Changes for sun258:
# remove space in abstract
# description around menuitem
# comment out/remove stuff at top
