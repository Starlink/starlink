#
# $Id$
# verbatimfiles.perl
#   Jens Lippmann <lippmann@cdc.informatik.th-darmstadt.de> 6-FEB-96
#
# Extension to LaTeX2HTML to support verbatim.sty/verbatimfiles.sty.
#
# Change Log:
# ===========
#  jcl = Jens Lippmann
#
# $Log$
# Revision 1.1  2004/02/20 13:13:28  nxg
# Initial import
#
# Revision 1.1  1998/08/20 16:03:47  pdraper
# *** empty log message ***
#
# Revision 1.2  1996/12/23 01:36:50  JCL
# o added some informative comments and log history
# o uses now shell variable TEXINPUTS (set up before by LaTeX2HTML)
#   to locate input files
# o verbatimlisting is now numbered according to the LaTeX output I
#   got here: empty lines also numbered, but not the first if empty.
#
# Revision 1.1 1996/12/18 04:31:29  JCL
# was formerly verbatim.perl, now renamed to this file
#
# JCL -- 6-FEB-96 -- created
#
#
# Note:
# This module provides translation for the \verbatimfile and
# \verbatimlisting commands of the verbatimfiles.sty package.
#
# The naming of verbatim.sty is a bit blurred.
# Here are the versions which are available, together with their
# identification:
#  o dbtex verbatim.sty by Rowley/Clark
#    Provides:
#    - \verbatimfile, \verbatimlisting
#    It is also named verbatimfiles.sty, and supported by
#    this Perl module.
# 
#  o verbatim.sty 1.4a (jtex), 1.4d (ogfuda), 1.4i (AMS LaTeX),
#    1.5i (LaTeX2e) by Sch"opf
#    Provides:
#    - verbatim environment, comment environment, \verbatiminput
#    Supported by verbatim.perl.
# 
#  o FWEB verbatim.sty
#    Provides:
#    - verbatim environment, \verbfile, \listing, \sublisting
#    Currently not supported by LaTeX2HTML.


package main;

sub do_cmd_verbatimfile {
    local($outer) = @_;
    local($_);

    $outer =~ s/$next_pair_pr_rx//o;
    local($file) = $2;
    $file .= ".tex" unless $file =~ /\.tex$/;

    foreach $dir ("$texfilepath", split(/:/,$ENV{'TEXINPUTS'})) { 
	if (-f ($_ = "$dir/$file")) {
	    #overread $_ with file contents
	    &slurp_input($_);
	    last;
	}
    }
    # pre_process file contents
    &replace_html_special_chars;

    $verbatim{++$global{'verbatim_counter'}} = $_;
    join('',"<BR>\n",$verbatim_mark,'verbatim',$global{'verbatim_counter'},$outer);
}

sub do_cmd_verbatimlisting {
    local($outer) = @_;
    local($_);
    local($counter) = 0;

    $outer =~ s/$next_pair_pr_rx//o;
    local($file) = $2;
    $file .= ".tex" unless $file =~ /\.tex$/;

    foreach $dir ("$texfilepath", split(/:/,$ENV{'TEXINPUTS'})) { 
	if (-f ($_ = "$dir/$file")) {
	    #overread $_ with file contents
	    &slurp_input($_);
	    last;
	}
    }
    # pre_process file contents
    &replace_html_special_chars;

    #insert numbers for every line
    #but not the first line if it's empty (LaTeX'ism?)
    s/^([ \t]+\n)//;
    local($first) = $1;
    #and not the last end of line
    s/\n$//;
    s/(^|\n)/$1.sprintf("%4d ",++$counter)/ge;

    #add the stuff from the first(if empty) and last line also
    $verbatim{++$global{'verbatim_counter'}} = $first.$_."\n";
    join('',"<BR>\n",$verbatim_mark,'verbatim',$global{'verbatim_counter'},$outer);
}

1; 		# Must be last line
