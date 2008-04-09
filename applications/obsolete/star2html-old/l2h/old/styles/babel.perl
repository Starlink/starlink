# babel.perl
#
# written by Marek Rouchal <marek@saftsack.fs.uni-bayreuth.de>
# Last modification: Sun Oct 27 20:34:56 MET 1996
#
# This file is in alpha stage development
# currently it serves only as a wrapper for german.perl

package main;

# for debugging only
# print "Using babel.perl\n";

# implement usable options from LaTeX

sub do_babel_german {
    local($file,$dir)=("german.perl",'');
    print "\nbabel.perl: Loading german.perl\n" if $DEBUG;
    foreach $dir (split(/:/,$LATEX2HTMLSTYLES)) {
	if (-f "$dir/$file") {
           require("$dir/$file");
	}
    }
}

# cancel redundant options from LaTeX

# none so far

1;	# Must be last line
