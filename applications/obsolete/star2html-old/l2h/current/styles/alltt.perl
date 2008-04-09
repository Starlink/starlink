# alltt.perl by Herbert Swan <dprhws.edp.Arco.com>  12-22-95
#
# Extension to LaTeX2HTML V 96.1 to supply support for the
# "alltt" standard LaTeX2e package.
#
# Change Log:
# ===========

package main;

sub preprocess_alltt {
    local ($before, $after, $alltt);
    local ($alltt_begin) = "<alltt_begin>";
    local ($alltt_end) = "<alltt_end>";
    local($saveRS) = $/; $*=1;undef $/;
    while (/\\begin\s*{alltt}([ \t]*\n)?/) {
	$alltt = "";
	($before, $after) = ($`, $');
	if ($after =~ /\\end\s*{alltt}/s) {
	    ($alltt, $after) = ($`, $');
	    local(@check) = split("\n",$before);
	    local($lastline) = pop @check;
	    $alltt = &alltt_helper($alltt)	 # shield special chars
		unless ($lastline =~ /(^|[^\\])(\\\\)*%.*$/m);  # unless commented out
	    undef @check; undef $lastline;
	}
	$_ = join('', $before, $alltt_begin, $alltt, $alltt_end, $after);
    }
    $/ = $saveRS; $*=0;
    s/$alltt_begin/\\begin{alltt}/go;
    s/$alltt_end/\\end{alltt}/go;
};

sub alltt_helper {
    local ($_) = @_;
    local($br_id) = ++$global{'max_id'};
    s/^/\\relax$O$br_id$C$O$br_id$C /;	# Preserve leading & trailing white space
    s/\t/ /g;		# Remove tabs
    s/\$/;SPMdollar;/g;
    s/\%/;SPMpct;/g;
    s/~/;SPMtilde;/g;
    s/\n/\n<BR>/g;	# preserve end-of-lines --- cannot have <P>s
    join('', $_, "\\relax ");
}

sub do_env_alltt {
    local ($_) = @_;
    local($closures,$reopens,$alltt_start,$alltt_end);

    #check the nature of the last opened tag
    local($last_tag) = pop (@open_tags);
    local($decl) = $declarations{$last_tag};
    if ( $decl =~ m|</.*$|) { $decl = $& }
    if (($last_tag)&&!($decl =~ /$block_close_rx/)) {
	# need to close tags, for re-opening inside
	push (@open_tags, $last_tag) if ($last_tag);
	($closures,$reopens) = &preserve_open_tags();
	$alltt_start = "<DIV$env_id>";
	$alltt_end = "</DIV>";
	$env_id = '';
    } else {
	push (@open_tags, $last_tag) if ($last_tag);
    }

    # This allows paragraph/quote/DIV etc. tags to be preserved
    local(@open_tags,@save_open_tags) = ((),());

    local($cnt) = ++$global{'max_id'};
    $_ = join('',"$O$cnt$C\\tt$O", ++$global{'max_id'}, $C
		, $_ , $O, $global{'max_id'}, "$C$O$cnt$C");

    $_ = &translate_environments($_);
    $_ = &translate_commands($_) if (/\\/);

    # preserve space-runs, using &nbsp;
    while (s/(\S) ( +)/$1$2;SPMnbsp;/g){};
    s/(<BR>) /$1;SPMnbsp;/g;

#RRM: using <PRE> tags doesn't allow images, etc.
#    $_ = &revert_to_raw_tex($_);
#    &mark_string; # ???
#    s/\\([{}])/$1/g; # ???
#    s/<\/?\w+>//g; # no nested tags allowed
#    join('', $closures,"<PRE$env_id>$_</PRE>", $reopens);
#    s/<P>//g;
#    join('', $closures,"<PRE$env_id>", $_, &balance_tags(), '</PRE>', $reopens);

    $_ = join('', $closures, $alltt_start, $reopens, $_
	, &balance_tags(), $closures, $alltt_end, $reopens);
}

1;	# Must be last line





