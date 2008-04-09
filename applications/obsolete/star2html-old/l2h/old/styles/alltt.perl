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
    while (/\\begin\s*{alltt}/) {
	$alltt = "";
	($before, $after) = ($`, $');
	if ($after =~ /\\end\s*{alltt}/) {
	    ($alltt, $after) = ($`, $');
	    $alltt = &alltt_helper($alltt)	 # shield special chars
		unless ($before =~ /\n.*%.*$/);  # unless commented out
	    }
	$_ = join('', $before, $alltt_begin, $alltt, $alltt_end, $after);
	}
    s/$alltt_begin/\\begin{alltt}/go;
    s/$alltt_end/\\end{alltt}/go;
    };

sub alltt_helper {
    local ($_) = @_;
    s/^/\\relax/;	# Preserve leading & trailing white space
    s/\t//g;		# Remove tabs
    s/\$/;SPMdollar;/g;
    s/\%/;SPMpct;/g;
    s/~/;SPMtilde;/g;
    join('', $_, "\\relax");
    }

sub do_env_alltt {
    local ($_) = @_;
    $_ = &revert_to_raw_tex($_);
    &mark_string;
    s/\\([{}])/$1/g;
    "<PRE>$_</PRE>";
    }

1;	# Must be last line
