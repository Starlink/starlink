# longtable.perl by Denis Koelewijn
#
# Extension to LaTeX2HTML supply support for the "longtable"
# LaTeX style, as described in "The LaTeX Companion," by
# Goossens, Mittelbach and Samarin (ISBN 0-201-54199-8). 
#
# Change Log:
# ===========

package main;
#
#  Translate the longtable environment as
#  an ordinary table.
#
#

sub do_env_longtable {
    local($_) = @_;
    local($border,$this,$cols);
    s/$next_pair_rx/$cols=$&;''/eo;
    local($cap_env, $captions) = ('table','');
    do { local($contents) = $_;
	&extract_captions($cap_env); $_ = $contents;
    } if (/\\caption/m);
    &do_env_tabular($cols.$_)
}

#    while (/\\end((first)?head|(last)?foot)/ ) {
#	$_ = $';
#	$this = $`;
#	if ($this =~ /(\\[hv]line)\b/) { $border = $1 }
#    }
#    $contents = join('', $cols, (($border)? "\n" : ''), $contents);
#    $contents = &process_environment("tabular", $global{'max_id'}++, $contents);
#}


&ignore_commands( <<_IGNORED_CMDS_);
LTleft
LTright
LTpre
LTpost
LTcapwidth
LTchunksize
setlongtables
_IGNORED_CMDS_

&process_commands_in_tex (<<_RAW_ARG_CMDS_);
_RAW_ARG_CMDS_

1;                              # This must be the last line
