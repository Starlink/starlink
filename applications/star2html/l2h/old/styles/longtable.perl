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
    &extract_captions;
    while ($contents =~ /\\end((first)?head|(last)?foot)/ ) {
	$contents = $';
	$this = $`;
	if ($this =~ /(\\[hv]line)\b/) { $border = $1 }
    }
    $contents = join('', $cols, (($border)? "\n" : ''), $contents);
    $contents = &process_environment("tabular", $global{'max_id'}++, $contents);
}

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
