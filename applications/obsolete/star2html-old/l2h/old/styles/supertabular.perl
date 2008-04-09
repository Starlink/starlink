# supertabular.perl by Denis Koelewijn
#
# Extension to LaTeX2HTML supply support for the "supertabular"
# LaTeX style, as described in "The LaTeX Companion," by
# Goossens, Mittelbach and Samarin (ISBN 0-201-54199-8). 
#
# Change Log:
# ===========

package main;
#
#  Translate the supertabular environment as
#  an ordinary table.
#
#

sub do_env_supertabular {
    local($_) = @_;

#    $contents =~ s/$next_pair_rx//o;
    &process_environment("tabular", $global{'max_id'}++);
    }

&ignore_commands( <<_IGNORED_CMDS_);
_IGNORED_CMDS_

&process_commands_in_tex (<<_RAW_ARG_CMDS_);
_RAW_ARG_CMDS_

1;                              # This must be the last line
