# graphics.perl by Herbert Swan <dprhws.edp.Arco.com>  12-22-95
#
# Extension to LaTeX2HTML V 96.1 to supply support for the "graphics"
# and "graphicx" standard LaTeX2e packages.
#
# Change Log:
# ===========

package main;


# Suppress option-warning messages:

sub do_graphics_dvips {
}
sub do_graphicx_dvips {
}



&process_commands_in_tex (<<_RAW_ARG_CMDS_);
rotatebox # [] # {} # {}
scalebox # {} # [] # {}
reflectbox # {}
resizebox # {} # {} # {}
resizeboxstar # {} # {} # {}
includegraphics # [] # {}
includegraphicsstar # [] # {}
_RAW_ARG_CMDS_

&ignore_commands( <<_IGNORED_CMDS_);
setkeys # {} # {}
_IGNORED_CMDS_

sub do_cmd_graphicspath {
    local($_) = @_;
    &do_cmd_special($_);
    }

1;	# Must be last line
