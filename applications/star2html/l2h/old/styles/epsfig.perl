# epsfig.perl by Michel Goossens <goossens@cern.ch>  01-14-96
#
# Extension to LaTeX2HTML V 96.1 to support epsfig.sty
# which is part of standard LaTeX2e graphics bunble.
#
# Change Log:
# ===========

package main;

# Get rid of color specifications, but keep contents

&ignore_commands( <<_IGNORED_CMDS_);
epsfverbosetrue
epsfverbosefalse
_IGNORED_CMDS_

&process_commands_in_tex (<<_RAW_ARG_CMDS_);
psfig # {}
epsfig # {}
epsfclipon
epsfclipoff
psdraft
psfull
pssilent
psnoisy
psfigdriver # {}
epsfbox  # [] # {}
epsffile # [] # {}
_RAW_ARG_CMDS_

1;	# Must be last line
