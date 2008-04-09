#
# $Id$
# xspace.perl
#   Jens Lippmann <lippmann@cdc.informatik.th-darmstadt.de> 26-JAN-97
#
# Extension to LaTeX2HTML to support xspace.sty.
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
# Revision 1.1  1997/01/27 19:40:44  JCL
# initial revision
#
#
# JCL -- 26-JAN-97 -- created
#


package main;

sub do_cmd_xspace {
    local($_) = @_;
    local($space) = " ";
    # the list is taken from xspace.sty 1.04
    $space = "" if /^([{}~.!,:;?\/'\)-]|\\\/|\\ )/;
    $space.$_;
}

1; 		# Must be last line
