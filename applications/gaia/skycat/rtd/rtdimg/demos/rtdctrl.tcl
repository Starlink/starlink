# -*-Tcl-*-
#
# E.S.O. - VLT project
#
# "@(#) $Id: rtdctrl.tcl,v 1.5 1998/10/28 17:41:16 abrighto Exp $" 
#
# Start the rtd image server as a standalone program
#
# Usage: rtdctrl
#
#  who      when     what
# -------- -------- ------------------
# abrighto 11/10/95 created 
#

lappend auto_path ../library
package require Rtd
setXdefaults
util::TopLevelWidget::start RtdServerTool
