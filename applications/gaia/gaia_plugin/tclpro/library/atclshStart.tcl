# E.S.O. - VLT project 
# "@(#) $Id: atclshStart.tcl,v 1.1 1999/03/17 20:49:16 abrighto Exp $"
#
# atclshInit.tcl - startup script for prowrapped utility shell
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  16 Mar 99  Created

set argv0 [lindex $argv 0]
set argv [lrange $argv 1 end]
incr argc -1
source $argv0
