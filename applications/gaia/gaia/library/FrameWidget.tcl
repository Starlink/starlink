# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id$"
#
# FrameWidget.tcl - Itk base class for widgets with their own frame
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
# Peter W. Draper 20 Jan 98. Fixed busy focus -lastfor so that it does
#                            restore the focus, not just get the window name. 

# The FrameWidget itcl class is a subclass of itk::Widget and thus
# inherits all of the features described in Widget(n).  In addition, a
# number of useful methods are defined, for use by the derived classes.

class util::FrameWidget {
    inherit itk::Widget

    # Create a frame with the same name as this object

    constructor {args} {
	#itk_option add hull.borderwidth hull.relief

	set w_ $itk_component(hull)
	eval itk_initialize $args

	after idle [code $this init]
    }


    # derived classes can re-define the "init" method to run code after all 
    # options have been evaluated
    
    method init {} {
    }


    # run the given tcl command while displaying the busy cursor
    # in the frame's parent top level window

    public method busy {cmd} {
	global ::errorInfo ::errorCode

	if {[incr busy_count_] == 1} {
	    catch {focus .}
	    blt::busy hold $w_
	    update idletasks
	}

	# save any errors and report them later
	if {[set code [catch [list uplevel $cmd] msg]]} {
	    set info $errorInfo
	} 

	if {[incr busy_count_ -1] == 0} {
	    blt::busy release $w_
	    catch {focus [focus -lastfor $w_]}
	}

	if {$code} {
	    uplevel [list error $msg $info $code]
	}
    }

    # set the text of short help message to be displayed whenever
    # the mouse enters the widget w (assumes you are using class
    # ToplevelWidget)

    public method add_short_help {w msg} {
	catch {[winfo command [winfo toplevel $w_]] add_short_help $w $msg}
    } 


    # set the text of the short help message (display now, assumes you 
    # are using class ToplevelWidget)

    public method short_help {msg} {
	catch {[winfo command [winfo toplevel $w_]] short_help $msg}
    } 

    
    # -- options --
 
    # -- class variables --

    # shorter name for $itk_component(hull)
    protected variable w_

    # count used for busy cursor
    private variable busy_count_ {0}
}

