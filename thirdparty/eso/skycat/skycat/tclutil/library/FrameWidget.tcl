# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: FrameWidget.tcl,v 1.1.1.1 2006/01/12 16:40:51 abrighto Exp $"
#
# FrameWidget.tcl - Itk base class for widgets with their own frame
#
# who             when       what
# --------------  --------   ----------------------------------------
# Allan Brighton  01 Jun 94  Created
#
# Peter W. Draper 20 Jan 98  Fixed busy focus -lastfor so
#                            that it does restore the focus,
#                            not just get the window name. 
#                 23 Jan 08  Make focus control is busy method optional.

# The FrameWidget itcl class is a subclass of itk::Widget and thus
# inherits all of the features described in Widget(n).  In addition, a
# number of useful methods are defined, for use by the derived classes.

itcl::class util::FrameWidget {
    inherit itk::Widget

    # Create a frame with the same name as this object

    constructor {args} {
	set class_ [utilNamespaceTail [$this info class]]
	set w_ $itk_component(hull)

	eval itk_initialize $args

	after idle [code $this init]
    }


    # derived classes can re-define the "init" method to run code after all 
    # options have been evaluated
    
    protected method init {} {
    }


    # run the given tcl command while displaying the busy cursor
    # in the frame's parent top level window, if defocus is false don't handle
    #  focussing. 


    public method busy {cmd {defocus 1}} {
	global ::errorInfo ::errorCode

	if {[incr busy_count_] == 1} {
            if { $defocus } {
   	       catch {focus .}
            }
	    blt::busy hold $w_
	    update idletasks
	}

	# save any errors and report them later
	if {[set code [catch [list uplevel $cmd] msg]]} {
	    set info $errorInfo
	} 

	if {[incr busy_count_ -1] == 0} {
	    blt::busy release $w_
            if { $defocus } {
                catch {focus [focus -lastfor $w_]}
	    }
        } 

	if {$code} {
	    uplevel [list error $msg $info $code]
	}
    }

    # set the text of short help message to be displayed whenever
    # the mouse enters the widget w (assumes you are using class
    # ToplevelWidget)

    public method add_short_help {w text} {
	if {[catch {[winfo toplevel $w_] add_short_help $w $text} msg]} {
	    # puts "$msg"
	}
    } 


    # set the text of the short help message (display now, assumes you 
    # are using class ToplevelWidget)

    public method short_help {text} {
	#catch {[winfo command [winfo toplevel $w_]] short_help $text}
	catch {[winfo toplevel $w_] short_help $text}
    } 

    
    # -- options --

    # -- class variables --

    # name of this (derived) class
    protected variable class_

    # shorter name for $itk_component(hull)
    protected variable w_

    # count used for busy cursor
    private variable busy_count_ {0}
}

