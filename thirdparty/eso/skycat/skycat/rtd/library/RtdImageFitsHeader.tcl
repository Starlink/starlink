# E.S.O. - VLT project 
# "@(#) $Id: RtdImageFitsHeader.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdImageFitsHeader.tcl - Itcl widget for displaying FITS header HDU's
# 
# See man page RtdImageFitsHeader(n) for a complete description.
# 
# who         when       what
# ----------  ---------  -----------------------------------------------------
# pbiereic    14/08/01   Created
# pbiereic    17/02/03   fixed problems with tabnotebook, packing order
#                        and labels of tabsets
# pdraper     05/04/06   Treat HISTORY like COMMENT
# pdraper     20/02/07   Formatting changes: keep empty lines and print
#                        blank COMMENT/HISTORY lines with blanks, do
#                        not fake that they start with COMMENT/HISTORY.

itk::usual RtdImageFitsHeader {}

# RtdImageFitsHeader is an itcl widget for displaying the FITS header of all
# Header Data Units (HDU's) contained in a FITS file. The user can view the
# FITS header in different ways, print it, search for keyword etc.
# RtdImageFitsHeader uses public methods of class util::TableList

itcl::class rtd::RtdImageFitsHeader {
    inherit util::TopLevelWidget

    constructor {args} {
	eval itk_initialize $args
        wm protocol $w_ WM_DELETE_WINDOW [code $this quit]
    }


    # This method is called after the options have been evaluated.

    protected method init {} {
	package require Iwidgets

	# set default table configuration parameters
	set tconfig_(sort_cols) ""
	set tconfig_(sort_order) ""
	set tconfig_(Keyword) 1
	set tconfig_(Value) 1
	set tconfig_(Comment) 1

	add_menubar
	make_notebook $w_
	add_buttons $w_

	# tabnotebook's configure event doesn't work (oszillates)
	bind [$tabnotebook_(w) component canvas]  <Configure> {}

	# pack the button frame first to ensure that it
	# doesn't get obscured when the window is resized (shrinked).

	pack $itk_component(buttons) -side bottom -fill both -expand 0
	pack $tabnotebook_(w) -fill both -expand 1 -side top
    }
    
    # Quit this widget

    public method quit { } {
        set activated_ 0
	destroy $w_
    }

    # add the menubar at the top of the window

    protected method add_menubar {} {
	TopLevelWidget::add_menubar

	# add menubuttons and menus

	set m [add_menubutton File]

	add_menuitem $m command "Print..." \
		{Print all FITS tables} \
		-command [code $this print]

	add_menuitem $m command "Close" \
		{Close this window} \
		-command [code $this quit]

	set m [add_menubutton Header]

	add_menuitem $m cascade "Sort..." \
		{Sort FITS header} \
		-menu [menu $m.sort]

	global $w_.sort
	set $w_.sort 2
	$m.sort add radiobutton -label "Keyword - increasing" \
		-command [code $this sort Keyword increasing] \
		-variable $w_.sort -value 0
	$m.sort add radiobutton -label "Keyword - decreasing" \
		-command [code $this sort Keyword decreasing] \
		-variable $w_.sort -value 1
	$m.sort add radiobutton -label "No sort" \
		-command [code $this sort "" ""] \
		-variable $w_.sort -value 2

	add_menuitem $m cascade "Show / Hide" \
		{Configure layout of table} \
		-menu [menu $m.show]

	global $w_.show_keyword $w_.show_value $w_.show_comment

	set $w_.show_keyword 1
	$m.show add checkbutton -label "Keyword" \
		-command [code $this show Keyword $w_.show_keyword] \
		-variable $w_.show_keyword -onvalue 1 -offvalue 0

	set $w_.show_value 1
 	$m.show add checkbutton -label "Value" \
		-command [code $this show Value $w_.show_value] \
		-variable $w_.show_value -onvalue 1 -offvalue 0

	set $w_.show_comment 1
 	$m.show add checkbutton -label "Comment" \
		-command [code $this show Comment $w_.show_comment] \
		-variable $w_.show_comment -onvalue 1 -offvalue 0
   }

    # add the buttons

    protected method add_buttons { f } {
	itk_component add buttons {
	    frame $f.buttons
	}
	itk_component add close {
	    button $itk_component(buttons).close \
		    -text Close -width 8 \
		    -command [code $this quit]
	}
	
	itk_component add search {
	    LabelEntry $itk_component(buttons).search \
		    -text Search -valuewidth 10 \
		    -command [code $this search]
	}
	pack $itk_component(search) -side left -fill x -expand 1
	pack $itk_component(close)  -side left -fill none -expand 0
    }

    # Make the layout

    protected method make_notebook { f } {
	set tabnotebook_(w) [iwidgets::tabnotebook $f.tab \
				 -tabpos n -width 400 -height 500 \
				 -equaltabs false -raiseselect 1 \
				 -font $itk_option(-labelfont) \
				 -backdrop [$w_ cget -background]]
	add_short_help $f.tab \
		{Drag {bitmap b2} = Drag mouse button 2 to scroll}
    }

    # Sort FITS header

    protected method sort { sort_cols sort_order } {
	set tconfig_(sort_cols) $sort_cols
	set tconfig_(sort_order) $sort_order

	loop i 0 $num_hdus_ {
            $tabnotebook_(table$i) config -sort_cols $sort_cols \
		    -sort_order $sort_order
	    $tabnotebook_(table$i) new_info
	}
    }

    # Show / Hide table column

    protected method show { label var } {
	global $var
	set val [set $var]
	set tconfig_($label) $val
	loop i 0 $num_hdus_ {
	    $tabnotebook_(table$i) set_option $label Show $val
	    $tabnotebook_(table$i) new_info
	}
    }

    # Print current selected header

    protected method print { } {
	set idx [$tabnotebook_(w) index select]
	set tbl $tabnotebook_(table$idx)
	busy {
            set w $w_.tblprint
            if {[winfo exists $w]} {
		$w config -table $tbl
                wm deiconify $w
            } else {
                TableListPrint $w -table $tbl -printcmd [$tbl cget -printcmd]
            }
        }
    }

    # Search and highlight pattern

    protected method search { string } {
	$itk_component(search) select

	set idx [$tabnotebook_(w) index select]
	set tbl $tabnotebook_(table$idx)
	set listbox [$tbl component listbox]

	set string [string tolower $string]

	set start_idx $search_idx_
	set search_idx_ 0
	if { "$string" != "$search_str_" } {
	    set start_idx 0
	    set search_str_ $string
	}

	set end_idx [$listbox index end]
	if { $start_idx >= $end_idx } {
	    set start_idx 0
	}

	set length [string length [$listbox get 0]]

        loop n $start_idx $end_idx {
	    set row [string tolower [$listbox get $n]]
            if {[catch {regexp -indices $string $row indices} idx] } { continue }
	    if { $idx > 0 } {
		$tbl select_row $n
		lassign $indices i1 i2
		set i1 [expr {double($i1) + ($i2 - $i1) / 2.}]

		lassign [$listbox bbox $n] x0 y0 w h
		set offs [expr {double($length) / $w * [winfo width $w_] / 2.0}]
		set i1 [expr {$i1 - $offs}]
		$listbox xview moveto [expr {(1.0  - (double($length - $i1) / $length))}]
		set search_idx_ [incr n]
		break
	    }
        }
    }
    
    # Set title

    protected method set_title { } {
        set s [$image_ object]
        if {"$s" == ""} {
            set s [$image_ cget -file]
        }
        if {"$s" == ""} {
            set s "FITS Header"
        } else {
            set s "FITS Header for $s"
        }
	wm title $w_ $s
	wm iconname $w_ $s
	return $s
    }
    
    # Activate this widget

    public method activate { } {
	set activated_ 1
	set title [set_title]
	set w $tabnotebook_(w)
	set hdu_count [$image_ hdu count]

	if { $hdu_count > 0 && "[$image_ cget -file]" == "$file_" } {
	    show_hdu_header [$image_ hdu]
	    return
	}

	set file_ [$image_ cget -file]

	if { $num_hdus_ > 0 } {
	    $w delete 0 [expr {$num_hdus_ - 1}]
	}
	set num_hdus_ $hdu_count

	set hlist [$image_ hdu list]

	set ExtName "HDU 1"
	loop i 0 $hdu_count {
	    set hdu [expr {$i + 1}]
	    set list [lindex $hlist $i]
	    
	    set tabnotebook_($i) [$w add -label $ExtName -gap overlap \
				      -command [code $this show_hdu_header [expr {$i + 1}]]]
	    set ExtName " [expr {$hdu + 1}] "

	    set child [$w childsite $i]
	    set tabnotebook_(table$i) $child.tab$i

	    # use TableList(n) widget for displaying the FITS header
	    pack [util::TableList $child.tab$i \
		     -title $title \
		     -hscroll 1 -vscroll 1 \
		     -headings "Keyword Value Comment" \
		     -sort_cols $tconfig_(sort_cols) \
		     -sort_order $tconfig_(sort_order)] \
		     -fill both -expand 1

	    $tabnotebook_(table$i) set_option Keyword Show $tconfig_(Keyword)
	    $tabnotebook_(table$i) set_option Value Show $tconfig_(Value)
	    $tabnotebook_(table$i) set_option Comment Show $tconfig_(Comment)
	}
	show_hdu_header [$image_ hdu]
    }

    # Show HDU header info

    public method show_hdu_header { hdu } {
	set search_idx_ 0
	if { [$image_ hdu count] < 1 || $hdu < 1 } { return }
	catch {$tabnotebook_(w) select [expr {$hdu -1}]}

	set idx [expr {$hdu - 1}]
	set w $tabnotebook_(table$idx)

	set fits [$image_ hdu fits $hdu]
	# TableList needs formatting...
	foreach line [split $fits "\n"] {
	    set l [string trim $line]
	    if {"$l" == "END"} {
		lappend info [list END {} {}]
		break
	    }
	    if { [lempty $l] } {
               lappend info [list {} {} {}]
               continue
            }
	    set triple [get_kvc $line]
	    if { [lempty $triple] } {
		set triple [list INVALID {} $line]
	    }
	    lappend info $triple
	}
	if { [info exists info] && ! [lempty $info] } {
	    $w config -info $info
	}
	[$w component listbox] xview moveto 0.0
    }

    # return a tcl list with keyword, value and comment (kvc)

    protected method get_kvc { line } {
	set key [string range $line 0 6]
        if { [lempty $key] || "$key" == "COMMENT" || "$key" == "HISTORY" } {
	    return [list $key {} [string trim [string range $line 7 end]]]
	}
	lassign [split $line =] l1 l2
        if { [lempty $l1] } { return "" }
	set key [string trim $l1]
	lassign [split $l2 /] l1 l2 l3
        if { [info exists l3] && $l3 !={} } {
           #  value = 'name/name' /comment?
           set l1 "$l1/$l2"
           set l2 "$l3"
        }
	set val [string trim $l1]
	set com [string trim $l2]
	return [list $key $val $com]
    }
	
    # -- options  --
    
    # target (main) RtdImage itcl widget
    itk_option define -image image Image {} {
	set image_ [$itk_option(-image) get_image]
    }
    
    # Font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal--10*
    
    # -- protected vars --
    
    # internal rtdimage object
    protected variable image_
    
    # array for tabnotebook
    protected variable tabnotebook_
    
    # array for table configuration
    protected variable tconfig_
     
    # last file
    protected variable file_ ""
     
    # widget activated, bool
    protected variable activated_ 0
    
    # number of HDU of last file
    protected variable num_hdus_ 0
    
    # last serach index
    protected variable search_idx_ 0
    
    # last serach string
    protected variable search_str_ ""
}
