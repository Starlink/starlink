#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

itk::usual LabelButton {}

# This widget displays a label and a button. A command may be associated
# with the button.

itcl::class gaia::LabelButton {
    inherit util::LabelWidget

    #  create a LabelButton
    constructor {args} {
	# The button
	itk_component add b {
	    button $w_.b
	} {
	    keep -relief -borderwidth -state -command -textvariable
	    rename -text -buttontext buttonText Text
	    rename -bitmap -buttonbitmap buttonbitmap Bitmap
	    rename -width -valuewidth valueWidth Width
	    rename -font -valuefont valueFont Font
	    rename -anchor -valueanchor valueAnchor ValueAnchor
	    ignore -disabledforeground
	}

	set default_bg_ [$itk_component(b) cget -background]
	eval itk_initialize $args
    }


    # options

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	pack $itk_component(b) \
	    -side $side_ -fill x -expand 1 -padx 1m -ipadx 1m
    }

    # -- protected variables --

    # default background color
    protected variable default_bg_

}


