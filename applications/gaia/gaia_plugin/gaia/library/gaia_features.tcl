# E.S.O. - VLT project/ESO Archive
# $Id: gaia_features.tcl,v 1.1 1998/04/06 23:54:15 abrighto Exp $
#
# gaia_features.tcl - add the GAIA(Starlink) grid feature to Skycat.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 13 Mar 98   created
#


# Add a grid item to the SkyCat View menu.
# $w is the name of the top level SkyCat window.

proc add_gaia_features {w} {
    set m [$w add_menubutton Image-Analysis]

    $w add_menuitem $m command "Overlay axes grid..." \
	{Draw axes over image } \
	-command [code make_astgrid_toolbox $w] \
	-accelerator {Control-a}

    $w add_menuitem $m command "Patch image..." \
         {Realistically replace parts of image} \
         -command [code make_patch_toolbox $w]
}


#  Make an AST grid toolbox or make it visible.
# $w is the name of the top level SkyCat window.

proc make_astgrid_toolbox {w} {
    # get the handle of the canvas window for the image
    set canvas [$w component image component canvas]

    # get the handle for the rtdimage item
    set image [$w component image get_image]

    #  Get the clone number for this window.
    regsub {\.skycat} $w {} clone
    if {"$clone" == ""} {
	set clone 0
    }
    
    if {! [$image isclear]} {
	utilReUseWidget StarAstGrid $w.astgrid \
	    -canvas $canvas \
	    -rtdimage $image \
	    -number $clone
    }
}

#  Make a patch toolbox or make it visible.
# $w is the name of the top level SkyCat window.

proc make_patch_toolbox {w} {
    # get the handle of the canvas window for the image
    set canvas [$w component image component canvas]

    # get the handle for the rtdimage item
    set image [$w component image get_image]

    # get the handle for the CanvasDraw item
    set draw [$w component image component draw]

    #  Get the clone number for this window.
    regsub {\.skycat} $w {} clone
    if {"$clone" == ""} {
	set clone 0
    }
    
    if {! [$image isclear]} {
	utilReUseWidget StarPatch $w.patch \
	    -canvasdraw $draw \
	    -canvas $canvas \
	    -rtdimage $image \
	    -number $clone
    }
}

