#-*-tcl-*-
# E.S.O. - VLT project/ESO Archive
# $Id: SkyCat_plugin.tcl,v 1.1.1.1 2006/01/12 16:42:12 abrighto Exp $
#
# SkyCat_plugin.tcl - example SkyCat plug-in script
#
# This is an example plugin file for skycat. It can be used to add
# features to skycat at startup. To use this file for skycat, set the
# environment variable SKYCAT_PLUGIN to the path name of this file.
# 
# This file must define at least one tcl proc: SkyCat_plugin, which
# takes one argument: the name of the top level skycat widget.  
#
# This plugin adds the ability to save and reload line graphics by
# adding 2 menu items to the Graphics menu.
#
# See the documentation, man pages and Itcl widget source code for details.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 Jan 98   created
#

puts "Loading the graphics features plugin..."

# This proc is required. It will be called once for each skycat
# instance. The parameter is the name ("$this") of the SkyCat
# class object.

proc SkyCat_plugin {this} {
    # get the toplevel widget name
    set w [utilNamespaceTail $this]

    # call a proc to add items to the Graphics menu.
    # Note that the directory containing this file is automatically
    # appended to the Tcl auto_path, so we can just call a proc
    # as long as there is a tclIndex file in this directory.
    add_graphics_features $w
}

