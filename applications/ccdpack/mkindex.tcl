#+
#  Name:
#     mkindex.tcl

#  Purpose:
#     Make the tclIndex file.

#  Invocation:
#     ccdwish mkindex.tcl

#  Type of Module:
#     Tcl script.

#  Description:
#     This is a script which can be sourced within the ccdwish shell to
#     generate the tclIndex file.  It just runs the standard Tcl
#     auto_mkindex proc on the relevant files in the current directory.
#     These files are pretty much all the files with .tcl, .itcl or .itk
#     extensions, with the exception of those which are themselves
#     designed to be sourced rather than simply defining procs.
# 
#     Note that it must be invoked within the directory for which the
#     tclIndex file is to be built.  It must also be invoked with a
#     valid setting of the DISPLAY environment variable (since otherwise
#     ccdwish cannot start up properly).

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     27-JUN-2001 (MBT):
#        Original version.
#-

#  Local constants:
      set exclude_files {
         CCDMain.tcl 
         CCDGeometryMain.tcl
         CCDBindings.tcl
         CCDOptions.tcl
         mkindex.tcl 
         ccdalign.tcl
         idicurs.tcl
         pairndf.tcl
         raisehack.tcl
         ccdpack_red.tcl
         ccdpack_reg.tcl
         ccdpack_res.tcl
         ccdpack_scr.tcl
      }

#  Withdraw the top level window for tidiness.  A secondary purpose of 
#  this is that if wish graphical initialisation fails (which will
#  happen if $DISPLAY is not set properly, and which would cause the
#  index file to be written in a deficient way), this call will fail
#  and cause the script to bomb out in a visible way.
      wm withdraw .

#  Initialise the glob pattern list.
      set patterns {}

#  Generate a list of the .tcl files to use; all of them apart from 
#  named exceptions.
      foreach file [glob *.tcl *.tk *.itcl *.itk] {
         if { [lsearch -exact $exclude_files $file] == -1 } {
            lappend patterns $file
         }
      }

#  Invoke the command which generates the index.
      eval auto_mkindex . $patterns

#  Exit the interpreter.
      exit
# $Id$
