set CCDdir $env(CCDPACK_DIR)

#  Define the autoload path for TCL procedures.
lappend auto_path $CCDdir

#  Set global bindings.
source $CCDdir/CCDBindings.tcl

#  Global options (colours, fonts, reliefs etc.). Also reads ~/.ccdpack
#  at an appropriate point.
source $CCDdir/CCDOptions.tcl

#-----------------------------------------------------------------------------
#  Widget creation.
#-----------------------------------------------------------------------------

#  Create top-level widget for main window.

global CCDimportfile
global CCDimportexists
global CCDimportfilter
global CCDcurrentdirectory

set CCDimportfilter {{NDF "*.sdf"} {IRAF "*.imh"} {FITS "*.fit"}}

CCDGetFileName .file "New get filename"
