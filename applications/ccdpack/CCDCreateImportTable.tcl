   proc CCDCreateImportTable { Top table } {
#+
#  Name:
#     CCDCreateImportTable.

#  Purpose:
#     Presents a form which contains widgets for creating or modifying
#     a FITS import control table.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This window allows the creation and/or modification of a FITS
#     import control table. An import control table is required to
#     generate extension information for CCDPACK from FITS items in the
#     FITS block of an NDF.
#
#     FITS information (probably provided by the instrument/telescope
#     control systems) can be used to specify certain parameters which
#     are required by CCDPACK to perform "automated" reductions. These
#     might cover such items as the type of data (target, flatfield,
#     bias frame etc.), the Analogue-to-Digital Conversion factor, the
#     nominal readout noise, the position of any bias strips (over-scan
#     regions) etc.
#
#     The import control table specifies how FITS keyword values should
#     be interpreted. This allows the evaluation of functions
#     containing many FITS keywords as well as the mapping of CCDPACK
#     recognised character items to arbitrary strings.
#
#     Using this method to create a table allows the information in the
#     FITS block of the NDFs to be quieried. The relevant items may now
#     be selected and typed for extraction from the FITS block.
#
#     The "known" extension items are displayed and can be selected. The
#     hard part of writing the FITS function (see the "Table Format"
#     section) still has to be performed, although modification of
#     existing tables is supported (under the menu "Options"). The final
#     product can be saved into a disk-file for future use.
#
#     Before exit is possible a filename must be given for the import
#     control table. Unless a file which has been read in is unmodified
#     is which case this will be used. The filename is returned in the
#     CCDimportfile global variable. The existence of such a file is
#     indicated by the value of the global boolean CCDimportexits.

#  Arguments:
#     Top = window (read)
#        The name of the top-level object for this form.
#     table = filename (read)
#        The name of an import table to read into form. None read if
#        this is {}.

#  Table Format:
#     The import control (translation) table is an ordinary text file
#     which contains instructions on how to transfer FITS information
#     from the FITS extension to the CCDPACK extension of an NDF.
#     "Translation" is required since no standard interpretation of
#     FITS keywords can be made and because the items which may be
#     required can be compounds of single FITS keyword values.
#
#     In its most simple format a FITS control table is just a series of
#     lines which contain the names of CCDPACK extension items and the
#     names of the FITS keywords to which they map.
#
#        Extension-item     FITS-keyword
#
#     If the HDS type of the destination Extension-item is known this
#     may be included.
#
#        Extension-item     _HDS-type     FITS-keyword
#
#     Normally this is inferred. This is the format used by the KAPPA
#     application FITSIMP (as of 20/12/93). Extension items are the
#     names of CCDPACK items (such as FRAME_TYPE, FILTER etc.). These
#     may be heirarchical, e.g. TIMES.DARK. Note that they exclude the
#     "NDF_NAME.MORE.CCDPACK." part of the extension path name.
#
#     To allow functions of FITS-keywords to be possible a second
#     "declarative" form of statement is necessary.
#
#        _HDS-type          FITS-keyword
#
#     So for instance if you wanted to derive the exposure time of a
#     observation which was given in milliseconds and which you wanted
#     to convert into seconds you would use this sequence of commands
#
#        _INTEGER          EXPOSURE
#        TIMES.EXPOSURE   _DOUBLE    1000.0D0*EXPOSURE
#
#     The "_INTEGER EXPOSURE" tells this application to find a FITS
#     keyword of EXPOSURE and extract its value as an integer.  If you
#     wanted to estimate the dark time as from a knowledge of the start
#     and end times (UT0 and UT1)
#
#        _DOUBLE        UTO
#        _DOUBLE        UT1
#        TIMES.DARK    _DOUBLE       (UT1-UT0)
#
#     The function may use any of the usual Fortran operators; +, -, *,
#     /, ** and the functions allowed by the TRANSFORM package (SUN/61).
#
#     Functions are allowed to not contain any FITS-keywords in which
#     case the extension item will be assigned to the value, so for
#     instance constants may be given.
#
#        EXTENT.MINX   _INTEGER       1
#        EXTENT.MINY   _INTEGER       1
#        FILTER        _CHAR          NONE         ! Spectroscopic
#
#     In this way import tables could actually be used to set all the
#     required values in the CCDPACK extension (but see PRESENT also).
#
#     Characters strings cannot be manipulated by functions so a single
#     special format for translating their values is provided. The name
#     of the destination extension item and (optionally) its type are
#     given as usual followed by a FITS-keyword which contains the
#     string to be translated. This is then followed by statements which
#     translate a "input" string into an "output" string. I.e.
#
#        FITS1 = Ext1 FITS2 = Ext2 FITS3 = Ext3 ... FITSn = Extn
#
#     So for instance if you wanted to translate frame types to those
#     recognised by CCDPACK you might use something like.
#
#        FTYPE    _CHAR   OBSTYPE  OBJECT=TARGET -
#                                  FF=FLAT -
#                                  ZERO=BIAS
#
#     Which would compare the value of the FITS-keyword OBSTYPE with
#     the strings "OBJECT", "FLAT" and "ZERO" (case insensitive) and
#     convert these into the values in the right-hand side of the equals
#     sign.
#
#     Logical data types are restricted to a single keyword whose value
#     must be "YES", "TRUE", "T", "Y" for TRUE or "NO", "FALSE", "N",
#     "F".
#
#     Fields in the table may be separated by commas if desired, any
#     amount of white space and tabs are also allowed. Comments may be
#     placed anywhere and should start with the characters "#" or "!".
#     Continuation onto a new line is indicated by use of "-".

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995-1996, 2000-2001 Central Laboratory of the
#     Research Councils. Copyright (C) 2006 Particle Physics &
#     Astronomy Research Council. All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     30-MAR-1994 (PDRAPER):
#        Original version.
#     11-MAY-1995 (PDRAPER):
#        Updated to Tk4.0.
#     19-MAY-1995 (PDRAPER):
#        Removed reference to internal method of Ccd::multiitem..
#     22-AUG-1995 (PDRAPER):
#        Converted to use new coding style and Table widgets.
#     2-FEB-1996 (PDRAPER):
#        Added fixed for -modified being passed to Fitsfunc instead
#        of Fitstable when import table read in.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Modified the arguments of CCDGetFileName.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g). Remove
#        confusing/unused CCDndfimportfilter and switch to correct
#        CCDimportfilter.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters:
      global ict_modified
      global CCDimportfile
      global CCDimportexists
      global CCDimportavail
      global CCDlaterunique

#  Local parameters:
      set Knownnames "FTYPE FILTER ADC BOUNDS.START1 \
                      BOUNDS.END1 BOUNDS.START2 BOUNDS.END2 \
                      TIMES.DARK DEFERRED DIRECTION EXTENT.MINX \
                      EXTENT.MAXX EXTENT.MINY EXTENT.MAXY \
                      TIMES.FLASH RNOISE SATURATION ZERO"
#.

#  Initialise the table as modified. The only time this changes is when
#  a table is read or saved, or entries are deleted or inserted into the
#  windows.
      set ict_modified 0
      set CCDimportavail 0

#--------------------------------------------------------------------------
#  Widget creation.
#--------------------------------------------------------------------------
#  Top-level window for this form.
      CCDCcdWidget Topwin topwin \
         Ccd::toplevel $Top -title "Create Import Table"
      wm withdraw $topwin

#  Menubar.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Topwin.menubar

#  Main frame for containing FITS item stuff.
      CCDTkWidget Frametop frametop frame $topwin.fits

#  Left part.
      CCDTkWidget Lefttop lefttop frame $frametop.left

#  Labelled entry for reference NDF name.
      CCDCcdWidget Refndf refndf \
         Ccd::labent $Lefttop.refndf -text "Reference frame:" -placelabel left

#  Create scrollbox for names extracted from NDF FITS structure. This
#  has a button above it which filters the output from a list of the
#  .more.fits structure of an NDF and enters the names of the fits
#  items found therein. Make sure that only a single selection is
#  possible in listbox.
      CCDCcdWidget Fitsbox fitsbox \
         Ccd::scrollbox $Lefttop.fitsbox -singleselect true
      CCDCcdWidget Fitsextract fitsextract \
         Ccd::choice $Lefttop.extract -standard 0

#  Right frame for fits extension items (and types).
      CCDTkWidget Righttop righttop frame $frametop.right

#  Labelled entry for fits ITEM name.
      CCDCcdWidget Fitsname fitsname \
         Ccd::labent $Righttop.name -text "FITS item:"

#  Option widget with a pop-up menu in the entry window and a
#  menubutton which will show the possible options for the HDS-type.
      CCDCcdWidget Fitstype fitstype \
         Ccd::option $Righttop.type -text "HDS type:" -constrain 1

#  Choice buttons for adding the HDS item and type, removing the current
#  selection and sorting/making unique (by item name).
      CCDCcdWidget Fitschoice fitschoice \
         Ccd::choice $Righttop.choice -standard 0

#  Table for showing the selected and typed FITS items.
      CCDCcdWidget Fitstable fitstable \
         Ccd::table $Righttop.table -columns 2 -flushright 0

#  Frame for containing the CCDPACK extension item and FITS functions
#  region.
      CCDTkWidget Framebot framebot frame $topwin.exten

#  Frame for list of all known CCDPACK extension items. Make listbox
#  have only one active selection at a time.
      CCDCcdWidget Knownbox knownbox \
         Ccd::scrollbox $Framebot.known -singleselect true \
                                        -label "Known extension items:"

#  Frame for right side of lower box. This contains the selected
#  extension item and the function (of FITS items) to use when
#  deriving a value.
      CCDTkWidget Rightbot rightbot frame $framebot.right

#  Labelled entry box for the extension item name.
      CCDCcdWidget Extname extname \
         Ccd::labent $Rightbot.name -text "Extension item:"

#  Labelled entry box for the FITS function.
      CCDCcdWidget Fitsfunc fitsfunc \
         Ccd::labent $Rightbot.func -text "FITS function:"

#  Choice bar buttons for adding the extension item, type and function,
#  for removing the current selection and for sorting/making unique.
      CCDCcdWidget Extchoice extchoice \
         Ccd::choice $Rightbot.choice -standard 0

#  Table for the extension transformations.
      CCDCcdWidget Transtable transtable \
         Ccd::table $Rightbot.table -columns 2 -flushright 0

#  Choice widget for OK and Cancel buttons.
      CCDCcdWidget Choice choice Ccd::choice $Topwin.bot

#--------------------------------------------------------------------------
#  Widget configuration.
#--------------------------------------------------------------------------
#  Menubar.
#  Add command to close and exit the window (equivalent to OK, Cancel
#  and exit interface).
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add command to menu which selects the reference NDF using a
#  filename form. This also invokes the application to extract and
#  parse the FITS items.
      $Menu addcommand Options {Select reference frame...} \
         "global CCDimportexists
          global CCDimportfile
	  global CCDimportfilter
          global CCDimagefilters
          set CCDimportfilter \$CCDimagefilters
          if \[info exists CCDimportfile\] {
             set importfile \$CCDimportfile
          } else {
             set importfile {}
          }
          CCDGetFileName $Topwin.refndf \"Select reference frame\" 1
          if { \$CCDimportexists } {
             set rndf \[CCDFileToNDFName \"\$CCDimportfile\" \]
             $Refndf clear 0 end
             $Refndf insert 0 \$rndf
             $Fitsextract invoke {Extract FITS items}
          }
          set CCDimportfile \$importfile
	 "

#  Add command to menu which reads an import table file entering the
#  values into the appropriate listboxes.
      $Menu addcommand Options {Read import table file...} \
         "global CCDimportexists
          global CCDimportfile
          global CCDimportavail
          global CCDimportfilter
          global ict_modified
          set CCDimportfilter \"*.DAT\"
          CCDGetFileName $Topwin.restore {Read import table file} 0
          if { \$CCDimportexists } {
             CCDRestoreFromImportFile \$CCDimportfile $Fitstable $Transtable
             set ict_modified 0
             set CCDimportavail 1
             $Fitstable configure -modified 0
          }
	 "

#  Add command to menu which saves the current setup to a file.
      $Menu addcommand Options {Save import table in file...} \
         "global CCDimportavail
          global CCDimportfile
          global ict_modified
          global CCDimportfilter
          set CCDimportfilter \"*.DAT\"
          CCDNewFileName $Topwin.savefile \"Save import table in file\"
          if { \$CCDimportavail } {
             set ok \[CCDSaveImportTable \
                         \"\$CCDimportfile\" \
                         $Fitstable \
                         $Transtable\]
             if { \$ok } {
                set ict_modified 0
                $Fitstable configure -modified 0
                $Transtable configure -modified 0
             } else {
                set CCDimportfile {}
                set CCDimportexists {}
             }
          }
	 "

#  Add switch for controlling the sense of the uniqueness. That is
#  whether to keep the earlier or later version.
      if { ! [info exists CCDlaterunique] } { set CCDlaterunique 1 }
      $Menu addcheckbutton Options {Keep later unique values} \
         -variable CCDlaterunique

#  Refndf.
#  Add a binding to <Return> to invoke the procedure for filling the
#  listbox with the names of the possible FITS items.
      $Refndf bind entry <Return> "$Fitsextract invoke {Extract FITS items}"

#  Add a button to extract the FITS extension information
      $Fitsextract addbutton {Extract FITS items} \
         "CCDExtractFitsFromNDF $Top $Refndf $Fitsbox"

#  Fitsbox.
#  Add a binding to a button press in the FITS item listbox to enter
#  this as the current FITS item. Set up drag of button one to follow
#  the selection.
      $Fitsbox  bind list <Button-1> \
         "$Fitsname clear 0 end
	  set index \[ %W nearest %y \]
          $Fitsname insert 0 \[ %W get \$index \]
          %W select anchor \$index
         "
      $Fitsbox bind list <B1-Motion> \
         "$Fitsname clear 0 end
	  set index \[ %W nearest %y \]
          $Fitsname insert 0 \[ %W get \$index \]
          %W select anchor \$index
         "

#  Fitstype.
#  Add the range of possible HDS types.
      $Fitstype addoption _INTEGER
      $Fitstype addoption _REAL
      $Fitstype addoption _DOUBLE
      $Fitstype addoption _CHAR
      $Fitstype addoption _LOGICAL

#  Add a binding to <Return> to invoke the add button.
      $Fitstype bind entry <Return> "$Fitschoice invoke Add"

#  Fitschoice.
#  Make the Add button read the fits item name and its type and append
#  them to the table.
      $Fitschoice addbutton {Add} \
         "global ict_modified
	  set item \[ $Fitsname get \]
	  set type \[ $Fitstype get \]
	  if { \"\$type\" != \"_CHAR\" &&
               \"\$type\" != \"_INTEGER\" &&
               \"\$type\" != \"_LOGICAL\" &&
               \"\$type\" != \"_REAL\" &&
               \"\$type\" != \"_DOUBLE\" } {
               CCDIssueInfo \"Unknown data type: \$type\"
          } else {

#  Insert the values into the listbox.
             if { \"\$type\" != \"\" && \"\$item\" != \"\" } {
                $Fitstable insert end \$type \$item
                $Fitstable see end
                set ict_modified 1
             }
	  }
	 "

#  Add a button to delete the current selections.
      $Fitschoice addbutton {Remove} \
         "global ict_modified
          CCDRemoveFromList $Fitstable clear
          set ict_modified 1
         "

#  Add a button to sort and remove any duplicate entries (by extension name).
      $Fitschoice addbutton {Unique} "CCDMakeUnique $Fitstable 2 1"

#  Fitstable.
#  Label the columns.
      $Fitstable setlabel 0 {HDS type}
      $Fitstable setlabel 1 {FITS item}

#  Knownbox.
#  Enter all the names of the known extension items.
      foreach item "$Knownnames" { $Knownbox insert end $item }

#  Add a binding for button press to enter nearest item enter as the current
#  item. Motion just inserts the nearest item as well.
      $Knownbox bind list <Any-Button-1> \
         "$Extname clear 0 end
	  set index \[ %W nearest %y \]
          $Extname insert 0 \[ %W get \$index \]
          %W select anchor \$index
         "
      $Knownbox bind list <B1-Motion> \
         "$Extname clear 0 end
	  set index \[ %W nearest %y \]
          $Extname insert 0 \[ %W get \$index \]
          %W select anchor \$index
         "

#  Fitsfunc.
#  Add a binding to  <Return> in to enter the values into the table.
      $Fitsfunc bind entry <Return> "$Extchoice invoke Add"

#  Extchoice.
#  Add a button to append the current entry value and the function to the
#  table.
      $Extchoice addbutton {Add} \
         "global ict_modified
	  set item \[ $Extname get \]
	  set func \[ $Fitsfunc get \]
          if { \"\$func\" != {} && \"\$item\" != {} } {
             $Transtable insert end \$item \$func
             $Transtable see end
             set ict_modified 1
          }
	 "

#  Add a button to remove the current selection from the table.
      $Extchoice addbutton {Remove} \
         "global ict_modified
          CCDRemoveFromList $Transtable clear
          set ict_modified 1
         "

#  Add a button to sort and remove any duplicate entries (by extension name).
      $Extchoice addbutton {Unique} "CCDMakeUnique $Transtable 2 0"

#  Transtable.
#  Label the columns.
      $Transtable setlabel 0 {Extension item}
      $Transtable setlabel 1 {FITS function}

#  Choice.
#  Add OK and Cancel buttons. OK button also saves the table if modified.
      $Choice addcommand OK \
         "global ict_modified
          global CCDimportavail
          set fitsmod \[$Fitstable cget -modified\]
          set tranmod \[$Transtable cget -modified\]
          if { \$ict_modified == 1 || \$fitsmod == 1 || \$tranmod == 1 } {

#  Need to save the contents to a file.
             $Menu invoke Options 3
          }

#  Only exit if a filename has been given.
          if { \$CCDimportavail } {
             $Topwin kill $Topwin
          } else {
             CCDIssueInfo {A filename is required before exit}
	  }
         "

#  Cancel. Make sure that no filename is returned.
      $Choice addcommand Cancel \
         "global CCDimportexists
          global CCDimportfile
          set CCDimportexists 0
	  set CCDimportfile {}
          $Topwin kill $Topwin
	 "

#--------------------------------------------------------------------------
#  Set the window help (pretty minimalist context wise).
#--------------------------------------------------------------------------
      $Topwin sethelp ccdpack CCDCreateImportTableWindow
      $Menu sethelpitem {On Window} ccdpack CCDCreateImportTableWindow
      $Menu sethelp all ccdpack CCDCreateImportTableMenu

      $Refndf sethelp ccdpack CCDCreateImportTableReferenceNDF
      $Fitsextract sethelp all ccdpack CCDCreateImportTableReferenceNDF
      $Fitsbox sethelp ccdpack CCDCreateImportTableReferenceNDF

      $Fitsname sethelp ccdpack CCDCreateImportTableFITStypes
      $Fitstype sethelp ccdpack CCDCreateImportTableFITStypes
      $Fitschoice sethelp all ccdpack CCDCreateImportTableFITStypes
      $Fitstable sethelp ccdpack CCDCreateImportTableFITStypes

      $Extname sethelp ccdpack CCDCreateImportTableFunctions
      $Fitsfunc sethelp ccdpack CCDCreateImportTableFunctions
      $Extchoice sethelp all ccdpack CCDCreateImportTableFunctions
      $Transtable sethelp ccdpack CCDCreateImportTableFunctions
      $Knownbox sethelp ccdpack CCDCreateImportTableFunctions

      $Choice sethelp all ccdpack CCDCreateImportTableOK

#--------------------------------------------------------------------------
#  Widget packing.
#--------------------------------------------------------------------------
#  Pack the top left-hand frame.
      pack $refndf -fill x
      pack $fitsextract
      pack $fitsbox -fill both -expand true

#  Pack the top right-hand frame.
      pack $fitsname -side top -fill x
      pack $fitstype -side top -fill x
      pack $fitschoice -side top -fill x
      pack $fitstable -expand true -fill both

#  Pack top frame.
      pack $lefttop -side left -fill both
      pack $righttop -side right -expand true -fill both

#  Pack bottom right-hand frame
      pack $extname -fill x
      pack $fitsfunc -fill x
      pack $extchoice -fill x
      pack $transtable -side bottom -expand true -fill both

#  Pack bottom frame.
      pack $knownbox -side left -fill both
      pack $rightbot -side right -expand true -fill both

#  Now pack major frames into top-level widget.
      pack $menu -fill x
      pack $choice -side bottom -fill x
      pack $frametop -fill both
      pack $framebot -expand true -fill both

#  Now reveal window.
      wm deiconify $topwin

#---------------------------------------------------------------------------
#  Initialisation of form.
#---------------------------------------------------------------------------
#  Read in table if one given.
      if { $table != {} } {
         set CCDimportfile $table
	 set CCDimportexists 1
         set CCDimportavail 1
         CCDRestoreFromImportFile $CCDimportfile $Fitstable $Transtable
      }

#  And wait for this window to be destroyed, which indicates completion
#  of the task.
      CCDWindowWait $Topwin
      return

#  End of procedure.
  }

# $Id$
