   proc CCDInsertNDFswithFactors { ndfs list args } {
#+
#  Name:
#     CCDInsertNDFswithFactors

#  Type of Module:
#     Tcl/Tk procedure

#  Purpose:
#     Parses and inserts a list of NDF names and factors into
#     a listbox.

#  Description:
#     This routine reads the contents of several entry widgets and attempts
#     to interpret this as a list of NDF names and some associated factors.
#     The NDF names may include wildcards which will be expanded using
#     globing and may be separated by spaces and or commas. The factors
#     must be space and/or comma separated. The number of factors must
#     equal the number of NDFs or must just be one value. If only one
#     value is given then this is used for all NDFs. The resultant list of NDF
#     names and factors are inserted at the end of the listbox (which
#     should be a Ccd_multitem listbox of size the number factors plus the 
#     ndfs).

#  Arguments:
#     ndfs = window (read)
#        The entry widget which contains the expression to be
#	 interpreted as a list of NDF names.
#     list = window (read)
#	 The name of the Ccd_multitem composite listbox which is to
#	 have the names of the NDFs and corresponding factors entered.
#     args = list of windows (read)
#        The entry widgets which contain the expression to be
#        interpreted as additional list of factors (may be {} in which
#        case no factors are used..

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     9-MAR-1994 (PDRAPER):
#     	 Original version.
#     15-MAR-1994 (PDRAPER):
#     	 Added prepend and append arguments.
#     5-MAY-1994 (PDRAPER):
#     	 Nows deals with factors as well as NDF names.
#     26-MAY-1994 (PDRAPER):
#        Now handles multiple factor entries.
#     {enter_further_changes_here}

#-

#  Get the contents of the NDF entry widget.
      set ndfexpress [ $ndfs get ]
      if { "$ndfexpress" != {} } {

#  Remove commas from expression (note may need to protect some commas
#  in parentheses etc. in future but ok for now)
         regsub -all "," "$ndfexpress" " " newndfexpress

#  Expand all the names. One word at a time is sorted to maintain the
#  same order as a directory listing of word.
         foreach word $newndfexpress { 
            lappend realndfs [ lsort [ glob -nocomplain $word ] ]
         }
         set realndfs [ eval concat $realndfs ]

#  Number of ndfs.
	 set nndfs    [ llength $realndfs ]

#  Now try for any factors.
         if { [ info exists realfactors ] } { unset realfactors }
	 set nargs [ llength $args ]
	 if { $nargs != 0 } {
	    for { set i 0 } { $i < $nargs } { incr i } { 
	       set facwin [ lindex $args $i ]
	       set factorexpress [ $facwin get ]
	       if { "$factorexpress" != {} } {
		  
#  Remove any commas as in NDFs case.
		  regsub -all "," "$factorexpress" " " realfactors($i)

#  Have we got the same number of
		  set nfactors($i) [ llength $realfactors($i) ]
		  if { $nfactors($i) != $nndfs && $nfactors($i) != 1 } {

#  This is a problem. Give up.
		     CCDIssueInfo "The number of factors is not 1 or does not
($nfactors($i)) does not equal the number of NDFs ($nndf)"
                     return
		  }
	       } else {

#  No factors so cannot proceed.
                  CCDIssueInfo "No factors given (must be at least 1)"
                  return
               }
	    }
         }

#  Now write out the values to the listboxes. See if the number of factors 
#  is one and record the current directory (in case we need it).
         set current [pwd]

#  Look at each ndf in turn.
         for { set i 0 } { $i < $nndfs } { incr i } { 
            set thisndf [ lindex $realndfs $i ] 

#  Create a string of the required factors.
            set thisfact {}
            for { set j 0 } { $j < $nargs } { incr j } { 
               if { $nfactors($j) == 1 } { 
                  lappend thisfact $realfactors($j)
               } else {
                  lappend thisfact [ lindex $realfactors($j) $i ]
               }
            }

#  Does the file specification contain full path information? Assume
#  that such names start with /, ./ or ../. If the names are not
#  complete we use the [pwd] function to get the value of the current
#  directory.
            if { [ regexp (^../|^/|^./) $thisndf ] } {
               eval $list insert end $thisndf $thisfact
            } else {

#  Add directory information as none is given.
               eval $list insert end ${current}/${thisndf} $thisfact
            }
         }
      }

#  End of procedure.
   }
# $Id$
