proc CCDDoPresent {Top args} {
#+
#  Name:
#     CCDDoPresent

#  Purpose:
#     Sorts data from CCDNDFDoImport and runs the PRESENT application.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     See CCDNDFDoImport for how this works and what on (this was a
#     code segment for the OK button that has been extracted for
#     clarity).

#  Copyright:
#     Copyright (C) 1997 Central Laboratory of the Research Councils.
#     All Rights Reserved.

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
#     {enter_new_authors_here}

#  History:
#     16-APR-1997 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
   global CCDhaveframe
   global CCDfilternames
   global CCDndfs
   global Target
   global Flat
   global Bias
   global Dark
   global Flash
   global Masterbias
   global Masterflat
   global Masterdark
   global Masterflash

#.

#  Create a list of space separated filter names. This should be
#  set to "NONE" if no filters are in use.
   if { [info exists CCDfilternames] } {
      set Fnames [split $CCDfilternames ", "]
   }

#  Do not proceed to present the files etc. if all isn't well.
   set proceed 1

#  Targets. Need at least one for each filter
   if { $CCDhaveframe(targets) } {
      foreach f $Fnames {
         set nt [CCDUpdateColourLists targets $Target($f) $f]
         if { $nt == 0 } {
            if { $f == "NONE" } {
               CCDIssueInfo "You must supply some TARGET frames"
            } else {
               CCDIssueInfo "You must supply some TARGET($f) frames"
            }
            set proceed 0
         }
      }
   }

#  Flatfields. Same as targets.
   if { $CCDhaveframe(flatfields) } {
      foreach f $Fnames {
         set nf [CCDUpdateColourLists flatfields $Flat($f) $f]
         if { $nf == 0 } {
            if { $f == "NONE" } {
               CCDIssueInfo "You must supply some FLAT frames"
            } else {
               CCDIssueInfo "You must supply some FLAT($f) frames"
            }
            set proceed 0
         }
      }
   }

#  Biases. These are colourless.
   if { $CCDhaveframe(biases) } {
      set CCDndfs(biases) [CCDCreateListofNames $Bias {} {}]
      set nb [$Bias size]
      if { $nb == 0 } {
         CCDIssueInfo "You must supply some BIAS frames"
         set proceed 0
      }
   }

#  Darks. These are colourless, but may have exposure factors.
   if { $CCDhaveframe(darks) } {
      set nd [CCDUpdateFactorLists darks $Dark]
      if { $nd == 0 } {
         CCDIssueInfo "You must supply some DARK frames"
         set proceed 0
      }
   }

#  Flashes. Same as darks
   if { $CCDhaveframe(flashes) } {
      set nf [CCDUpdateFactorLists flashes $Flash]
      if { $nf == 0 } {
         CCDIssueInfo {You must supply some FLASH frames}
         set proceed 0
      }
   }

#  Master Bias.
   if { $CCDhaveframe(master_biases) } {
      set CCDndfs(master_biases) [CCDCreateListofNames $Masterbias {} {}]
      set nb [$Masterbias size]
      if { $nb == 0 } {
         CCDIssueInfo "You must supply a MASTER BIAS frame"
         set proceed 0
      }
   }

#  Master flatfields.
   if { $CCDhaveframe(master_flats) } {
      foreach f $Fnames {
         set nf [CCDUpdateColourLists master_flats $Masterflat($f) $f]
         if { $nf == 0 } {
            if { $f == "NONE" } {
               CCDIssueInfo "You must supply a MASTER FLAT frame"
            } else {
               CCDIssueInfo "You must supply some MASTER FLAT($f) frames"
            }
            set proceed 0
         }
      }
   }

#  Master Dark.
   if { $CCDhaveframe(master_darks) } {
      set CCDndfs(master_darks) [CCDCreateListofNames $Masterdark {} {}]
      set nb [$Masterdark size]
      if { $nb == 0 } {
         CCDIssueInfo "You must supply a MASTER DARK frame"
         set proceed 0
      }
   }

#  Master flash.
   if { $CCDhaveframe(master_flashes) } {
      set CCDndfs(master_flashes) [CCDCreateListofNames $Masterflash {} {}]
      set nb [$Masterflash size]
      if { $nb == 0 } {
         CCDIssueInfo "You must supply a MASTER FLASH frame"
         set proceed 0
      }
   }

#  If all necessary conditions have been met, then present the frames.
   if { $proceed } {
      eval CCDPresent $Top $args
      $Top kill $Top

#  Reset the lists of frames for possible next time.
      catch { unset Target }
      catch { unset Flat }
      catch { unset Bias }
      catch { unset Dark }
      catch { unset Flash }
      catch { unset Masterbias }
      catch { unset Masterflat }
      catch { unset Masterdark}
      catch { unset Masterflash }
   }

#  End of procedure.
}
# $Id$
