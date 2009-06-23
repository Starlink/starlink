#+
#  Name:
#     GaiaSampAgent

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#    GAIA-specific implementation of SAMP MType support.

#  Description:
#     An instance of this class does the actual work for responding to
#     SAMP messages.  It should be installed in a samp::SampClient,
#     and it should contain a method for each MType that it implements,
#     along with the standard get_subscribed_mtypes method listing them.
#     The name of each method is the actual MType name.
#
#     Some of the MTypes here are standard SAMP ones; a list and
#     descriptions can be found near http://www.ivoa.net/samp/.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     23-JUN-2009 (MBT):
#        Original version, adapted from GaiaPlastic.tcl.

#-

package require rpcvar
namespace import -force rpcvar::*

itcl::class gaia::GaiaSampAgent {

   public method get_subscribed_mtypes {} {
      return {
         image.load.fits
      }
   }

   #  Loads a FITS file specified as a URL into the display.
   public method image.load.fits {sender_id param_list} {
      array set params $param_list
      set img_url $params(url)
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         set fname [get_file_ $img_url]
         if { $fname != {} } {
            $basegaia open $fname
         } else {
            #  Remote file, arrange to download this in the background, if not
            #  already busy downloading...
            if { $urlget_ == {} } {
               set urlget_ \
                  [GaiaUrlGet .\#auto -notify_cmd [code $this display_file_]]
               $urlget_ get $img_url
            } else {
               error "Already downloading - can't do two at once"
            }
         }
      } else {
         error "No GAIA window found for display"
      }
   }

   #  Utility procs:
   #  --------------

   #  Locate an instance of Gaia for displaying images.
   private proc get_gaia_ {} {
      foreach image [::skycat::SkyCat::get_skycat_images] {
         return [winfo parent $image]
      }
      return ""
   }

   #  Tries to turn a URL into a file name.  If the URL uses the file:
   #  protocol (in either its correct RFC1738 "file://host/..." or its
   #  incorrect but common "file:..." form) then the corresponding
   #  local filename is returned.  Otherwise an empty string is returned.
   private proc get_file_ {url} {
      if {[regsub ^file://(localhost|[info host]|)/ $url "" fname]} {
         return /$fname
      } elseif {[regsub ^file: $url "" fname]} {
         return $fname
      } else {
         return ""
      }
   }

   #  Instance variables:
   #  -------------------

   #  Name of the active instance of GaiaUrlGet.
   protected variable urlget_ {}
}
