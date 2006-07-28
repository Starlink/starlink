#+
#  Name:
#     GaiaPlastic

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     GAIA-specific implementation of PLASTIC messages.

#  Description:
#     An instance of this class does the actual work for responding to
#     PLASTIC messages.  It should be supplied to the constructor of
#     a plastic::PlasticApp, and it should contain a method for each
#     message that it implements.  The name of each method is the actual
#     PLASTIC message ID, and the argument list is the PLASTIC ID of
#     the sender application, followed by all the other arguments which
#     have been passed as message parameters.
#
#     See http://plastic.sourceforge.net/ for listing of the standard
#     PLASTIC messages, their argument lists, and their semantics.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     13-JUL-2006 (MBT)
#        Original version.
#     {enter_further_changes_here}

#-

package require rpcvar
namespace import -force rpcvar::*

itcl::class gaia::GaiaPlastic {

   #  PLASTIC message implementations.
   #  --------------------------------

   #  Echo text.
   public method ivo://votech.org/test/echo {sender_id text args} {
      return [rpcvar string $text]
   }

   #  Return application name.
   public method ivo://votech.org/info/getName {sender_id args} {
      return "gaia"
   }

   #  Return application description.
   public method ivo://votech.org/info/getDescription {sender_id args} {
      return "Graphical Astronomy and Image Analysis Tool"
   }

   #  Return version of PLASTIC supported.
   public method ivo://votech.org/info/getVersion {sender_id args} {
      return [rpcvar string 0.4]
   }

   #  Return URL of an icon representing this application.
   public method ivo://votech.org/info/getIconURL {sender_id args} {
      return "http://star-www.dur.ac.uk/~pdraper/gaia/gaialogo.gif"
   }

   #  Load a FITS file specified as a URL into the display.
   public method  ivo://votech.org/fits/image/loadFromURL {sender_id img_url
                                                           args} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         set fname [get_file_ $img_url]
         if { $fname != {} } {
            $basegaia open $fname
            return $TRUE
         } else {
            #  Remote file, arrange to download this in the background.
            set urlget_ \
               [GaiaUrlGet .\#auto -notify_cmd [code $this display_file_]]
            $urlget_ get $img_url
            return $TRUE
         }
      }
      return $FALSE
   }

   #  Point at a coordinate by drawing an identifier graphic, ra and dec
   #  are both in decimal degrees.
   public method  ivo://votech.org/sky/pointAtCoords {sender_id ra dec args} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         if { ![catch {$basegaia position_of_interest $ra $dec "deg J2000"}]} {
            return $TRUE
         }
      }
      return $FALSE
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

   #  Download and display a file.
   protected method display_file_ {filename type} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         $basegaia open $filename
      }
      delete object $urlget_
      set urlget_ {}
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


   #  Name of the active instance of GaiaUrlGet.
   protected variable urlget_ {}
   private common TRUE [rpcvar boolean 1]
   private common FALSE [rpcvar boolean 0]
}
