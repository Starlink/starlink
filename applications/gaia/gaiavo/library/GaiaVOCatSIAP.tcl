#+
#  Name:
#     GaiaVOCatSIAP

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a SIAP server for images.

#  Description:
#     Extends the GaiaVOCat class to query a given SIAP server for any images.
#     The images may then be downloaded and displayed.

#  Invocations:
#
#        GaiaVOCatSIAP object_name [configuration options]
#
#     This creates an instance of a GaiaVOCatSIAP object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Copyright:
#     Copyright (C) 2008 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     08-AUG-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaVOCatSIAP {}

itcl::class gaiavo::GaiaVOCatSIAP {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCat

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------

   #  Add the component that will control the query.
   protected method add_query_component_ {} {

      itk_component add siap {
         gaiavo::GaiaVOSIAPSearch $w_.siap \
            -accessURL $itk_option(-accessURL) \
            -feedbackcommand  [code $this set_feedback] \
            -astrocat [code $w_.cat] \
            -command [code $this query_done]
      }
      pack $itk_component(siap) -side top -fill both -expand 1
      add_short_help $itk_component(siap) {Controls for querying SIAP server}

      set query_component_ $itk_component(siap)
   }

   #  Open a service, "args" is a list of values from a row of the current table.
   protected method open_service_ {args} {

      #  Need to locate the VOX:Image_AccessReference field to get the URL for
      #  downloading the image.
      set ucds [$w_.cat ucd]
      set n 0
      foreach ucd $ucds {
         if { [string match -nocase "*accessref*" $ucd] } {
            break
         }
         incr n
      }
      set accessref [eval lindex $args $n]

      if { $itk_option(-gaia) != {} } {
         if { $urlget_ == {} } {
            set urlget_ [gaia::GaiaUrlGet .\#auto \
                            -notify_cmd [code $this display_image_]]
            blt::busy hold $w_
            $urlget_ get $accessref
         }
      }
   }

   #  Display an image.
   protected method display_image_ {filename type} {
      blt::busy release $w_
      $itk_option(-gaia) open $filename
      if { $urlget_ != {} } {
         catch {delete object $urlget_}
      }
      set urlget_ {}
   }

   #  Extract the accessURL for the SIAP service from a list of headers
   #  and the associated data row.
   public proc getAccessURL {headers row} {
      eval lassign "$row" $headers
      if { [info exists accessURL] } {
         return $accessURL
      }
      return {}
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The accessURL for the SIAP server.
   itk_option define -accessURL accessURL AccessURL {}

   #  Instance of GAIA to display the image.
   itk_option define -gaia gaia Gaia {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Active getter for downloading an image.
   protected variable urlget_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

}
