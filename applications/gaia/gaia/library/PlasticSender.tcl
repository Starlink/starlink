#+
#  Name:
#     PlasticSender

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Provides functionality for sending PLASTIC messages.

#  Description:
#     This class contains methods which use a supplied PlasticApp object
#     to send GAIA-specific application-level messages to some or all
#     of the other registered PLASTIC applications.

#  Configuration options:
#
#     plastic_app
#        The PlasticApp object which handles hub communication.
#        This object will not function unless it has an active PlasticApp.

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
#     PWD: Peter W. Draper
#     {enter_new_authors_here}

#  History:
#     21-JUL-2006 (MBT):
#        Original version.
#     26-JUL-2006 (PWD):
#        Fixed up support for non-FITS images.
#     {enter_further_changes_here}

#-

itcl::class gaia::PlasticSender {

   constructor {args} {
      eval configure $args
   }

   #  Methods:
   #  --------

   #  Sends an image to one or more listening PLASTIC applications.
   #  The image_ctrl argument gives a GaiaImageCtrl object.
   #  If the recipients_list is non-empty it gives a list of application
   #  IDs for the applications to send to.  If empty, the message is
   #  broadcast to all.
   public method send_image {image_ctrl recipients} {
      check_app_
      set msg_id "ivo://votech.org/fits/image/loadFromURL"

      #  Get the actual image object which provides most of the useful
      #  methods.
      set image [$image_ctrl get_image]

      #  If the image is FITS and has a filename which exists, 
      #  we can just send a file: URL based on it.
      set file [$image cget -file]
      if { [$image isfits] && $file != {} && [::file exists $file] } {
         if { [catch {
            set url [get_file_url_ $file]
            $plastic_app send_message_async $msg_id $url $recipients
         } msg] } {
            error "error in FITS send: $::errorInfo"
         }

      #  Otherwise life is more complicated.  We have to write the 
      #  image to a temporary FITS file, send the load message
      #  synchronously, and then tidy up the file.
      } else {

         #  If it's not FITS, this will only work if the CONVERT package
         #  is available.
         if { ! [$image isfits] && ! [info exists ::env(NDF_FORMATS_IN)] } {
            error "Cannot send non-FITS images, no CONVERT support"
         }

         #  Attempt the write and send.
         set tmpfile [get_temp_file_ ".fits"]
         if { [catch {
            $image dump $tmpfile FITS-WCS
            set url [get_file_url_ $tmpfile]
            $plastic_app send_message_sync $msg_id $url $recipients
            ::file delete -force $tmpfile
         } msg] } {
            ::file delete -force $tmpfile
            error "error in image send: $::errorInfo"
         }
      }
   }

   #  Check that we have the necessary configuration variables set.
   #  If not, generate an error.
   private method check_app_ {} {
      if {$plastic_app == ""} {
         error "No PlasticApp"
      }
   }


   # Common Procedures:
   # ------------------

   #  Turn a filename into a file: URL.
   protected proc get_file_url_ {file} {
      if {[string index $file 0] != "/"} {
         set file "[pwd]/$file"
      }
      return "file://localhost$file"
   }

   #  Get the name of a file it's OK to use for scratch space.
   #  The exten argument gives a file extension (e.g. ".fits").
   protected proc get_temp_file_ {exten} {
      set tmpdir ""
      foreach trydir {/tmp /usr/tmp .} {
         if {[file isdirectory $trydir] && [file writable $trydir]} {
            set tmpdir $trydir
            break
         }
      }
      if { $tmpdir == "" } {
         error "No temporary directory"
      }

      set basefile "${tmpdir}/gaia_temp_"
      for { set ix 1 } { $ix < 100 } { incr ix } {
         set tryfile "$tmpdir/gaia_temp_$ix$exten"
         if {! [file exists $tryfile] } {
            return $tryfile
         }
      }
      error "No free files with name like $tryfile"
   }


   #  Instance variables:
   #  -------------------

   #  Set this to a PlasticApp object to make this object ready for use.
   public variable plastic_app {}

}
