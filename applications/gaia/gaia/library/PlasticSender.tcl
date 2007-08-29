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
#     22-SEP-2006 (PWD):
#        Added method to send a spectrum.
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
   #  If recipients is non-empty it gives a list of application
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

   #  Sends a spectrum image to one or more listening PLASTIC applications.
   #  If recipients is non-empty it gives a list of application IDs for the
   #  applications to send to.  If empty, the message is broadcast to all. The
   #  spectrum must be in FITS format and exist.  "shortname" is just some
   #  simple value (usually OBJECT), and "coordunit" and "dataunit" the units
   #  (SSAP/FITS standard).
   public method send_spectrum {spectrum shortname coordunit dataunit
                                recipients} {
      check_app_
      set msg_id "ivo://votech.org/spectrum/loadFromURL"

      #  Create a URL of the local filename given.
      if { [catch {
         set url [get_file_url_ $spectrum]

         set map(Access.Reference) $url
         set map(Access.Format) "application/fits"
         set map(Target.Name) $shortname
         if { $dataunit != {} && $coordunit != {} } {
            set map(vox:spectrum_units) "$coordunit $dataunit"
         }

         set send_args [list [rpcvar string $url] \
                             [rpcvar string $url] \
                             [rpcvar struct map]]
         $plastic_app send_message_async $msg_id $send_args $recipients
      } msg] } {
         error "error in spectrum send: $::errorInfo"
      }
   }

   #  Identify a particular sky position as of interest and transmit it
   #  to other PLASTIC applications.  ra and dec are J2000 in either
   #  degrees or sexagesimal.
   #  If recipients is non-empty it gives a list of application
   #  IDs for the applications to send to.  If empty, the message is
   #  broadcast to all.
   public method send_radec {ra dec recipients} {
      check_app_
      set msg_id "ivo://votech.org/sky/pointAtCoords"
      catch {
         if {[catch {expr $ra + $dec}]} {
            lassign [::skycat::.wcs hmstod $ra $dec] ra dec
         }
         set send_args [list [rpcvar double $ra] [rpcvar double $dec]]
         $plastic_app send_message_async $msg_id $send_args $recipients
      }
   }

   #  Identify a single row index within a PLASTIC-acquired table as of
   #  interest, and invite other applications to highlight it.
   #  If recipients is non-empty it gives a list of application
   #  IDs for the applications to send to.  If empty, the message is
   #  broadcast to all.
   public method send_row {table_id idx recipients} {
      check_app_
      set msg_id "ivo://votech.org/votable/highlightObject"
      set send_args [list $table_id [rpcvar int $idx]]
      catch {
         $plastic_app send_message_async $msg_id $send_args $recipients
      }
   }

   #  Identify a list of row indices within a PLASTIC-acquired table as of
   #  interest, and invite other applications to highlight this list.
   #  idx_list is a list of integer values giving 0-based indices of the
   #  rows to select, in terms of the table as originally qcquired.
   #  If recipients is non-empty it gives a list of application
   #  IDs for the applications to send to.  If empty, the message is
   #  broadcast to all.
   public method send_selection {table_id idx_list recipients} {
      check_app_
      set msg_id "ivo://votech.org/votable/showObjects"
      if {[llength $idx_list] > 0} {
         set send_args [list $table_id [rpcvar array $idx_list]]
         $plastic_app send_message_async $msg_id $send_args $recipients
      }
   }

   #  Check that we have the necessary configuration variables set.
   #  If not, generate an error.
   private method check_app_ {} {
      if {$plastic_app == ""} {
         error "No PlasticApp"
      } elseif {! [$plastic_app is_registered]} {
         error "Not registered"
      }
   }


   # Common Procedures:
   # ------------------

   #  Turn a filename into a file: URL.
   protected proc get_file_url_ {file} {
      if {[string index $file 0] != "/"} {
         set file "[pwd]/$file"
      }

      # If in CYGWIN environment make the URL a Windows file path.
      if { [string match {CYGWIN*} $::tcl_platform(os)] } {
         set file "file://localhost/[exec cygpath -wa $file]"
         regsub -all {\\} $file {/} file
         return "$file"
      }
      return "file://localhost$file"
   }

   #  Get the name of a file it's OK to use for scratch space.
   #  The exten argument gives a file extension (e.g. ".fits").
   protected proc get_temp_file_ {exten} {
      set tmpdir ""
      if { [info exists ::env(GAIA_TEMP_DIR)] } {
         set trydirs "$::env(GAIA_TEMP_DIR) /tmp /usr/tmp ."
      } else {
         set trydirs "/tmp /usr/tmp ."
      }
      foreach trydir $trydirs {
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
