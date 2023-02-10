#+
#  Name:
#     SampSender

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Provides functionality for sending SAMP messages.

#  Description:
#     This class contains methods which use a supplied SampClient object
#     to send GAIA-specific client-level messages to one or all
#     of the other registered SAMP clients.
#
#     The send methods here feature a recipient_id argument; if this is
#     supplied it is the SAMP client-id of the intended recipient.
#     If it is blank, the send is a broadcast to all subscribed clients.

#  Configuration options:
#
#     samp_client
#        The SampClient object which handles hub communication.
#        This object will not function unless it has an active SampClient.

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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     25-JUN-2009 (MBT):
#        Original version, adapted from PlasticSender.tcl.
#     {enter_further_changes_here}

#-

itcl::class gaia::SampSender {

   constructor {args} {
      eval configure $args
   }

   destructor {
      if { $temp_files_ != {} } {
         $temp_files_ clear
      }
   }

   #  Methods:
   #  --------

   #  Sends an image to one or all subscribed SAMP clients.
   #  The image_ctrl argument gives a GaiaImageCtrl object.
   public method send_image {image_ctrl recipient_id} {
      check_client_
      set mtype "image.load.fits"

      #  Get the actual image object which provides most of the useful
      #  methods.
      set image [$image_ctrl get_image]

      #  If the image is FITS and has a filename which exists,
      #  we can just send a file: URL based on it.
      set file [$image cget -file]
      if { [$image isfits] && $file != {} && [::file exists $file] } {
         if { [catch {
            set url [get_file_url_ $file]
            set params(url) $url
            {*}$samp_client call $mtype [array get params] $recipient_id
         } msg]} {
            error "error in FITS send: $msg"
         }

      #  Otherwise life is more complicated.  We have to write the image
      #  to a temporary FITS file, and arrange that the file is deleted
      #  when all the responses are in.
      } else {

         #  If it's not FITS, this will only work if the CONVERT package
         #  is available.
         if { ! [$image isfits] && ! [info exists ::env(NDF_FORMATS_IN)] } {
            error "Cannot send non-FITS images, no CONVERT support"
         }

         #  Attempt the write and send.
         if { $temp_files_ == {} } {
            set temp_files_ [gaia::GaiaTempName \#auto \
                                -prefix "GaiaTempImage" \
                                -type ".fits" -exists 0]
         }
         set tmpfile [$temp_files_ get_name]
         set tidy_code "::file delete $tmpfile"
         if { [catch {
            $image dump $tmpfile FITS-WCS
            set url [get_file_url_ $tmpfile]
            set params(url) $url
            {*}$samp_client call $mtype [array get params] $recipient_id $tidy_code
         } msg] } {
            eval $tidy_code
            error "error in image send: $::errorInfo"
         }
      }
   }

   #  Sends a spectrum image to one or all subscribed SAMP clients.  The
   #  spectrum must be in FITS format and exist.  "shortname" is just some
   #  simple value (usually OBJECT) and "coordunit" and "dataunit" the units
   #  (SSAP/FITS standard).
   public method send_spectrum {spectrum shortname coordunit dataunit
                                recipient_id} {
      check_client_
      set mtype "spectrum.load.ssa-generic"

      #  Create a URL of the local filename given.
      if { [catch {
         set url [get_file_url_ $spectrum]

         set ssameta(Access.Reference) $url
         set ssameta(Access.Format) "application/fits"
         set ssameta(Target.Name) $shortname
         if { $dataunit != "" && $coordunit != "" } {
            set ssamap(vox:spectrum_units) "$coordunit $dataunit"
         }
         set params(url) $url
         set params(meta) [rpcvar struct [array get ssameta]]
         {*}$samp_client call $mtype [array get params] $recipient_id
      } msg] } {
         error "error in spectrum send: $::errorInfo"
      }
   }

   #  Identify a particular sky position as of interest and transmit it
   #  to other SAMP clients.  ra and dec are J2000 in either
   #  degrees or sexagesimal.
   public method send_radec {ra dec recipient_id} {
      check_client_
      set mtype "coord.pointAt.sky"

      catch {
         if {[catch {expr $ra + $dec}]} {
            lassign [::skycat::.wcs hmstod $ra $dec] ra dec
         }
         set params(ra) $ra
         set params(dec) $dec
         {*}$samp_client notify $mtype [array get params] $recipient_id
      }
   }

   #  Identify a single row index within a SAMP-acquired table as of
   #  interest, and invite other clients to highlight it.
   public method send_row {table_id idx recipient_id} {
      check_client_
      set mtype "table.highlight.row"
      set params(table-id) $table_id
      set params(row) $idx
      catch {
         {*}$samp_client notify $mtype [array get params] $recipient_id
      }
   }

   #  Identify a list of row indices within a SAMP-acquired table as of
   #  interest, and invite other clients to highlight this list.
   #  idx_list is a list of integer values giving 0-based indices of the
   #  rows to select, in terms of the table as original acquired.
   public method send_selection {table_id idx_list recipient_id} {
      check_client_
      set mtype "table.select.rowList"
      if {[llength $idx_list] > 0} {
         set params(table-id) $table_id
         set params(row-list) [rpcvar array $idx_list]
         catch {
            {*}$samp_client notify $mtype [array get params] $recipient_id
         }
      }
   }

   #  Check that we have the necessary configuration variables set.
   #  If not, generate an error.
   private method check_client_ {} {
      if {$samp_client == ""} {
         error "No SampClient"
      } elseif {! [{*}$samp_client is_registered]} {
         error "Not registered"
      }
   }

   #  Common Procedures:
   #  ------------------

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

   #  Instance Variables:
   #  -------------------

   #  Set this to a SampClient object to make this object ready for use.
   public variable samp_client {}

   private variable temp_files_ {}
}
