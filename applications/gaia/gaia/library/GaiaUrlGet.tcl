#+
#  Name:
#     GaiaUrlGet

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Download the content of a URL to a local file.

#  Description:
#     Instances of this class are used to download the content of
#     a URL to a named local file. The download happens in a background
#     process, whose progress can be monitored and interrupted.

#  Invocations:
#
#        GaiaUrlGet object_name [configuration options]
#
#     This creates an instance of a GaiaUrlGet object. The return is
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

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2008 Science and Technology Facilities Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     14-JUL-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

package require ftp
package require uri

itk::usual GaiaUrlGet {}

itcl::class gaia::GaiaUrlGet {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the interface.
      #  Label for URL.
      itk_component add label {
         label $w_.label -wraplength 500
      }
      pack $itk_component(label) -side top -fill x -expand 1 -pady 5 -padx 5

      #  Add a progress bar to display the progress of data transfers.
      itk_component add progress {
         util::ProgressBar $w_.progress
      }
      pack $itk_component(progress) -side top -fill x -expand 1 -pady 5 -padx 5

      #  Button to interrupt download.
      itk_component add interrupt {
         button $w_.interrupt -text "Interrupt" -command [code $this interrupt]
      }
      pack $itk_component(interrupt) -side top -expand 1 -pady 5 -padx 5

      #  Evaluate options (like -withdraw) after UI initialisation.
      eval itk_initialize $args

      #  Dialog-like behaviour (no modal blocking).
      wm transient $w_ [winfo parent $w_]
      wm title $w_ "GAIA: Download data"

      #  Make sure proxys are established.
      check_proxies

      #  Controller for temporary files.
      set tmpfiles_ [gaia::GaiaTempName \#auto -prefix "GaiaTemp"]

      #  Create the astrocat instance for doing the download. Needs an actual
      #  catalogue to open, so create a fudged one.
      astrocat $w_.cat
      catch {
         set tmpcat_ [$tmpfiles_ get_typed_name ".TAB"]
         set fd [open "$tmpcat_" w]
         puts $fd "id\t x\t y\n--\n0\t 0\t 0\n"
         close $fd
         $w_.cat open "$tmpcat_"
      }

      #  Create an object for running interruptable batch queries.
      util::Batch $w_.batch \
         -command [code $this download_done] -debug $itk_option(-debug)

   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Close the astrocat instance.
      catch {$w_.cat delete}

      #  Fudged catalogue.
      catch {file delete -force $tmpcat_}

      #  Make sure feedback pipes are closed.
      if { $rfd_ != {} } {
         catch {close $rfd_}
      }
      if { $wfd_ != {} } {
         catch {close $wfd_}
      }

      #  Delete temporary files.
      catch {
         $tmpfiles_ clear
         delete object tmpfiles_
      }
   }

   #  Methods:
   #  --------

   #  Download the data from a given URL to a temporary local file. If the
   #  URL points at a local file that name will be returned.
   #
   #  When the download complete the notify_cmd command will be executed and
   #  the name of the temporary file will be appended to it.

   public method get { url } {

      if { [string first "file:" $url] == 0 } {

         #  URL points at a local file, just return that.
         set filename [regsub {file://localhost|file://|file:} $url ""]
         download_done 0 [list $filename image/x-fits]

      } elseif { [string first "ftp:" $url] == 0 } {

         #  Assume FTP, so activate dialog and start the download.
         wm deiconify $w_
         raise $w_

         $itk_component(label) configure -text "Downloading: $url"

         $itk_component(progress) config -text \
            "Contacting remote server..."
         $itk_component(progress) look_busy
         set_feedback on

         set tempfile [$tmpfiles_ get_typed_name ".fits"]
         $w_.batch bg_eval [code $this get_ftp_ $url $tempfile]

      } else {

         #  Assume HTTP, so activate dialog and start the download.
         wm deiconify $w_
         raise $w_

         $itk_component(label) configure -text "Downloading: $url"

         $itk_component(progress) config -text \
            "Contacting remote server..."
         $itk_component(progress) look_busy
         set_feedback on

         set tempfile [$tmpfiles_ get_typed_name ".fits"]
         $w_.batch bg_eval [code $this get_http_ $url $tempfile]
      }
   }

   #  Download the given HTTP URL.
   protected method get_http_ {url tempfile} {
      return [$w_.cat getpreview -url $url -tempfile $tempfile]
   }

   #  Download the the FTP URL.
   protected method get_ftp_ {url tempfile} {
      if { [catch {exec $::gaia_dir/ftpget $url $tempfile} msg] } {
         error "Failed to download FTP file: $msg"
      }
      return "$tempfile image/x-fits"
   }

   #  Method called when the download is complete.
   #
   #  The "status" argument is the status of the background http get operation
   #  (0 if ok). The result is a list of {filename Content-type}, where
   #  filename contains the data and Content-type indicates the type of the
   #  downloaded data.
   #
   #  If we get an authorization error, then just throw an error.
   protected method download_done {status result} {

      set_feedback off

      $itk_component(label) configure -text ""
      set filename {}
      set type {}

      if { $status } {
         error_dialog $result $w_
      } else {
         if {[llength $result] != 2} {
            error_dialog "error downloading remote data (result = $result)"
         } else {
            if { $itk_option(-notify_cmd) != {} } {
               lassign $result filename type

               #  Check if this is a compressed file. If so change the suffix
               #  so that it will be opened correctly.
               set filename [check_compressed_ $filename]
            }
         }
      }

      #  Always perform notify command (so that blocks can be released).
      if { $itk_option(-notify_cmd) != {} } {
         eval $itk_option(-notify_cmd) \$filename \$type
      }
      catch {wm withdraw $w_}
   }

   #  Open or close a pipe to get feedback during HTTP transfers.
   #  The arg should be "on" to turn feedback on, or "off" to turn it off.
   protected method set_feedback { onoff } {

      #  Process any pending file events. Note: this is important: if we don't
      #  process the events before closing the feedback file, a crash may
      #  result.
      update

      if { $itk_option(-debug) } {
         #  If -debug was given, the feedback is disabled as the query
         #  is done in the foreground.
         return
      }
      if { "$onoff" == "on" } {

         #  Open the pipe to receive the feedback.
         lassign [pipe] rfd_ wfd_
         fileevent $rfd_ readable [code $this feedback_]
         $w_.cat feedback $wfd_

      } elseif { $rfd_ != {} } {

         #  Close the feedback pipe.
         ::close $rfd_
         ::close $wfd_
         set rfd_ {}
         set wfd_ {}
         $w_.cat feedback {}
      }
   }

   #  This method is called by the fileevent handler during the data transfer.
   protected method feedback_ {} {
      set text [gets $rfd_]

      if {[scan $text {total length: %d bytes} n] == 1} {
         $itk_component(progress) config -to $n
      } elseif {[scan $text {read %d bytes} n] == 1} {
         $itk_component(progress) config -value $n
      }

      $itk_component(progress) config -text $text
      update idletasks
   }

   #  Interrupt the download.
   public method interrupt {} {
      $w_.batch interrupt
      set_feedback off
      wm withdraw $w_
   }

   #  Check the temporary file to see if it is known to be compressed.
   protected method check_compressed_ {filename} {

      set fid [::open $filename "r"]
      set magic0 [::read $fid 1]
      set magic1 [::read $fid 1]

      if { $magic0 == "\037" } {
         if { $magic1 == "\213" } {
            #  GZIP.
            set newname "${filename}.gz"
         } elseif { $magic1 == "\036" } {
            #  HUFFMANN.
            set newname "[file rootname $filename].hfits"
         } elseif { $magic1 == "\235" } {
            #  UNIX COMPRESS
            set newname "${filename}.Z"
         }
         if { $newname != $filename } {
            ::file rename -force $filename $newname
            $tmpfiles_ add_name $newname
            return $newname
         }
      }
      return $filename
   }

   #  Check for a file ~/.skycat/proxies, once each session, and use it to
   #  initialize environment variables for a proxy server (see also
   #  tclutil/util/src/HTTP.C).
   public proc check_proxies {} {
      global ::env
      #  Only do it once.
      if { $checked_proxies_ } {
         return
      }
      cat::ProxyDialog::check_proxies $env(HOME)/.skycat/proxies
      set checked_proxies_ 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  A command to execute when the download is complete. This will have the
   #  local filename appended and the mime type of the data.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Run queries in foreground to help debugging.
   itk_option define -debug debug Debug 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Pipe to read feedback during HTTP transfer.
   protected variable rfd_ {}

   #  Pipe to write feedback during HTTP transfer.
   protected variable wfd_ {}

   #  Temporary catalogue to initialise astrocat instance.
   protected variable tmpcat_ {}

   #  Object for managing temporary files.
   protected variable tmpfiles_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Make sure proxy server settings are done once.
   common checked_proxies_ 0

   #  Simple count for creating files.
   common count_ 0

#  End of class definition.
}
