#+
#  Name:
#     GaiaVOSIAPSearch

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for querying a VO SIAP server.

#  Description:
#     Provides controls for constructing and doing a query of a SIAP version
#     1 server. The server is defined by an accessURL, which will be qualified
#     by a position and size on the sky.

#  Invocations:
#
#        GaiaVOSIAPSearch object_name [configuration options]
#
#     This creates an instance of a GaiaSIAPSearch object. The return is
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

itk::usual GaiaVOSIAPSearch {}

itcl::class gaiavo::GaiaVOSIAPSearch {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Handler for temporary files.
      set tempcats_ [gaia::GaiaTempName \#auto -prefix GaiaVOSIAP \
                        -exists 0 -type ".TAB"]

      #  Display the SIAP accessURL. XXX Can edit during testing.
      set lwidth 10
      set vwidth 50
      itk_component add server {
         LabelEntry $w_.server \
            -text "Server:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $itk_option(-accessURL) \
            -textvariable [scope itk_option(-accessURL)]
      }
      pack $itk_component(server) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(server) {SIAP server URL}

      #  Get the position on the sky, an RA and a Dec.
      itk_component add ra {
         LabelEntry $w_.ra \
            -text "RA:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $ra_ \
            -textvariable [scope ra_]
      }
      pack $itk_component(ra) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(ra) {RA centre of images}

      itk_component add dec {
         LabelEntry $w_.dec \
            -text "Dec:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $dec_ \
            -textvariable [scope dec_]
      }
      pack $itk_component(dec) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(dec) {Dec centre of images}

      itk_component add size {
         LabelEntry $w_.size \
            -text "Size:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -value $size_ \
            -textvariable [scope size_]
      }
      pack $itk_component(size) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(size) {Size of images}
   }

   #  Destructor:
   #  -----------
   destructor  {
      $tempcats_ clear
   }

   #  Methods:
   #  --------

   #  Do the query as a batch job.
   public method query {} {

      #  Query starts, so might want to do something.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) on
      }

      #  Establish object to run the query script.
      if { $querytask_ == {} } {
         set querytask_ [gaia::GaiaForeignExec \#auto \
                            -application $::gaia_dir/querysiap \
                            -notify [code $this query_done_]]
      }
      set votable_ [$tempcats_ get_typed_name ".vot"]
      set interrupted_ 0
      puts "$querytask_ runwith $itk_option(-accessURL) $ra_ $dec_ $size_ $votable_"
      $querytask_ runwith $itk_option(-accessURL) $ra_ $dec_ $size_ $votable_
   }

   #  Interrupt the query.
   public method interrupt {} {
      if { $querytask_ != {} } {
         set interrupted_ 1
         catch {$querytask_ delete_now}
         set querytask_ {}
      }
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }
   }

   #  Called when the query completes.
   protected method query_done_ {} {
      
      puts "query_done"

      #  Immediate notification we're finished.
      if { $itk_option(-feedbackcommand) != {} } {
         eval $itk_option(-feedbackcommand) off
      }

      if { $interrupted_ } {
         info_dialog "Query interrupted"
         return
      }

      #  Check file exists.
      if { ! [::file exists $votable_] } {
         warning_dialog "Failed to query SIAP server"
         return
      }

      #  Convert to TST and do the command to display etc.
      read_query $votable_
   }

   #  Save the result of a query to an external VOTable.
   public method save_query {filename} {
      if { $votable_ != {} && [::file exists $votable_] } {
         ::file copy -force $votable_ $filename
      }
   }

   #  Read the query directly from an existing file.
   public method read_query {filename} {

      puts "read_query: $filename"

      #  Convert to a TST file so we can open it up as usual.
      set vot [gaiavotable::open $filename]
      puts "1"
      set tempname [$tempcats_ get_name]
      puts "2"
      set tst [gaiavotable::save $vot 0 $tempname]
      puts "3"
      gaiavotable::close $vot
      puts "4"

      #  This is the current VOTable now.
      set votable_ $filename
      puts "5"

      #  Do command so that something happens.
      if { $itk_option(-command) != {} } {
         puts "6"
         eval $itk_option(-command) $tempname
      }
      puts "7"
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The SIAP accessURL.
   itk_option define -accessURL accessURL AccessURL {}

   #  Command to execute when a list of images is accepted.
   itk_option define -command command Command {}

   #  Command to execute when batch jobs starts and stops.
   itk_option define -feedbackcommand feedbackcommand FeedBackCommand {}

   #  An astrocat instance for handling the result as a TST.
   itk_option define -astrocat astrocat AstroCat {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Temporary files.
   protected variable tempcats_ {}

   #  Name of the VOTable from query.
   protected variable votable_ {}

   #  Task controlling querys.
   protected variable querytask_ {}

   #  Set true when a query is being interrupted.
   protected variable interrupted_ 0

   #  SIAP RA, Dec and size.
   protected variable ra_ 0.0
   protected variable dec_ 0.0
   protected variable size_ 0.01

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
