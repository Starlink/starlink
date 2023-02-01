#+
#  Name:
#     GaiaApp

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for controlling an application.

#  Description:
#     This class defines a object that represents an application. It
#     provides methods for controlling the activity of the application
#     and associates it with the correct monolith.
#
#     More than one command can be sent to the application, extra
#     commands are queued until they can be executed. The status of
#     the application can be "void...", if the application isn't
#     initialised yet, "waiting..." if the application isn't running
#     and "running..." when active.

#  Invocations:
#
#        GaiaApp object_name [configuration options]
#
#     This creates an instance of a GaiaApp object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        application full_path_name
#
#     This option sets the name of the application to be run. The name
#     should include the full path so that the associated monolith can
#     be located. This option must be set at sometime before any
#     methods to control the application are used.
#
#        show_output
#
#     If true then a scrolled listbox is created and any output from
#     the application is shown. If set to the name of a widget then
#     the output is directed into that (unless it no longer exists).

#  Methods:
#
#        status
#
#     Returns the current status of the application. If the
#     application name is undefined then this is "void...", otherwise
#     it is "running..." or "waiting...".
#
#        runwith args
#        runwiths qual
#
#     This runs the application, passing it the given args as program
#     parameters. You may add new run commands before the previous one
#     has completed. The runwiths form just accepts a single string
#     as argument (this can be useful when it is difficult to form a
#     well behaved list).
#
#        run_next_command_
#
#     Internal method used to run the next command in the queue.
#
#        delete_sometime
#
#     If invoked this method will cause the application to end when
#     the current commands are completed. This should always be used
#     in preference to the inbuilt "delete" command.
#
#        command_completed_
#
#     Private method that is invoked when a command is completed.
#
#        inform_ message
#
#     Internal method used to process any return messages from the
#     application as it runs.
#
#        start_monolith_
#
#     Internal method to start the monolith associated with the
#     application.
#
#        stop_monolith_
#
#     Internal method to stop the monolith. This only happens when all
#     applications using the monolith are deleted.
#
#        set_monolith_ name
#
#     Internal method to initialise the monolith status.

#  Inheritance:
#     This widget inherits no other classes.

#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA
#     Copyright (C) 2006 Particle Physics and Astronomy Research Council

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-APR-1996 (PWD):
#        Original version.
#     9-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     5-MAR-1999 (PWD):
#        Changed name in error messages to reflect the command.
#     5-MAR-2001 (DSB):
#        Added public method getparam.
#     9-MAR-2001 (PWD):
#        Renamed GaiaApp from GaiaPolApp (and StarApp before that).
#     7-FEB-2006 (PWD):
#        Removed adamtask from GAIA. Now uses StarTcl version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaApp {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor  {args} {

      #  Create the command queue.
      set command_queue_ [gaia::Queue \#auto]

      #  And evaluate any configuration options.
      if { $args != {} } {
         eval configure $args
      }
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Stop the monolith if running (and no other applications need it).
      if { $monolith_ != {} } {
         stop_monolith_
      }
      #  Delete scrollbox of output.
      if { [winfo exists $Scrollbox_] } {
         destroy $Scrollbox
      }

      #  And top-level widget.
      if { [winfo exists $Top_] } {
         destroy $Top_
      }
   }

   #  Methods:
   #  --------

   #  Get the value of an application parameter.
   public method getparam {pname} {
      set ret ""
      if { $application != {} && $pname != "" } {

         #  Make sure monolith is running.
         start_monolith_

         #  Construct the full parameter specificiation.
         set parspec "$shortname_:$pname"

         #  Initiate a request for the parameter value. The code specified
         #  by public variable parnotify is executed when the request is
         #  completed.
         $monotask_($monolith_) get $parspec \
            -getresponse [code $this got_param_ $pname %V]

      }
      return $ret
   }

   #  Run the application with the given commands.
   public method runwith {args} {
      if { $application != {} } {

         #  Make sure monolith is running.
         start_monolith_

         #  And add the command to the execution queue.
         $command_queue_ push "$args"
         if { [$command_queue_ size] == 1 && $application_status_ == {waiting...} } {
            set application_status_ {running...}
            run_next_command_
         }
      }
   }

   #  Run the application. Single string as arguments.
   public method runwiths {qual} {
      if { $application != {} } {

         #  Make sure monolith is running.
         start_monolith_

         #  And add the command to the execution queue.
         $command_queue_ push "$qual"
         if { [$command_queue_ size] == 1 && $application_status_ == {waiting...} } {
            set application_status_ {running...}
            run_next_command_
         }
      }
   }


   #  Return status of application.
   public method status {} {
      return $application_status_
   }

   #  Delete the application when ready. Do it now if nothing is queued.
   public method delete_sometime {} {
      set delete_sometime_ 1
      if { [$command_queue_ size] == 0 &&
           $application_status_ != {running...} } {
         catch {delete object $this}

         #  Make sure any vwaits are released.
         set application_status_ $application_status_
      }
   }

   #  Execute the next command on the queue. Note status is set.
   protected method run_next_command_ {} {
      set args [$command_queue_ pop]
      if { $args != {}  } {
         $monotask_($monolith_) obey $shortname_ "$args" \
            -endmsg [code $this command_completed_] \
            -inform [code $this inform_ %V] \
            -paramreq "$monotask_($monolith_) paramreply %R !!"
      }
   }

   #  Method to deal with the parameter value obtained message.
   protected method got_param_ {param val} {
      if { $parnotify != {} } {
         eval $parnotify $param $val
      }
   }

   #  Method to deal with the command completed message.
   protected method command_completed_ {} {
      set queue_size [$command_queue_ size]
      if { $delete_sometime_ && $queue_size == 0 } {
         catch {delete object $this}
      }

      #  If the command queue isn't empty then run the next command.
      if { $queue_size != 0 } {
         set application_status_ {running...}
         run_next_command_
      } else {
         set application_status_ {waiting...}
      }

      #  Issue notify command if it exists.
      if { $notify != {} } {
         eval $notify
      }
   }

   #  Method to deal with the inform return of a task.
   protected method inform_ {output} {

      #  Check for an error. Note this is start of the error (!!), so
      #  output_ isn't complete yet, so we wait for application_status_
      #  to change, should be done by the command_completed_ method.
      if { [string range $output 0 1 ] == "!!" } {
         ::vwait [scope application_status_]
         error_dialog "$output$output_"
	 puts stderr "Error running $shortname_:\n$output$output_"
         set output_ {}
      }

      #  Write output from task into window if required.
      if { $show_output_ } {
         if [winfo exists $Scrollbox_] {
            $Scrollbox_ insert end "$output"
            if { $see_end } {
               $Scrollbox_ see end
            }
         } else {
            set show_output_ 0
         }
      }

      #  Keep a copy of the error output.
      if { [string range $output 0 0 ] == "!" } {
         append output_ "\n$output"
      }
   }

   #  Start monolith.
   protected method start_monolith_ {} {

      #  Check if the monolith is already loaded and running.
      set start 1
      if { [info exists monotask_($monolith_)] } {
         set start 0
         if { [$monotask_($monolith_) path] == 0 } {
            set start 1
         }
      }
      if { $start } {
         #  Either not running or failed so start it up.  Need new
         #  task name to avoid clashes with other (previous) instances
         #  of the monolith, so append a unique integer to the monolith
         #  filename (no directory or file type).
         set taskname "[file rootname [file tail $monolith_]][incr monocount_]"
         ::adamtask $taskname $monolith_
         set count 0
         while { [$taskname path] == 0 } {
            after 250
            incr count
            if { $count > 200 } {
               puts stderr \
                  "Failed to load the application ($application) monolith ($monolith_)."
               $taskname kill
               catch {delete object $this}
            }
         }
         set monotask_($monolith_) $taskname
      }
   }

   #  Stop monolith if reference count is now zero.
   protected method stop_monolith_ {} {
      incr monorefcount_($monolith_) -1
      if { $monorefcount_($monolith_) == 0 } {
         $monotask_($monolith_) kill
         unset monostatus_($monolith_)
         unset monorefcount_($monolith_)
         unset monotask_($monolith_)
         set monolith_ {}
      }
   }

   #  Initialise monolith.
   protected method set_monolith_ {name} {
      set monolith_ $name
      if { ! [info exists monostatus_($name)] } {
         set monostatus_($name) 0
      }
      if { ! [info exists monorefcount_($name)] } {
         set monorefcount_($name) 1
      } else {
         incr monorefcount_($name)
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Set the name of the application and find the associated
   #  monolith, if one exists, if not pretend application is a
   #  monolith .
   public variable application {} {
      if { $application != {} && $fullname_ == {} } {
         if { [file executable $application] } {
            set directory [file dirname $application]

            #  Monolith is assumed to be at the end of the
            #  softlink. Note this may be a relative name so add the
            #  directory information.
            if { [catch {set monolith [file readlink $application] } ] } {
               set_monolith_ "$application"
            } else {
               if { [string index $monolith 0] != "/" } {
                  set monolith "$directory/$monolith"
               }
               set_monolith_ "$monolith"
            }
            set shortname_ [file tail $application]
            set fullname_ $application
            set application_status_ {waiting...}
         } else {

            # Application does not exist.
            error "The application \"$application\" does not exist."
            return 0
         }
      } elseif { $application != {} } {

         #  Cannot set application more than once, just revert to old name.
         set application $fullname_
      }
   }

   #  Whether to show output of the application in a window or not.
   public variable show_output {0} {
      if { $show_output == "1" } {
         set show_output_ 1
         set Top_ [TopLevelWidget .\#auto]
         wm title $Top_ "Output from application \"$this\""
         set Scrollbox_ [gaia::Scrollbox $Top_.output]
         pack $Scrollbox_ -fill both -expand true
      } elseif { [winfo exists $show_output] } {
         set Scrollbox_ $show_output
         set show_output_ 1
      } elseif { [winfo exists $Scrollbox_] } {
         destroy $Scrollbox_
         destroy $Top_
         set Scrollbox_ {}
      }
   }

   #  Notify command to be executed when the application completes
   #  (note this is executed once after each job in the queue
   #  completes).
   public variable notify {} {}

   #  Notify command to be executed when a parameter value has been
   #  obtained. The command should accept two arguments; the parameter
   #  name and the parameter value.
   public variable parnotify {} {}

   #  Whether to always make sure that end of output inserted
   #  into scrollbox is visible.
   public variable see_end 0 {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Status of application, running... or waiting...
   protected variable application_status_ {void...}

   #  Name of application without directory info. May only be set once.
   protected variable shortname_ {}

   #  Full name of application. May only be set once.
   protected variable fullname_ {}

   #  Name of monolith.
   protected variable monolith_ {}

   #  Whether to delete application when finished.
   protected variable delete_sometime_ 0

   #  The name of the command queue.
   protected variable command_queue_

   #  All the output from the application.
   protected variable output_ {}

   #  Scrollbox to contain output from application.
   protected variable Scrollbox_ {}

   #  Name of top level widget.
   protected variable Top_ {}

   #  Whether to show the output or not.
   protected variable show_output_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Names and states of any monoliths associated with all
   #  applications. Monolith is killed when last application
   #  associated with it is destroyed.

   #  Status of monolith, 1 if running, 0 if available. Index
   #  is name of monolith.
   common monostatus_

   #  Reference count of objects dependent on monolith. Index is name
   #  of monolith.
   common monorefcount_

   #  Task name given to the monolith (used when sending messages to
   #  monolith).
   common monotask_

   #  Count for monoliths started.
   common monocount_ 0

#  End of class definition.
}
