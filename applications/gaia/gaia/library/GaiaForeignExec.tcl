#+
#  Name:
#     GaiaForeignExec

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for running a "foreign" program in the
#     background.

#  Description:
#     This class defines a object that represents an foreign
#     application. It provides methods for controlling and monitoring
#     the activity of the application

#  Invocations:
#
#        GaiaForeignExec object_name [configuration options]
#
#     This creates an instance of a GaiaForeignExec object. The return is
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
#     should include the full path. This option must be set at
#     sometime before any methods to control the application are used.
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
#        inform_
#
#     Internal method used to process any return messages from the
#     application as it runs.
#

#  Inheritance:
#     This widget inherits no other classes.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     14-SEP-1998 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

class gaia::GaiaForeignExec {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor  {args} {

      #  Create the command queue.
      set command_queue_ [Queue \#auto]

      #  Set trace's on common variables that get the output from the
      #  application.
      trace variable [scope forout_($this)] w [code $this inform_]
      trace variable [scope forerr_($this)] w [code $this error_]

      #  And evaluate any configuration options.
      if { $args != {} } {
         eval configure $args
      }
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Delete scrollbox if it exists.
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

   #  Run the application with the given commands.
   method runwith {args} {
      if { $application != {} } {

         #  And add the command to the execution queue.
         $command_queue_ push "$args"
         if { [$command_queue_ size] == 1 && $application_status_ == {waiting...} } {
            set application_status_ {running...}
            run_next_command_
         }
      }
   }

   #  Run the application. Single string as arguments.
   method runwiths {qual} {
       if { $application != {} } {

         #  And add the command to the execution queue.
         $command_queue_ push "$qual"
         if { [$command_queue_ size] == 1 && $application_status_ == {waiting...} } {
            set application_status_ {running...}
            run_next_command_
         }
      }
   }

   #  Return status of application.
   method status {} {
      return $application_status_
   }

   #  Delete the application when ready.
   method delete_sometime {} {
      set delete_sometime_ 1
   }

   #  Execute the next command on the queue.
   private method run_next_command_ {} {
      set args [$command_queue_ pop]
      if { $args != {}  } {

         #  Run the jobs with the current arguments.
         set forret_($this) {}
         catch {eval "blt::bgexec \[scope forret_($this)\] \
                         -keepnewline 1 \
                         -error \[scope forerr_($this)\] \
                         -update \[scope forout_($this)\] \
                         $application $args"} msg
         if { $msg != "" } {
            info_dialog "$msg"
         }
         command_completed_
      }
   }

   #  Method to tidy up when a command completes. This may delete the
   #  application queue or start the next request. Notification of the
   #  command completion is given, if requested.
   private method command_completed_ {} {
      set queue_size [$command_queue_ size]
      if { $delete_sometime_ && $queue_size == 0 } {
         delete object $this
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

   #  Method to deal with messages on stdout.
   private method inform_ {args} {

      #  If request use a callback to preprocess the output (this
      #  allows the user to clean the output of escape sequences).
      if { $preprocess != {} } {
         set output [$preprocess $forout_($this)]
      } else {
         set output $forout_($this)
      }

      #  Write output from task into window if required.
      if { $show_output_ } {
         if [winfo exists $Scrollbox_] {
            foreach line [split $output "\n"] {
               $Scrollbox_ insert end "$line"
            }
            $Scrollbox_ see end
         } else {
            set show_output_ 0
         }
      }

      #  Keep a copy of the output for parsing.
      append output_ "$output"
   }

   #  Method to deal with messages on stderr.
   private method error_ {args} {
      if { $forerr_($this) != {} } {
         if { $use_error } {

            #  Ordinary message arrive on standard error, just pass on
            #  as if ordinary text.
            set forout_($this) $forerr_($this)
         } else {
            error_dialog "$shortname_: $forerr_($this)"
         }
      }
   }

   #  Method to run a job immediately and wait for completion.
   public method runnow {args} {
      if { $args != {}  } {
         catch {exec $application $args} msg
         if { $msg != "0" && $msg != {} } {
            info_dialog "$msg"
            return 0
         }
      }
      return 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Set the name of the application.
   public variable application {} {
      if { $application != {} && $fullname_ == {} } {
         if { [file executable $application] } {
            set directory [file dirname $application]
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
         set Scrollbox_ [Scrollbox $Top_.output]
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

   #  If the application writes ordinary messages to standard error,
   #  then process these as ordinary output text.
   public variable use_error 0 {}

   #  Command to use if the output from the program needs
   #  preprocessing (say to remove escape sequences).
   public variable preprocess {} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Status of application, running... or waiting...
   protected variable application_status_ {void...}

   #  Name of application without directory info. May only be set once.
   protected variable shortname_ {}

   #  Full name of application. May only be set once.
   protected variable fullname_ {}

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

   #  Common variables (shared between all instances):
   #  ------------------------------------------------
   common forout_
   common forret_
   common forerr_

#  End of class definition.
}

