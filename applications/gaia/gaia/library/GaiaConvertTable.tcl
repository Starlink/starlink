#+
#  Name:
#     GaiaConvertTable

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for running a "foreign" program as catalogue conversion filter.

#  Description:
#     This class defines a object that controls a series of defined
#     filters for converting to and from CAT and ASCII supported formats.

#  Invocations:
#
#        GaiaConvertTable object_name [configuration options]
#
#     This creates an instance of a GaiaConvertTable object. The return is
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

#  Methods:

#  Inheritance:
#     This widget inherits no other classes.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-SEP-1998 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

class gaia::GaiaConvertTable {

   #  Inheritances:
   #  -------------
   #  Nothing

   #  Constructor:
   #  ------------
   constructor  {args} {

       #  Set the names of the conversion filters.
       global env
       foreach type $cattypes_ {
          set to_app_($type) "$env(GAIA_DIR)/cat2tab"
          set from_app_($type) "$env(GAIA_DIR)/tab2cat"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
       foreach type $asciitypes_ {
          set to_app_($type) "$env(GAIA_DIR)/asc2tab"
          set from_app_($type) "$env(GAIA_DIR)/tab2asc"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Convert from a CAT/ASCII catalogue to a tab table.
   public method to {in out {now 0}} {

      #  Get the file type so we can invoke the correct filter.
      set type [file extension $in]
      
      if { $to_filter_($type) == {} } {
         global env
         set to_filter_($type) [GaiaForeignExec \#auto \
                                   -application $to_app_($type) \
                                   -show_output 0]
      }

      #  Now attempt the conversion.
      set cmd [format $to_cmd_ $in $out]
      if { $now } {
         catch {eval $to_filter_($type) runnow $cmd} msg
         if { $msg == "1" || $msg == "0" } {
            set msg {}
         }
      } else {
         catch {$to_filter_($type) runwiths $cmd} msg
      }
      if { $msg != {} } {
         return 0
      }
      return 1
   }

   #  Convert a tab table to a CAT/ASCII catalogue.
   public method from {in out {now 0}} {

      #  Get the file type so we can invoke the correct filter.
      set type [file extension $out]

      if { $from_filter_($type) == {} } {
         global env
         set from_filter_($type) [GaiaForeignExec \#auto \
                                     -application $from_app_($type) \
                                     -show_output 0]
      }
      
      #  CAT will not overwrite existing files, so do this ourselves.
      if { [file exists $out] } {
         file delete $out
      }

      #  Now attempt the conversion.
      set cmd [format $from_cmd_ $in $out]
      if { $now } {
         set res [catch {eval $from_filter_($type) runnow $cmd} msg]
         if { $msg == "1" || $msg == "0" } {
            set msg {}
         }
      } else {
         set res [catch {$from_filter_($type) runwiths $cmd} msg]
      }
      if { $msg != {} || $res != 0 } {
         if { $msg == {} } {
            set msg "Failed to convert temporary file: $in back to $out"
         }
         error_dialog "$msg"
         return 0
      }
      return 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of applications that form the input/output filters. These
   #  are indexed by the file types.
   protected variable to_app_
   protected variable from_app_

   #  Known file types.
   protected variable cattypes_ ".fits .FITS .fit .FIT .gsc .GSC .txt .TXT"
   protected variable asciitypes_ ".asc .ASC .lis .LIS"

   #  Command strings to run the convert to/from a tab-table. This
   #  contains "format" specifiers for the input and output names.
   protected variable to_cmd_   "in=%s out=%s accept"
   protected variable from_cmd_ "in=%s out=%s accept"

   #  Names of the objects used to control the filters.
   protected variable to_filter_
   protected variable from_filter_

   #  End of class definition.
}

