#+
#  Name:
#     OptionDialog

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Extend DialogWidget to add additional checkbutton.

#  Description:

#     This class extends DialogWidget to add a checkbutton
#     below the main choice buttons. This checkbutton is expected to
#     add functionality that say allows the user to request that this
#     sort of information is not shown again. Control of the
#     checkbutton state is provided using a command (given by the
#     user) that should be executed when the checkbutton state is
#     toggled. The initial state and text of the checkbutton are given
#     by the option -check_state and -check_text. The command is
#     -check_cmd. 

#  Invocations:
#
#        OptionDialog object_name [configuration options]
#
#     This creates an instance of a OptionDialog object. The return is
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

#  Configuration options:
#     See itk_option definitions below

#  Methods:
#     See method declarations below.

#  Inheritance:
#     DialogWidget

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     23-MAY-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

usual OptionDialog {}

itcl::class gaia::OptionDialog {
   
   #  Inheritances:
   #  -------------
   inherit util::DialogWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      
      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Add the check button.
      itk_component add check {
         checkbutton $w_.check \
            -text "$itk_option(-check_text)" \
            -variable [scope itk_option(-check_state)] \
            -onvalue 1 \
            -offvalue 0 \
            -command [code $this do_check_cmd_]
      }
      pack $itk_component(check) -side bottom -fill x
   }   

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Execute the check_cmd option if defined.
   protected method do_check_cmd_ {} {
      if { $itk_option(-check_cmd) != {} } {
         eval $itk_option(-check_cmd) $itk_option(-check_state)
      }
   }


   #  Configuration options: (public variables)
   #  ----------------------
   
   #  Initial state of checkbutton (0 or 1).
   itk_option define -check_state check_state Check_state 0

   #  Text describing the checkbutton function.
   itk_option define -check_text check_text Check_text {}

   #  Command to execute when the checkbutton state is changed.
   itk_option define -check_cmd check_cmd Check_cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
