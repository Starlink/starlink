   itcl_class Ccd_class {

#+
#  Name:

#  Type of Module:
#     [incr Tcl] class

#  Purpose:

#  Description:

#  Invocations:
#
#        Ccd_class window [-option value]...
#
#     This command create an instance of a "class" and returns a
#     command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -option arguments
#
#     This option configures ... (default is ???)

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the "class " widget with a default configuration,
#	 except when overridden by command line options.
#     destructor
#        Destroys the "class" instance, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     method arguments ....
#        description

#  Inheritance:
#     This class inherits "???" and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     {date} (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-

#  Inheritances:
      inherit ?????

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } { 

#  Create a frame widget. This must have the same name as the class
#  command.
         Ccd_base::constructor

#  Check options database for values to override widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less 
#  specific class.
         set opt [ _getoption "option Option"]
         if { $opt != {} } { set ????? $opt }

#  Set default configurations.
         configure -?????          $????

#  Pack sub-widgets.

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($this:???) $this.????
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Destructor "method delete". Deletes the object. 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Note: only use this if need to do more than just destroy the base object.
#      destructor  {
#         if $exists {
#            destroy $this
#            set exists 1
#         }
#      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration method to change public attributes.
      method configure { config } { }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      public option { default } {
         if $ exists {
	 }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  End of class defintion.
   }

# $Id$
