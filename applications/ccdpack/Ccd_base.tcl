   itcl_class Ccd_base {

#+
#  Name:
#     Ccd_base

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines the base "superclass" for all compound widgets.

#  Description:
#     This routine defines a series of basic methods and configurations
#     which apply to all compound widgets. It should consequently be
#     inherited by all classes which construct compound widgets. The
#     Ccd_base::constructor command should be used first if a frame
#     widget is required for containing the compound construct.

#  Invocations:
#
#        Ccd_base window
#
#     This form of the command creates a frame widget. This form should
#     not normally be used directly. The Ccd_base::constructor command
#     should be used to create a base frame widget for compounds.
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
#        -borderwidth width
#
#     All widgets are assumed to be based on a underlying frame (shell)
#     widget. This set the borderwidth of this widget (defaults to 2).
#
#        -background colour
#
#     Sets the background colour of the shell widget. Defaults to ""
#     which causes no change.
#
#        -relief  "raised|flat|groove|ridge|sunken"
#
#     Sets the relief of the border. Defaults to flat.
#
#        -takefocus  "1|0|{}"
#
#     Determines if the widget should accept the focus from the <TAB>
#     and <SHIFT-TAB> default bindings. The default is
#     {} which indicates the the built-in procedures tkFocusNext and
#     tkPrevFocus should make their own minds up.

#  Methods:
#     constructor [-option value]...
#        Initiates the default configuration options.
#
#        This method is invoked automatically for all classes that
#        inherit from this class (this happens after the constructor
#        method of the inheriting class unless the constructor of this
#        class is invoked explicitly). If this command is not invoked
#        explicitly (creating a base frame for containing the other
#        widgets), then the compound will require that a frame with the
#        object class is created. See the contructor of this class for
#        how to do this.
#
#     destructor
#        Destroys the base widget and removes the command associated
#        with it from the global scope. Invoked by the "delete" method.
#
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#        is given then the current value of any known option is returned
#        in a form similar (but not identical too) the Tk widget
#        command of the same form (see _report).
#
#     cget option
#        Returns the current value of a configuration option.
#
#     wconfig -option sub-widget value
#        This method is designed to allow access to the primitive widget
#        configuration options (of entries, labels, listboxes etc.),
#        from derived classes. Each class should maintain a list of
#        the names of its primitive widgets in the variables.
#
#           widgetnames()
#
#        Widgetnamess is an array is indexed by names of the form
#        "$oldthis:widget" where widget is a shortened name within the
#        context of the class. The value of this element is the
#        fullname of the widget. So for instance if an entry widget
#        $oldthis.something.entry was  created in a class the entries
#        would be.
#
#           set widgetnames($oldthis:entry) $oldthis.something.entry
#
#        The class library builder can then invoke a configuration of a
#        primitive widget using commands like.
#
#           $compound wconfig -relief entry raised
#
#     _getoption item_names
#        This method queries the option database for any entries which
#        match the given resource. The resources are obtained first
#        using the class and (if this fails) using no class. The
#        arguments to this are all the possible names of the item to be
#        queried. This method is only really intended for use by classes
#        (hence the "_" in name).
#
#     _report
#        This method queries the class about all it public variables
#        (the configuration options) and reports their values in a form
#        similar to the Tk "widget configure" command.
#
#     bind sub-widget event proc
#        This method allows the binding of events in sub-widgets. It
#        uses the same mechanisms as the wconfig method and is otherwise
#        identical to the Tk bind command. The sub-widget is the name of
#        a primitive in a compound.
#
#     focus [command] sub-widget
#        This method allows the setting of the focus into a sub-widget.
#        The possible widget names are stored in the widgetfocus
#        array using "($oldthis:widgetname)". The string "widgetname"
#        should be set a sensible value such as "entry", "listbox"
#        etc. The "command" is optional and should be a known value
#        (i.e. default or none).
#
#     kill top-window
#        This method invokes the delete method for all the meta-widgets
#        related to the given top-level window. It can be invoked from
#        any object in the scope of the top-level window. The names of
#        metawidgets should be registered in the array "metawidgets".
#        Class developers should perform this task by invoking the
#        internal method _register or by using the base constructor method.
#
#     _register name
#        Registers a metawidget, if it is not created via the
#        constructor method.
#
#     sethelp widget docname label
#        Sets up bindings to provide context sensitive help for the named
#        component widget. The name of the document and label are
#        stored for retrival via the showhelp method. If the widget
#        doesn't exist then no bindings are set. The document name and
#        label are stored regardless.
#
#     showhelp widget
#        Returns the document and label name for any help associated
#        with the named component widget. Note the widget name may not
#        belong to an actual widget. This is allowed for storing help
#        information related to parts of a widget. In this case a
#        showhelp method occluding this one should be provided.

#  Inheritance:
#     This widget inherits no classes and is the base superclass for all
#     compound widgets.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     19-MAR-1994 (PDRAPER):
#        Original version.
#     25-MAR-1994 (PDRAPER):
#        Finally understand constructors and destructors.
#     28-MAR-1994 (PDRAPER):
#        Added widget traversal and focus methods.
#     22-MAR-1995 (PDRAPER):
#        Added context sensitive help support.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed.
#     9-MAY-1995 (PDRAPER):
#        Added takefocus configuration option.
#     1-JUN-1995 (PDRAPER):
#        Removed keyboard traversal. This is now controlled by Tk.
#     30-JUN-1995 (PDRAPER):
#        Reorganised the sub-widget registration and made destruction
#        via the kill method a instance responsibility (for speed).
#     {enter_further_changes_here}

#-

#  Inherits nothing (base superclass)

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction perform initialisation of the fundermental configuration
#  options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Create a frame widget for containing the derived widget. This must
#  have the same class as the object.
         set oldthis [info namespace tail $this]
         if { ! [ winfo exists $oldthis ] } {
            set class [info namespace tail [info class]]
            if { $class == "Ccd_toplevel" } {
               toplevel  $oldthis          -class $class
            } else {
               frame  $oldthis          -class $class
            }
         }
         set exists 1

#  Check options database for widget defaults. Look for more
#  specific option of having a class specified, if this fails try for less
#  specific class.
         set opt [ _getoption "borderwidth borderWidth BorderWidth"]
         if { $opt != {} } { set borderwidth $opt }
         set opt [ _getoption "relief Relief"]
         if { $opt != {} } { set relief $opt }
         set opt [ _getoption "background backGround BackGround"]
         if { $opt != {} } { set background $opt }
         set opt [ _getoption "takefocus takeFocus TakeFocus"]
         if { $opt != {} } { set takefocus $opt }

#  Configuration options.
         configure -borderwidth     $borderwidth
         configure -relief          $relief
         configure -background      $background
         configure -takefocus       $takefocus

#  Register this object as a meta-widget (used by kill). Note we use
#  the namespace qualified version.
         _register $this
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Destructor "method delete". Deletes the object.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      destructor  {
         if $exists {
            destroy $oldthis
            set exists 0
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration method to change public attributes (inherited by all
#  widgets).
      method config { args } { eval $oldthis configure $args }
      method configure { config } {
         if { $config == {} } {
            _report
         } else {

#  Convert first word into lower case to catch all similar occurences of
#  the configuration option.
            set config [lreplace $config 0 0 [string tolower [lindex $config 0]]]
         }
      }

#  Method for configuring constituent widgets. The known widgets and
#  their commands are stored in the common variables widgetnames.
      method wconfig { option basewidget value } {

#  Check for known sub-widgets.
         if { [info exists widgetnames($oldthis:$basewidget)] } {
            set widget $widgetnames($oldthis:$basewidget)
            if { [ winfo exists $widget ] } {
               $widget configure $option $value
            }
         } else {
            error "Unknown sub-widget \"$basewidget\""
         }
      }

#  Method for determining if a possible user preference (via the option data
#  base) has been made. Look for a class based value
      method _getoption names {
         set value {}
         if { $names != {} } {

#  Use class based option.
            set class [info namespace tail [info class]]
            foreach oneof $names {
               set value [option get $oldthis $oneof $class]
               if { $value != {} } {
                  break
               }
            }

#  No value, so try less specific method without class information.
            if { $value != {} } {
               foreach oneof $names {
                  set value [option get $oldthis $oneof {} ]
                  if { $value != {} } {
                     break
                  }
               }
            }
         }
         return $value
      }

#  Method for adding a binding to a widget. Actual binding occurs at global
#  scope.
      method bind { basewidget event proc } {
         if $exists {

#  Check for known sub-widgets.
            if { [info exists widgetnames($oldthis:$basewidget)] } {
               set widget $widgetnames($oldthis:$basewidget)
               if { [ winfo exists $widget ] } {
                  ::bind $widget "$event" "$proc"
               }
            } else {
               error "Unknown sub-widget \"$basewidget\""
            }
         }
      }

#  Method for reporting configuration.
      method _report {} {
         set variables [ $oldthis info public ]
         set sendlist ""
         foreach oneof $variables {
            set current [ $oldthis info public $oneof -value ]
            set init    [ $oldthis info public $oneof -init ]
            set first [string first :: $oneof]
            incr first +2
            set option [string range $oneof $first end]
            lappend sendlist "-$option $oneof $oneof $init $current "
         }
         return $sendlist
      }

#  Method for returning the value of a configuration option.
      method cget { var } {
         if { $var != "" } {
            regsub {\-} $var "" clean
            set value [ $this info public $clean -value ]
            return $value
         } else {
            error "wrong \#args: should be $this cget option"
         }
      }

#  Set the focus into a widget.
      method focus { args } {
         if $exists {

#  Window name if last element of args (which may also contain the
#  "default" and "none" qualifiers).
            set last [ llength $args ]
            incr last -1
            set basewidget [ lindex $args $last ]
            if { $last != 0 } {
               set command [ lindex $args 0 ]
            } else {
               set command {}
            }

#  Check for known sub-widgets.
            if { [info exists widgetnames($oldthis:$basewidget)] } {
               set widget $widgetfocus($oldthis:$basewidget)
               if { [ winfo exists $widget ] } {
                  if { $command != {} } {
                     ::focus $command $widget
                  } else {
                     ::focus $widget
                  }
               }
            } else {
               error "Unknown sub-widget \"$basewidget\" "
            }
         }
      }

#  Define method to destroy all objects which are related. This is
#  defined as being a child of a given top-level window. Destroy these
#  in the reverse order to which they were created. Note not all widgets
#  may be availble in this namespace (if they are metawidgets created by
#  other metawidgets!) so use catch and leave destructors
#  the real job of deleting these.
      method kill topwin {
         for { set i $nobjects } { $i > 0 } { incr i -1 } {
            set widget $metawidgets($i)
	    set window [info namespace tail $widget]
            if { [ winfo exists $window ] } {
               if { [ winfo toplevel $window ] == "$topwin"} {
                  catch {::delete object $widget}
               }
            }
         }
      }

#  Define method to add new object to metawidget register.
      method _register object {
         incr nobjects
         set metawidgets($nobjects) $object
      }

#  Define method for setting the document elements associated with a
#  component widget. The bindings are also set for all as the focus
#  may not be in this widget when help is asked for. All will not work
#  by itself as all keyboard input can be trapped by some widgets at
#  the Class or widget specific levels. Always record helpinfo even if
#  widget doesn't exist at this stage (it might later or may be a name
#  for storing help for use in some other methods).
      method sethelp { widget docname label } {
         if { [winfo exists $widget] } {
            ::bind $widget <Any-F1>   "CCDContextHelp %X %Y;break"
            ::bind $widget <Any-F2>   "CCDContextHelp %X %Y;break"
            ::bind $widget <Any-Help> "CCDContextHelp %X %Y;break"
            ::bind all <Any-F1>   "CCDContextHelp %X %Y"
            ::bind all <Any-F2>   "CCDContextHelp %X %Y"
            ::bind all <Any-Help> "CCDContextHelp %X %Y"
         }
         set helpinfo($widget) "$docname $label"
      }

#  Method to locate and return any help information about a named
#  component widget.
      method showhelp { widget } {
         if { [info exists helpinfo($widget)] } {
            return "$helpinfo($widget)"
         } else {

#  No help for this widget. Is it covered by help about the
#  meta-widget that it is part of? Or failing this is there help
#  at the top-level?
            if { [info exists helpinfo($oldthis)] } {
               return "$helpinfo($oldthis)"
            } else {

#  Look for top-level help (default for this form).
               set top [winfo toplevel $oldthis]
               if { $top != "" } {
                  if { [info exists helpinfo($top)] } {
                     return "$helpinfo($top)"
                  }
               }

#  No help at all.
               return ""
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Width of the widget border.
      public borderwidth 2 {
         if $exists {
            $oldthis configure -borderwidth $borderwidth
         }
      }

#  Relief of border.
      public relief flat {
         if $exists {
            $oldthis configure -relief $relief
         }
      }

#  Colour of background.
      public background {} {
         if $exists {
            if { $background != {} } {
               $oldthis configure -background $background
            }
         }
      }

#  Should widget take the focus?
      public takefocus 0 {
         if $exists {
            if { $takefocus != {} } {
               $oldthis configure -takefocus $takefocus
            }
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      protected exists 0

#  Set common widget list for dealing with configure and binding
#  requests for constituent widgets. These should be set in the classes
#  which construct a compound widget. The widgetfocus array is an array
#  of window names of widgets that may recieve the focus.
      common widgetnames
      common widget
      common widgetfocus

#  Number and names of any metawidgets.
      common metawidgets
      common nobjects 0

#  Document information for context sensitive help.
      common helpinfo

#  Old name of widget (pre itcl2.0).
      protected oldthis

#  End of class definition.
   }
# $Id$
