#+
#  Name:
#     GaiaPolList

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Implements an extendable list with a built-in iterator for storing
#     GaiaPolObjects.

#  Description:
#     This class implements an extendable list of GaiaPolObjects, in which
#     one entry is nominated as the current entry.
#
#  Invocations:
#
#        GaiaPolList object_name [configuration options]
#
#     This creates an instance of a GaiaPolList object. The returned value
#     is the name of the object.
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
#     See the "itk_option define" declarations below.

#  Methods:
#     Public:
#        last{}: Is the current entry the last entry?
#        first{}: Is the current entry the first entry?
#        next{}: Increments the index of the current entry if it is not
#                already at the last entry.
#        prev{}: Decrements the index of the current entry if it is not
#                already at the first entry.
#        get{}:  Returns a clone of the current entry GaiaPolObject, or a
#                blank string if the list is empty.
#        add{ GaiaPolObject }: Truncates the list at the current object,
#                and then appends a clone of the supplied GaiaPolObject
#                to the end of the list. The new entry becomes the current
#                entry.
#        getCurrent{}: Returns the index of the current entry, or -1 if
#                the list is empty.
#        reset{}: Empties the list.
#        setMax{max}: Set the max. no. of objects which can be stored in
#                the list. Once the list is full, adding a new object
#                causes te oldest object to be removed from the list.
#                A blank value (the default) implies no limit.
#        getMax{}: Returns the max. no. of objects allowed in the list.
#     Protected:
#        getStack{}: Returns a Tcl list containing the names of all the
#                GaiaPolObjects in the list, in the order in which they were
#                added.

#  Inheritance:
#     ::gaia::GaiaPolObject

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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

#  Authors:
#     DSB: David S. Berry  (STARLINK)
#     {enter_new_authors_here}

#  History:
#     15-JUN-2000 (DSB):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaPolList {

#  Inheritances:
#  =============
   inherit gaia::GaiaPolObject

#  Constructor:
#  ============
   constructor {} {

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolList" } {
         init
      }
   }

#  Destructor:
#  ============
   destructor {

#  Annull the references to the GaiaPolObjects stored in the stack.
      foreach obj $stack_ {
         catch { $obj annull }
      }
   }

#  Initializer:
#  ============
#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolList constructor using a user-supplied
#  argument list.
   protected method init {args} {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class.
      set current_ -1
      set stack_ ""
      set mxlen_ ""

   }

#  Public methods:
#  ===============
#  Format for display.
#  --------------------
   public method toString {} {
      set ret "GaiaPolList:\n"
      for {set i 0} {$i < [llength $stack_] } {incr i} {
         if { $i == $current_ } {
            append ret "--> "
         } else {
            append ret "    "
         }
         append ret [eval [lindex $stack_ $i] toString] "\n"
      }
      return $ret
   }

#  Adds a new entry into the stack. The current stack is truncated to
#  remove any entries above the "current" entry, and the new value is
#  then appended to the stack, and becomes the "current" entry. A clone
#  of the supplied GaiaPolObject is taken and stored in the stack.
#  ------------------------------------------------------------------
   public method add { object } {

#  Do nothing if the supplied string is not the name of an existing
#  GaiaPolObject (or derived class).
      if { ![catch {{*}$object isa ::gaia::GaiaPolObject} rc] } {
         if { $rc } {

#  If the stack is currently empty, just initialise the stack to hold
#  a cloned reference for the supplied GaiaPolObject.
            if { $current_ < 0 } {
               set stack_ [list [{*}$object clone]]
               set current_ 0

#  Otherwise...
            } else {

#  If the list is full, remove the oldest member, and correct the current
#  index.
               if { $mxlen_ != "" && $current_ + 1 >= $mxlen_ } {
                  set stack_ [lreplace $stack_ 0 0]
                  incr current_ -1
               }

#  Modify the index of the current entry so that it will refer
#  to the new entry.
               incr current_

#  If there are entries above the original current entry, first annul the
#  object references, and then truncate the list to remove the references.
               if { $current_ < [llength $stack_] } {
                  foreach obj [lrange $stack_ $current_ end] {
                     $obj annull
                  }
                  set stack_ [lreplace $stack_ $current_ end]
               }

#  Append a clone of the supplied object to the end of the stack.
               lappend stack_ [{*}$object clone]
            }
         }
      }
   }

#  Returns a cloned reference for the "current" entry in the stack, or a
#  null string if the stack is empty.
#  ------------------------------------------------------------------
   public method get {} {
      if { $current_ < 0 } {
         set ret ""
      } else {
         set ret [ [lindex $stack_ $current_] clone]
      }
      return $ret
   }

#  Return 1 if there are no entries after the current entry, and zero
#  otherwise.
#  ------------------------------------------------------------------
   public method last {} {
      set ret 1
      if { $current_ >= 0 } {
         if { $current_ < [llength $stack_] - 1 } {
            set ret 0
         }
      }
      return $ret
   }

#  Return 1 if there are no entries before the current entry, and zero
#  otherwise.
#  ------------------------------------------------------------------
   public method first {} {
      if { $current_ > 0 } {
         set ret 0
      } else {
         set ret 1
      }
      return $ret
   }

#  This method increments the index of the current entry in the stack
#  so long as the current entry is not already the last entry. It returns
#  1 if the current enry was changed, and zero otherwise.
#  ------------------------------------------------------------------
   public method next {} {
      if { ![last] } {
         incr current_
         set ret 1
      } else {
         set ret 0
      }
      return $ret
   }

#  This method decrements the index of the current entry in the stack
#  so long as the current entry is not already the first entry. It returns
#  1 if the current enry was changed, and zero otherwise.
#  ------------------------------------------------------------------
   public method prev {} {
      if { ![first] } {
         incr current_ -1
         set ret 1
      } else {
         set ret 0
      }
      return $ret
   }

#  Return the index of the current entry.
#  --------------------------------------
   public method getCurrent {} { return $current_ }

#  Empty the list.
#  ----------------
   public method reset {} {
      foreach obj $stack_ {
         $obj annull
      }
      set current_ -1
      set stack_ ""
   }

#  Accessor methods.
#  -----------------
   public method getMax {} { return mxlen_ }
   public method setMax { max } { set mxlen_ $max }

#  Protected methods:
#  ==================
   public method getStack {} { return $stack_ }

#  Private methods:
#  ==================

#  Public data members:
#  ====================

#  Protected data members:
#  =======================
   protected {

#  Index within the "stack_" list, of the current entry.
      variable current_ -1

#  A list containing the object names or values in the stack.
      variable stack_ ""

#  The maximum number of objects to allow in the stack.
      variable mxlen_ ""

   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  End of class definition.
}
