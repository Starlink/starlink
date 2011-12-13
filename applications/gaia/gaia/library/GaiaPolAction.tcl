#+
#  Name:
#     GaiaPolAction

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     A GaiaPolAction which describes an undo-able action.

#  Description:
#     This class implements a type of GaiaPolObject which contains a
#     description of an action which can be un-done or re-done, typically
#     using the "Undo" and "Redo" items in the Edit menu.
#
#  Invocations:
#
#        GaiaPolAction object_name [configuration options]
#
#     This creates an instance of a GaiaPolAction object. The returned value
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
#        getType{}: Return the action type
#        getDesc{}: Return the action description.

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

itcl::class gaia::GaiaPolAction {

#  Inheritances:
#  =============
   inherit gaia::GaiaPolObject

#  Constructor:
#  ============
#  "$type" is a string which GaiaPolarimetry uses to identify the class of
#  action. "$desc" is a human readable description of the action. "$obj"
#  is a reference to an object associated with the action. "$ucmd" is a
#  string describing a method to be invoked on $obj (with any required
#  arguments) which will undo the action and "$rcmd" is a method to be
#  invoked on $obj which will redo the action. $obj, $ucmd and $rcmd are
#  usually only used if $type is "object".
#  -----------------------------------------------------------------------
   constructor {type desc {obj ""} {ucmd "" } {rcmd "" } } {

#  Now initialize the class data. If this constructor has been invoked
#  to construct the base class part of some super class, do not
#  initialize the data since this will be done as a consequence of
#  initializeing the super class data.
      if { [$this info class] == "::gaia::GaiaPolAction" } {
         init $type $desc $obj $ucmd $rcmd
      }
   }

#  Destructor:
#  ===========
#  none

#  Initializer:
#  ============
#  Override the parent Init method to initialise the contents of the
#  memory allocated by the GaiaPolAction constructor using a user-supplied
#  argument list.
   protected method init {type desc obj ucmd rcmd} {

#  First initialize the parent class data
      gaia::GaiaPolObject::init

#  Now initialize this class.
      variable type_ $type
      variable desc_ $desc
      variable obj_ $obj
      variable ucmd_ $ucmd
      variable rcmd_ $rcmd

   }

#  Public methods:
#  ===============

#  Undo the action
#  ---------------
   public method undo {} {
      if { $obj_ != "" && $ucmd_ != "" } {
         eval $obj_ $ucmd_
      }
   }

#  Redo the action
#  ---------------
   public method redo {} {
      if { $obj_ != "" && $rcmd_ != "" } {
         eval $obj_ $rcmd_
      }
   }

#  Convert to a string
#  -------------------
   public method toString {} {
      return "GaiaPolAction type=$type_ desc=\"$desc_\""
   }

#  Accessor methods
#  ----------------
   public method getType {} { return $type_ }
   public method getDesc {} { return $desc_ }

#  Protected methods:
#  ==================

#  Private methods:
#  ==================

#  Public data members:
#  ====================

#  Protected data members:
#  =======================
   protected {

#  The action type
      variable type_ ""

#  The action description
      variable desc_ ""

#  The associated object
      variable obj_ ""

#  The undo command for the associated object
      variable ucmd_ ""

#  The redo command for the associated object
      variable rcmd_ ""

   }

#  Private data members:
#  =====================
#  (none)

#  Common (i.e. static) data members:
#  ==================================

#  End of class definition.
}
