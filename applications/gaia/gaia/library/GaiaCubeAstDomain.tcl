#+
#  Name:
#     GaiaCubeAstDomain

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Creates a toolbox for changing the domain (i.e. coordinate
#     system) displayed for a data cube.

#  Description:
#     This class creates a window that contains controls for changing
#     to one of the available AST domains in the cube WCS.

#  Invocations:
#
#        GaiaCubeAstDomain object_name [configuration options]
#
#     This creates an instance of a GaiaCubeAstDomain object. The return is
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
#    See itk_option define lines below.

#  Methods:
#     See method definitions below.

#  Inheritance:
#     gaia::GaiaAstDomain

#  Copyright:
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
#     10-DEC-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeAstDomain {}

itcl::class gaia::GaiaCubeAstDomain {

   #  Inheritances:
   #  -------------

   inherit gaia::GaiaAstDomain

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: cube built-in coordinate system ($itk_option(-number))"
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Called after realisation, do any tweaks to the UI.
   public method init {} {
      gaia::GaiaAstDomain::init
      set m [get_menu Help]
      $m entryconfigure "On Window..." \
         -command [code $this show_help cubedomain]
   }

   #  Override these from superclass so we can access the cube WCS, not one
   #  associated with an image display.

   #  Set the current domain.
   protected method set_current_domain_ {iframe} {
      [$itk_option(-gaiacube) get_cubeaccessor] astset "Current=$iframe"
   }

   #  Get the current domain and assign to original_
   protected method get_current_domain_ {} {
      set original_ [[$itk_option(-gaiacube) get_cubeaccessor] astget "Current"]
   }

   #  Get a list of the currently available domains.
   protected method get_domains_ {} {
      set domains_ [[$itk_option(-gaiacube) get_cubeaccessor] astdomains]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of GaiaCube accessing the data, must be defined.
   itk_option define -gaiacube gaiacube GaiaCube {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
