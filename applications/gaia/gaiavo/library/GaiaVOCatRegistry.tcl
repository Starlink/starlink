#+
#  Name:
#     GaiaVOCatRegistry

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Query a (the) VO registry for services.

#  Description:
#     Extends the GaiaVOCat class to query the NVO registry for services
#     of a given type.

#  Invocations:
#
#        GaiaVOCatRegistry object_name [configuration options]
#
#     This creates an instance of a GaiaVOCatRegistry object. The return is
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

itk::usual GaiaVOCatRegistry {}

itcl::class gaiavo::GaiaVOCatRegistry {

   #  Inheritances:
   #  -------------
   inherit gaiavo::GaiaVOCat

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args
      wm title $w_ "Query VO Registry for services"
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------

   #  XXX debug method, remove before release (or make last query
   #  persistent and reload that).
   public method init {} {
      GaiaVOCat::init
      puts "debug: loading siap_query.vot..."
      after idle [code $itk_component(registry) read_query siap_query.vot]
   }

   #  Add the component that will control the registry query.
   protected method add_query_component_ {} {

      #  Registry searches for now.
      itk_component add registry {
         gaiavo::GaiaVORegistrySearch $w_.registry \
            -feedbackcommand  [code $this set_feedback] \
            -astrocat [code $w_.cat] \
            -command [code $this query_done] \
            -query_cmd [code $this query]
      }
      pack $itk_component(registry) -side top -fill x
      add_short_help $itk_component(registry) {Controls to set registry query}

      set query_component_ $itk_component(registry)
   }

   #  Open a service, "args" is a list of values from a row of the current
   #  table.
   protected method open_service_ {args} {
      if { $itk_option(-activate_cmd) != {} } {
         set headings [$itk_component(results) get_headings]
         eval $itk_option(-activate_cmd) "\$headings" "\$args"
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

}
