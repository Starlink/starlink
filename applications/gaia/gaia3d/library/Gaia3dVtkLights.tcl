#+
#  Name:
#     Gaia3dVtkLights

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Controls for changing the lighting of a scene.

#  Description:
#     Uses the vtkLightKit associated with a window to control the
#     lighting.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     04-JUN-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dVtkLights {}

itcl::class ::gaia3d::Gaia3dVtkLights {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA3D: Lighting ($itk_option(-number))"

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Set the close menu item.
      $File add command -label Close \
         -command [code $this close] \
        -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Main controls.
      add_controls_

      #  Action bar.
      itk_component add actionframe {
         frame $w_.actions
      }
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5


      #  Luminance.
      itk_component add maintlum {
         checkbutton $itk_component(actionframe).maintlum \
            -text "Maintain luminance" \
            -variable [scope maintain_luminance_] \
            -command [code $this set_maintain_luminance_]
      }

      #  Reset.
      itk_component add reset {
         button $itk_component(actionframe).resetx -text "Reset" \
            -command [code $this apply_defaults_]
      }

      #  Close the window.
      itk_component add close {
         button $itk_component(actionframe).close -text "Close" \
            -command [code $this close]
      }

      pack $itk_component(maintlum) $itk_component(reset) \
         $itk_component(close) -side left -expand 1 -padx 3 -pady 3

      #  Apply he initial defaults.
      apply_defaults_
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods and procedures:
   #  -----------------------

   #  Close this window.
   public method close {} {
      wm withdraw $w_
   }

   #  Add main controls.
   private method add_controls_ {} {

      #  Frame for all.
      itk_component add container {
         frame $w_.container
      }
      set lwidth 12

      #  Main controls. Intensities for the various lights.
      itk_component add intfrm {
         labelframe $itk_component(container).intfrm \
            -text "Intensities and ratios:"
      } {}

      itk_component add keyint {
         gaia::LabelScale $itk_component(intfrm).int \
            -text "Key light:" \
            -labelwidth $lwidth \
            -from 0 \
            -to 3 \
            -resolution 0.05 \
            -command [code $this set_prop KeyLightIntensity]
      }
      itk_component add fillrat {
         gaia::LabelScale $itk_component(intfrm).fillrat \
            -text "Fill light ratio:" \
            -labelwidth $lwidth \
            -from 2 \
            -to 10 \
            -resolution 0.1 \
            -orient horizontal \
            -command [code $this set_prop KeyToFillRatio]
      }
      itk_component add headrat {
         gaia::LabelScale $itk_component(intfrm).headrat \
            -text "Head light ratio:" \
            -labelwidth $lwidth \
            -from 2 \
            -to 15 \
            -resolution 0.1 \
            -orient horizontal \
            -command [code $this set_prop KeyToHeadRatio]
      }
      itk_component add backrat {
         gaia::LabelScale $itk_component(intfrm).backrat \
            -text "Back light ratio:" \
            -labelwidth $lwidth \
            -from 2 \
            -to 10 \
            -resolution 0.1 \
            -orient horizontal \
            -command [code $this set_prop KeyToBackRatio]
      }

      pack $itk_component(keyint) $itk_component(fillrat) \
         $itk_component(backrat) $itk_component(headrat) \
         -side top -fill x

      set lwidth 8

      #  Other key light panel
      itk_component add keyfrm {
         labelframe $itk_component(container).keyfrm \
            -text "Key light tweaks:"
      } {}
      itk_component add keywarm {
         gaia::LabelScale $itk_component(keyfrm).warm \
            -text "Warmth:" \
            -labelwidth $lwidth \
            -from 0 \
            -to 1 \
            -resolution 0.01 \
            -orient horizontal \
            -command [code $this set_prop KeyLightWarmth]
      }
      itk_component add keyelev {
         gaia::LabelScale $itk_component(keyfrm).elev \
            -text "Elevation:" \
            -labelwidth $lwidth \
            -from 0 \
            -to 90 \
            -orient horizontal \
            -command [code $this set_prop KeyLightElevation]
      }
      itk_component add keyazim {
         gaia::LabelScale $itk_component(keyfrm).azim \
            -text "Azimuth:" \
            -labelwidth $lwidth \
            -from -90 \
            -to 90 \
            -orient horizontal \
            -command [code $this set_prop KeyLightAzimuth]
      }

      pack $itk_component(keywarm) $itk_component(keyelev) \
         $itk_component(keyazim) -side top -fill x

      #  Other fill light panel
      itk_component add fillfrm {
         labelframe $itk_component(container).fillfrm \
            -text "Fill light tweaks:"
      } {}
      itk_component add fillwarm {
         gaia::LabelScale $itk_component(fillfrm).warm \
            -labelwidth $lwidth \
            -text "Warmth:" \
            -from 0 \
            -to 1 \
            -resolution 0.01 \
            -orient horizontal \
            -command [code $this set_prop FillLightWarmth]
      }
      itk_component add fillelev {
         gaia::LabelScale $itk_component(fillfrm).elev \
            -labelwidth $lwidth \
            -text "Elevation:" \
            -from -90 \
            -to 10 \
            -orient horizontal \
            -command [code $this set_prop FillLightElevation]
      }
      itk_component add fillazim {
         gaia::LabelScale $itk_component(fillfrm).azim \
            -labelwidth $lwidth \
            -text "Azimuth:" \
            -from -90 \
            -to 90 \
            -orient horizontal \
            -command [code $this set_prop FillLightAzimuth]
      }

      pack $itk_component(fillwarm) $itk_component(fillelev) \
         $itk_component(fillazim) -side top -fill x

      #  Other back light panel
      itk_component add backfrm {
         labelframe $itk_component(container).backfrm \
            -text "Back light tweaks:"
      } {}
      itk_component add backwarm {
         gaia::LabelScale $itk_component(backfrm).warm \
            -labelwidth $lwidth \
            -text "Warmth:" \
            -from 0 \
            -to 1 \
            -resolution 0.01 \
            -orient horizontal \
            -command [code $this set_prop BackLightWarmth]
      }
      itk_component add backelev {
         gaia::LabelScale $itk_component(backfrm).elev \
            -labelwidth $lwidth \
            -text "Elevation:" \
            -from -45 \
            -to 45 \
            -orient horizontal \
            -command [code $this set_prop BackLightElevation]
      }
      itk_component add backazim {
         gaia::LabelScale $itk_component(backfrm).azim \
            -labelwidth $lwidth \
            -text "Azimuth:" \
            -from 60 \
            -to 170 \
            -orient horizontal \
            -command [code $this set_prop BackLightAzimuth]
      }

      pack $itk_component(backwarm) $itk_component(backelev) \
         $itk_component(backazim) -side top -fill x

      #  Other head light panel
      itk_component add headfrm {
         labelframe $itk_component(container).headfrm \
            -text "Head light tweaks:"
      } {}
      itk_component add headwarm {
         gaia::LabelScale $itk_component(headfrm).warm \
            -labelwidth $lwidth \
            -text "Warmth:" \
            -from 0 \
            -to 1 \
            -resolution 0.01 \
            -orient horizontal \
            -command [code $this set_prop HeadLightWarmth]
      }

      pack $itk_component(headwarm) -side top -fill x

      pack $itk_component(intfrm) $itk_component(keyfrm) \
         $itk_component(fillfrm) $itk_component(backfrm) \
         $itk_component(headfrm) -side left -fill both -expand 1

      pack $itk_component(container) -fill both -expand 1
   }

   #  Set defaults. These are from the LightKit which should be
   #  tuned already.
   protected method set_defaults_ {} {

      if { $need_defaults_ } {
         #  Loop over all getter methods.
         foreach {m w} $names_ {
            set defaults_($w) [$lightkit_ Get${m}]
         }

         #  Maintain luminance option.
         set defaults_(maintlum) 0
         set need_defaults_ 0
      }

      #  Apply defaults to the UI.
      foreach {m w} $names_ {
         $itk_component($w) set $defaults_($w)
      }
      set maintain_luminance_ $defaults_(maintlum)
   }

   #  Apply the settings of the UI to the light kit.
   protected method apply_defaults_ {} {
      set_defaults_

      #  Loop over all setter methods.
      foreach {m w} $names_ {
         $lightkit_ Set${m} $defaults_($w)
      }
      set_maintain_luminance_
   }

   #  Set various values and apply immediately. The name is the
   #  LightKit property (see vtkLightKit documentation).
   public method set_prop {name value} {
      $lightkit_ Set${name} $value
      $itk_option(-renwindow) render
   }

   #  Checkbutton requires slight different handling.
   protected method set_maintain_luminance_ {} {
      set_prop MaintainLuminance $maintain_luminance_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Gaia3dVtkWindow that displays the scene.
   itk_option define -renwindow renwindow Renwindow {} {
      if { $itk_option(-renwindow) != {} } {
         set lightkit_ [$itk_option(-renwindow) get_lightkit]
      } else {
         set lightkit_ {}
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------
   protected variable lightkit_ {}
   protected variable maintain_luminance_ 0
   protected variable defaults_
   protected variable need_defaults_ 1

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Names of all light kit properties getter and setter methods we use and
   #  their local widget names.
   common names_ {
      KeyLightIntensity  keyint
      KeyToFillRatio     fillrat
      KeyToBackRatio     backrat
      KeyToHeadRatio     headrat
      KeyLightWarmth     keywarm
      KeyLightElevation  keyelev
      KeyLightAzimuth    keyazim
      FillLightWarmth    fillwarm
      FillLightElevation fillelev
      FillLightAzimuth   fillazim
      BackLightWarmth    backwarm
      BackLightElevation backelev
      BackLightAzimuth   backazim
      HeadLightWarmth    headwarm
   }

#  End of class definition.
}
