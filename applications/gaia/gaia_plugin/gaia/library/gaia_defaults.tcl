# E.S.O. - VLT project/ESO Archive
# @(#) $Id: gaia_defaults.tcl,v 1.1 1998/04/06 23:54:15 abrighto Exp $
#
# catdefaults.tcl - X defaults for itk catalog widgets
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 96   created


# itk widget defaults (normally you'd put these in the itk sources, but
# putting them here makes it easier to "pre-load" the classes for the single
# binary version...

proc gaia::setXdefaults {} {
    #  GAIA defaults:
    option add *GaiaImage.canvasBackground     black
    option add *GaiaImageCtrl.canvasBackground black
    option add *GaiaImageCtrl.canvasWidth      512
    option add *GaiaImageCtrl.canvasHeight     512
    option add *StarLabelCheck.anchor          w

    #  Stop ugly tearoff menus.
    option add *TearOff  0 userDefault
}
