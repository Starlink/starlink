# E.S.O. - VLT project
# "@(#) $Id: rtd_defaults.tcl,v 1.1 1997/11/28 01:35:14 abrighto Exp $"
#
# defaults.tcl - set widget defaults
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 11 Oct 95   created


# set general widget defaults

proc rtd::setXdefaults {} {
    util::setXdefaults

    option add *RtdImage.canvasBackground black 
    option add *RtdImageCtrl.canvasBackground black 
    option add *RtdImageCtrl.canvasWidth 520 
    option add *RtdImageCtrl.canvasHeight 520 
    option add *RtdImagePan.background black 
    option add *RtdImageTrans.relief flat 
    option add *RtdImageColorRamp.cursor exchange 
}

