dir .. ../src ../../../cat/astrocat/src ../../../cat/tclcat/src ../../../rtd/rtdimg/src ../../../rtd/rtilib/src ../../../astrotcl/wcslib/src ../../../astrotcl/imageio/src ../../../tclutil/util/src ../../../tclutil/tclutil/src
cd ../demos
b main
set env SKYCAT_LIBRARY ../library
set env CAT_LIBRARY ../../../cat/tclcat/library
set env RTD_LIBRARY ../../../rtd/rtdimg/library
set env ASTROTCL_LIBRARY ../../../astrotcl/astrotcl/library
set env TCLUTIL_LIBRARY ../../../tclutil/tclutil/library

set env RTD_CAMERA SCAM_FF
r skycat.tcl -debug 1 -rtd 1 -panel_orient horizontal 

