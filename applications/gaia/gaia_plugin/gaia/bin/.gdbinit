dir .. ../src ../../../../skycat/interp/src ../../../../cat/astrocat/src ../../../../cat/tclcat/src ../../../../rtd/rtdimg/src ../../../../rtd/rtilib/src ../../../../astrotcl/wcslib/src ../../../../astrotcl/imageio/src ../../../../tclutil/util/src ../../../../tclutil/tclutil/src
cd ../demos
b main
set env GAIA_LIBRARY ../library
set env SKYCAT_LIBRARY ../../../../skycat/interp/library
set env CAT_LIBRARY ../../../../cat/tclcat/library
set env RTD_LIBRARY ../../../../rtd/rtdimg/library
set env ASTROTCL_LIBRARY ../../../../astrotcl/astrotcl/library
set env TCLUTIL_LIBRARY ../../../../tclutil/tclutil/library
r gaia.tcl

