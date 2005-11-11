dir .. ../src ../../rtilib/src ../../../astrotcl/wcslib/src ../../../astrotcl/imageio/src ../../../tclutil/util/src ../../../tclutil/tclutil/src
cd ../demos
b main
set env RTD_LIBRARY ../library
set env ASTROTCL_LIBRARY ../../../astrotcl/astrotcl/library
set env TCLUTIL_LIBRARY ../../../tclutil/tclutil/library
r rtd.tcl -debug 1 test.fits

