dir .. ../src ../../wcslib/src ../../imageio/src ../../../tclutil/util/src ../../../tclutil/tclutil/src
cd ../demos
b main
set env ASTROTCL_LIBRARY ../library
set env TCLUTIL_LIBRARY ../../../tclutil/tclutil/library
r astrotcl.tcl

