#
# fmath.tcl --
#
#   Contains a package of procs that interface to the Tcl expr command built-in
# functions.  These procs provide compatibility with older versions of TclX and
# are also generally useful.
#------------------------------------------------------------------------------
# Copyright 1993-1997 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: fmath.tcl,v 8.2 1997/08/23 18:55:20 markd Exp $
#------------------------------------------------------------------------------

#@package: TclX-fmath acos asin atan ceil cos cosh exp fabs floor log log10 \
           sin sinh sqrt tan tanh fmod pow atan2 abs double int round

proc acos  x {uplevel [list expr acos($x)]}
proc asin  x {uplevel [list expr asin($x)]}
proc atan  x {uplevel [list expr atan($x)]}
proc ceil  x {uplevel [list expr ceil($x)]}
proc cos   x {uplevel [list expr cos($x)]}
proc cosh  x {uplevel [list expr cosh($x)]}
proc exp   x {uplevel [list expr exp($x)]}
proc fabs  x {uplevel [list expr abs($x)]}
proc floor x {uplevel [list expr floor($x)]}
proc log   x {uplevel [list expr log($x)]}
proc log10 x {uplevel [list expr log10($x)]}
proc sin   x {uplevel [list expr sin($x)]}
proc sinh  x {uplevel [list expr sinh($x)]}
proc sqrt  x {uplevel [list expr sqrt($x)]}
proc tan   x {uplevel [list expr tan($x)]}
proc tanh  x {uplevel [list expr tanh($x)]}

proc fmod {x n} {uplevel [list expr fmod($x,$n)]}
proc pow {x n} {uplevel [list expr pow($x,$n)]}

# New functions that TclX did not provide in eariler versions.

proc atan2  x {uplevel [list expr atan2($x)]}
proc abs    x {uplevel [list expr abs($x)]}
proc double x {uplevel [list expr double($x)]}
proc int    x {uplevel [list expr int($x)]}
proc round  x {uplevel [list expr round($x)]}



