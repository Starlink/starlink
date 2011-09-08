#!/bin/bash

# Simple script to demonstrate how to create a single-valued
# heater efficiency file suitable for use by SMURF.

source $STARLINK_DIR/etc/profile
kappa > /dev/null

# Original resistor values now scaled from 3 ohms
# to get initial heater efficiency values

# SG450_M1004D1000.refres = 1.34   => 0.446666
# SG450_M1007D1002.refres = 1.05   => 0.35
# SG450_M1006D1003.refres = 2.52   => 0.84
# SG450_M1009D1008.refres = 1.61   => 0.536666

# SG850_M0906D1005.refres = 0.71   => 0.236666
# SG850_M1002D1006.refres = 0.77   => 0.256666
# SG850_M1005D1007.refres = 0.77   => 0.256666
# SG850_M1003D1004.refres = 0.93   => 0.31

# Ratio is 3/original
arrayid="SG450_M1004D1000"
ratio=0.4466666666
arrayid="SG450_M1007D1002"
ratio=0.35
arrayid="SG450_M1006D1003"
ratio=0.84
arrayid="SG450_M1009D1008"
ratio=0.5366666666
arrayid="SG850_M0906D1005"
ratio=0.2366666666
arrayid="SG850_M1002D1006"
ratio=0.2566666666
arrayid="SG850_M1005D1007"
ratio=0.2566666666
arrayid="SG850_M1003D1004"
ratio=0.31

filename=`echo ${arrayid}_heateff.sdf | tr "[:upper:]" "[:lower:]"`

if test -e $filename ; then rm $filename; fi

creframe lbound="[0,0]" ubound="[31,39]" mode=FL mean=$ratio out=$filename

# Now update the headers
fitsmod ndf=$filename edit=AMEND position=! keyword=INSTRUME value="SCUBA-2" COMMENT='"Instrument name"'
fitsmod ndf=$filename edit=AMEND position=! keyword=ARRAYID value=$arrayid COMMENT='"Array serial number"'

# Title
settitle $filename title=$arrayid
setlabel $filename "'Heater Efficiency'"