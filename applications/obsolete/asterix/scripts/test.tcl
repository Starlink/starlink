#!/star/bin/awish

button .a -text Exit -command exit -relief flat

pack .a

bind .a <Enter> {.a configure -relief raised}
bind .a <Leave> {.a configure -relief flat}
