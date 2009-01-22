# tDOM Tcl package index file

package ifneeded tdom 0.8.2 \
    "[list load   [file join $dir tdom082[info sharedlibextension] ] tdom];\
     [list source [file join $dir tdom.tcl]]"
