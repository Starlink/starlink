# doxx.tcl --
#
# Common code shared by doconfig.tcl and domakefile.tcl
#

case $argv {
    tk4.0 {
	set TCL_VERSION 7.4
	set subs(@@_V_TCL_VER_@@) 7.4
	set subs(@@_V_TCLVER_@@)  74
	set subs(@@_V_TK_VER_@@)  4.0
	set subs(@@_V_TKVER_@@)   40
    }
    tk4.1 {
	set TCL_VERSION 7.5
	set subs(@@_V_TCL_VER_@@) 7.5
	set subs(@@_V_TCLVER_@@)  75
	set subs(@@_V_TK_VER_@@)  4.1
	set subs(@@_V_TKVER_@@)   41
    }
    tk4.2 {
	set TCL_VERSION 7.6
	set subs(@@_V_TCL_VER_@@) 7.6
	set subs(@@_V_TCLVER_@@)  76
	set subs(@@_V_TK_VER_@@)  4.2
	set subs(@@_V_TKVER_@@)   42
    }
    tk4.3 {
	set TCL_VERSION 7.7
	set subs(@@_V_TCL_VER_@@) 7.7
	set subs(@@_V_TCLVER_@@)  77
	set subs(@@_V_TK_VER_@@)  4.3
	set subs(@@_V_TKVER_@@)   43
    }
    tk8.0 {
	set TCL_VERSION 8.0
	set subs(@@_V_TCL_VER_@@) 8.0
	set subs(@@_V_TCLVER_@@)  80
	set subs(@@_V_TK_VER_@@)  8.0
	set subs(@@_V_TKVER_@@)   80
    }
    itcl2.0 {
	set TCL_VERSION 7.4
	set subs(@@_V_TCL_VER_@@)  7.4
	set subs(@@_V_TCLVER_@@)   74
	set subs(@@_V_TK_VER_@@)   4.0
	set subs(@@_V_TKVER_@@)    40
	set subs(@@_V_ITCL_VER_@@) 2.0
	set subs(@@_V_ITCLVER_@@)  20
    }
    itcl2.1 {
	set TCL_VERSION 7.5
	set subs(@@_V_TCL_VER_@@)  7.5
	set subs(@@_V_TCLVER_@@)   75
	set subs(@@_V_TK_VER_@@)   4.1
	set subs(@@_V_TKVER_@@)    41
	set subs(@@_V_ITCL_VER_@@) 2.1
	set subs(@@_V_ITCLVER_@@)  21
	set subs(@@_V_IWIDGETS_VER_@@) 2.1.0
    }
    itcl2.2 {
	set TCL_VERSION 7.6
	set subs(@@_V_TCL_VER_@@)  7.6
	set subs(@@_V_TCLVER_@@)   76
	set subs(@@_V_TK_VER_@@)   4.2
	set subs(@@_V_TKVER_@@)    42
	set subs(@@_V_ITCL_VER_@@) 2.2
	set subs(@@_V_ITCLVER_@@)  22
	set subs(@@_V_IWIDGETS_VER_@@) 2.2.0
    }
    default {
	puts stderr "option \"$argv\" not supported"
	exit 1
    }
}

if [info exists subs(@@_V_ITCLVER_@@)] {
    set ITCL 1
    set subs(@@_V_LNAME_@@)   tix
    set subs(@@_V_BVEREXT_@@) 1
} else {
    set ITCL 0
    set subs(@@_V_LNAME_@@) tix
    set subs(@@_V_BVEREXT_@@) ""
}

if {$subs(@@_V_TCL_VER_@@) == 7.4} {
    set TCL74 1
} else {
    set TCL74 0
}

proc p {string} {
    global subs

    foreach name [array name subs] {
	regsub -all $name $string $subs($name) string
    }
    regsub ^[format \n] $string "" string
    regsub "\[[format \t] \]*\$" $string "" string
    regsub -all \\\\\\\\ $string \\ string
    puts $string
}

proc p74 {string} {
    global TCL74
    if $TCL74 {
	p $string
    }
}

proc p75+ {string} {
    global TCL74
    if !$TCL74 {
	p $string
    }
}

proc pitcl {string} {
    global ITCL
    if $ITCL {
	p $string
    }
}

proc ptcl {string} {
    global ITCL
    if !$ITCL {
	p $string
    }
}

set ENABLE_SAM 0
set SAM_LIB    0
set SAM_EXE    0

if {!$ITCL} {
    if {$TCL_VERSION <= 7.6} {
	set ENABLE_SAM 1
	set SAM_LIB    1
	set SAM_EXE    1
    } else {
	set ENABLE_SAM 1
	set SAM_LIB    1
	set SAM_EXE    0
    }
} else {
    if {$TCL_VERSION == 7.6} {
	set ENABLE_SAM 1
	set SAM_LIB    1
	set SAM_EXE    0
    }
}

proc p_sam {string} {
    global ENABLE_SAM
    if $ENABLE_SAM {
	p $string
    }
}
