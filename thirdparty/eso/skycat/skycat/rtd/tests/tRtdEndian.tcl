#!/bin/sh
#\
    exec rtdimage_wish "$0" ${1+"$@"}

# E.S.O. - VLT project 
# "@(#) $Id: tRtdEndian.tcl,v 1.1.1.1 2006/01/12 16:38:01 abrighto Exp $"
#
# tRtdEndian.tcl - automatic test procedure for Rtd byte swap handling
#
# Test anything which is releated to Big/Little/Native Endian
# and this for every data type supported by RTD (with special attention
# to float data).
# Eg. data are produced by a detector system on a Sparc and transferred
# (without byte swap) to a Linux-PC where RTD is running on. 
#
#
# who             when       what
# --------------  ---------  ----------------------------------------
# P. Biereichel   11/08/99   Created

#
# --------------------------------------------------------------
# Common procedures
# --------------------------------------------------------------
#

proc feedback { args } {
    global debug
    if {$debug != 0} {
	puts $args
    }
}

proc startRtd {} {
    feedback "starting rtd ..."
    return [exec rtd -rtd_geometry -0+0 -attach 1 \
		-rtd_title {RTD TEST ** RTD TEST ** RTD TEST} &]
}

proc connectToRtd {} {
    global ::rtd_fd
    while {1} {
        feedback "connecting to Rtd..."
        # open the connection
        set rtd_fd [connect_to_rtd]
        if {$rtd_fd > 0} { break }
        sleep 2
    }
    feedback "connected to rtd."
}

proc sendTclCmd {cmd} {
    global ::rtd_fd
    # feedback "sending rtd tcl command: $cmd"
    set ret [etcl $rtd_fd $cmd]
    # feedback "received reply: $ret\n"
    return $ret
}

proc sendRtdCmd {cmd} {
    global ::rtd_fd
    # feedback "sending rtd subimage command: $cmd"
    set ret [send_to_rtd $rtd_fd $cmd]
    # feedback "received reply: $ret\n"
    return $ret
}

# run tRtd for some seconds and abort it afterwards
proc run_tRtd {endian dataType} {
    set pid [exec tRtd -E $endian -D $dataType -t 200 &]
    after 2000
    kill $pid
    after 500
}

# get min,max values of the reference pixel
proc getRefPix { } {
    set minvalues [sendRtdCmd "get 11 11 image"]
    set maxvalues [sendRtdCmd "get 11 12 image"]

    lassign $minvalues x y minvalue
    lassign $maxvalues x y maxvalue

    return "$minvalue $maxvalue"
}

#
# --------------------------------------------------------------
# Test procedures
# --------------------------------------------------------------
#

#
# run tRtd check the value of the reference pixel
#
proc Test1 { text endian dataType minval maxval } {
    global TestError

    run_tRtd $endian $dataType

    lassign [getRefPix] minvalue maxvalue

    if {$minvalue != $minval || $maxvalue != $maxval} {
	feedback "FAILED... $text\: Expected values $minval, $maxval .. got .. $minvalue, $maxvalue"
	set TestError 1
	return 1
    }
    feedback "OK... $text\: values at ref. (11,11-12) = $minvalue, $maxvalue"
    return 0
}

#
# write shm image to disk, clear image and load shm image from disk. Then test ref. pixel.
#
proc Test2 { text endian dataType minval maxval } {
    global TestError fitsfile

    run_tRtd $endian $dataType

    sendRtdCmd "dump $fitsfile"
    sendRtdCmd "clear"
    after 100
    sendRtdCmd "config -file $fitsfile"
    after 500
    
    lassign [getRefPix] minvalue maxvalue

    if {$minvalue != $minval || $maxvalue != $maxval} {
	feedback "FAILED... $text\: Expected values $minval, $maxval .. got .. $minvalue, $maxvalue"
	set TestError 1
	return 1
    }
    feedback "OK... $text\: values at ref. (11,11-12) = $minvalue, $maxvalue"
    return 0
}


#
# run tRtd, store bias frame, subtract and check that the
# image is zero
#
proc Test3 { text endian dataType } {
    global TestError

    sendRtdCmd "biasimage off"
    sendRtdCmd "biasimage select 0"

    run_tRtd $endian $dataType
    sendRtdCmd "camera pause"
    sendRtdCmd "update"
    sendRtdCmd "biasimage copy 0"
    sendRtdCmd "biasimage on"
    sendRtdCmd "update"
    after 100

    set val1 [sendRtdCmd "get 10 40 image"]
    set val2 [sendRtdCmd "get 10 41 image"]

    sendRtdCmd "camera continue"

    lassign $val1 x y val1
    lassign $val2 x y val2

    if {$val1 != 0 || $val2 != 0} {
	feedback "FAILED... $text\: Expected values 0, 0 .. got .. $val1, $val2"
	set TestError 1
	return 1
    }
    feedback "OK... $text\: image is cleared"
    return 0
}
#
# run tRtd, load bias frame from disk, subtract and check the
# reference pixels
#
proc Test4 { text endian dataType1 dataType2 minval1 maxval1 minval2 maxval2 } {
    global TestError fitsfile
    
    sendRtdCmd "biasimage off"

    run_tRtd $endian $dataType2

    sendRtdCmd "dump $fitsfile"
    sendRtdCmd "update"
    sendRtdCmd "biasimage file $fitsfile 0"
    sendRtdCmd "biasimage on"
    sendRtdCmd "biasimage select 0"

    run_tRtd $endian $dataType1
    
    lassign [getRefPix] val1 val2
    
    set expmin [expr $minval1 - $minval2]
    set expmax [expr $maxval1 - $maxval2]

    # take into account the tcl_precision
    if {[format "%.6g" $expmin] !=  $val1 || [format "%.6g" $expmax] != $val2} {
	feedback "FAILED... $text\: Expected values $expmin, $expmax .. got .. $val1, $val2"
	set TestError 1
	return 1
    }
    feedback "OK... $text"
    return 0
}

#
# run tRtd, copy images of different data types to bias images,
# generate a main image of type dataType, subtract
# 
#
proc Test5 { text endian dataType nr  minval1 maxval1 minval2 maxval2} {
    global TestError Test5Inititialized

    if {! $Test5Inititialized } {
	sendRtdCmd "biasimage off"
	set Test5Inititialized 1
	set i 0
	foreach type "-32 32 -16 16 8" {
	    run_tRtd $endian $type
	    sendRtdCmd "biasimage copy $i"
	    incr i
	}
	sendRtdCmd "biasimage on"
    }
    run_tRtd $endian $dataType

    sendRtdCmd "biasimage select $nr"

    lassign [getRefPix] val1 val2
    
    set expmin [expr $minval1 - $minval2]
    set expmax [expr $maxval1 - $maxval2]

    # take into account the tcl_precision
    if {[format "%.6g" $expmin] !=  $val1 || [format "%.6g" $expmax] != $val2} {
	feedback "FAILED... $text\: Expected values $expmin, $expmax .. got .. $val1, $val2"
	set TestError 1
	return 1
    }
    feedback "OK... $text (bias number $nr)"
    return 0
}

#
# --------------------------------------------------------------
# Procedures which call the test programs
# --------------------------------------------------------------
#

proc callTest1 { } {
    feedback "\nTest 1: Checking that RTD handles byte-swapped data in shm correctly..."

    foreach nameTypeype {"Little Endian" "Big Endian" "Native"} endian {1 0 -1} {
	Test1 "$nameTypeype, Byte   Data" $endian   8   0       255
	Test1 "$nameTypeype, Short  Data" $endian  16   -10000  10000
	Test1 "$nameTypeype, UShort Data" $endian -16   0       30000
	Test1 "$nameTypeype, Int    Data" $endian  32   -100000 100000
	Test1 "$nameTypeype, Float  Data" $endian -32   -1.E7   1.E7
    }
}

proc callTest2 { } {
    feedback "\nTest 2: Write shm image to disk, clear image and load shm image from disk. Then test ref. pixel."

    foreach nameTypeype {"Little Endian" "Big Endian" "Native"} endian {1 0 -1} {
	Test2 "$nameTypeype, Float  Data" $endian -32   -1.E7   1.E7
	Test2 "$nameTypeype, Int    Data" $endian  32   -100000 100000
	Test2 "$nameTypeype, UShort Data" $endian -16   0       30000
	Test2 "$nameTypeype, Short  Data" $endian  16   -10000  10000
	Test2 "$nameTypeype, Byte   Data" $endian   8   0       255
    }
}

proc callTest3 { } {
    feedback "\nTest 3: Generate image, store as bias frame, subtract. Then test that ref. pixel. is zero"

    foreach nameTypeype {"Little Endian" "Big Endian" "Native"} endian {1 0 -1} {
	Test3 "$nameTypeype, Byte   Data" $endian   8
	Test3 "$nameTypeype, Short  Data" $endian  16
	Test3 "$nameTypeype, UShort Data" $endian -16
	Test3 "$nameTypeype, Int    Data" $endian  32
	Test3 "$nameTypeype, Float  Data" $endian -32
    }
}

proc callTest4 { } {
    feedback "\nTest 4: Generate image, load bias frame from disk, subtract. Then test ref. pixel."

    foreach nameTypeype {"Little Endian" "Big Endian" "Native"} endian {1 0 -1} {
	Test4 "$nameTypeype, Float - Float " $endian -32 -32  -1.E7   1.E7   -1.E7   1.E7
	Test4 "$nameTypeype, Float - Int   " $endian -32  32  -1.E7   1.E7   -100000 100000
	Test4 "$nameTypeype, Float - UShort" $endian -32 -16  -1.E7   1.E7   0       30000
	Test4 "$nameTypeype, Float - Short " $endian -32  16  -1.E7   1.E7   -10000  10000
	Test4 "$nameTypeype, Float - Byte  " $endian -32   8  -1.E7   1.E7   0       255

	Test4 "$nameTypeype, Int   - Int   " $endian  32  32  -100000 100000 -100000 100000
	Test4 "$nameTypeype, Int   - UShort" $endian  32 -16  -100000 100000 0       30000
	Test4 "$nameTypeype, Int   - Short " $endian  32  16  -100000 100000 -10000  10000
	Test4 "$nameTypeype, Int   - Byte  " $endian  32   8  -100000 100000 0       255

	Test4 "$nameTypeype, UShort- UShort" $endian -16 -16  0       30000  0       30000
	Test4 "$nameTypeype, UShort- Byte  " $endian -16   8  0       30000  0       255

	Test4 "$nameTypeype, Short - Short " $endian  16  16  -10000  10000  -10000  10000
	Test4 "$nameTypeype, Short - Byte  " $endian  16   8  -10000  10000  0       255

	Test4 "$nameTypeype, Byte  - Byte  " $endian   8   8  0       255    0       255

    }
}

proc callTest5 { biasEndianName biasEndian } {
    feedback "\nTest 5: Test subtraction of different bias frame numbers (bias: $biasEndianName)"

    sendRtdCmd "biasimage off"
    set i 0
    foreach type "-32 32 -16 16 8" {
	run_tRtd $biasEndian $type
	sendRtdCmd "biasimage copy $i"
	incr i
    }
    sendRtdCmd "biasimage on"


    foreach nameTypeype {"Little Endian" "Big Endian" "Native"} endian {1 0 -1} {
	Test5 "$nameTypeype, Float  Data" $endian -32 0   -1.E7   1.E7   -1.E7   1.E7
	Test5 "$nameTypeype, Float  Data" $endian -32 1   -1.E7   1.E7   -100000 100000
	Test5 "$nameTypeype, Float  Data" $endian -32 2   -1.E7   1.E7   0       30000
	Test5 "$nameTypeype, Float  Data" $endian -32 3   -1.E7   1.E7   -10000  10000
	Test5 "$nameTypeype, Float  Data" $endian -32 4   -1.E7   1.E7   0       255

	Test5 "$nameTypeype, Int    Data" $endian  32 1  -100000 100000 -100000 100000
	Test5 "$nameTypeype, Int    Data" $endian  32 2  -100000 100000 0       30000
	Test5 "$nameTypeype, Int    Data" $endian  32 3  -100000 100000 -10000  10000
	Test5 "$nameTypeype, Int    Data" $endian  32 4  -100000 100000 0       255

	Test5 "$nameTypeype, UShort Data" $endian -16 2  0       30000  0       30000
	Test5 "$nameTypeype, UShort Data" $endian -16 4  0       30000  0       255

	Test5 "$nameTypeype, Short  Data" $endian  16 3  -10000  10000  -10000  10000
	Test5 "$nameTypeype, Short  Data" $endian  16 4  -10000  10000  0       255

	Test5 "$nameTypeype, Byte   Data" $endian   8 4  0       255    0       255
    }
}

#
# --------------------------------------------------------------
# "Main"
# --------------------------------------------------------------
#

lappend auto_path ../library
package require Rtd

# debug flag for development
set debug 0

set rtdPid [startRtd]
connectToRtd
set TestError 0
set Test5Inititialized 0
set fitsfile /tmp/$env(USER)tRtdEndian.fits

callTest1
callTest2
callTest3
callTest4
callTest5 "Little Endian" 1
callTest5 "Big Endian" 0
callTest5 "Native" -1


if {$TestError} {
    puts "\ntRtdEndian Test FAILED !"
} else {
    feedback "tRtdEndian OK"
}
catch {kill $rtdPid}
catch {exec rm -f $fitsfile"}
catch {exec rm -f "$fitsfile.BAK"}
exit 0














