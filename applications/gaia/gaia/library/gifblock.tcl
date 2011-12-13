# gifblock.tcl
#
#   Manipulate GIF streams in pure Tcl
#
# Copyright (c) 2006 Michael Thomas Greer
# Copyright (c) 2006 Particle Physics and Astronomy Research Council
#
#   This library is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Lesser General Public
#   License as published by the Free Software Foundation; either
#   version 2.1 of the License, or (at your option) any later version.
#
#   This library is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public
#   License along with this library; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# See gifblock.txt for documentation
#

namespace eval ::gifblock:: {
   namespace export \
      gif.blocknames \
      gif.get        \
      gif.index      \
      gif.load       \
      gif.save       \
      gif.set
   package provide gifblock 1.0
}

#-----------------------------------------------------------------------------
proc ::gifblock::gif.blocknames varName {
#-----------------------------------------------------------------------------
 upvar 1 $varName blocks
 set cntr -1
 set count [llength $blocks]
 set result {}
 while {[incr cntr] < $count} {lappend result [gif.get blocks $cntr type]}
 return $result
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.get {varName index args} {
#-----------------------------------------------------------------------------
 upvar 1 $varName blocks

 foreach {index args} [eval gif.IndexBlock blocks [list $index] $args] break

 if {$args eq {}} {return [lindex $blocks $index]}

 array set block [lindex $blocks $index]

 foreach name $args {if {![info exists block($name)]} {
   if {$name eq {type}} \
     then {return -code error "element \"type\" required in all blocks; missing in block #$index"} \
     else {return -code error "element \"$name\" not found in block #$index ($block(type))"}
   } }

 if {[llength $args] == 1} {return $block([lindex $args 0])}

 foreach name $args {lappend result $block($name)}
 return $result
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.index {varName index args} {
#-----------------------------------------------------------------------------
 upvar 1 $varName blocks
 return [lindex [eval gif.IndexBlock blocks [list $index] $args] 0]
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.load {varName filename} {
#-----------------------------------------------------------------------------
 upvar 1 $varName result
 set result {}

 set f [open $filename r]
 fconfigure $f -translation binary

 set iserror [catch {

 # ................................................................... Header
 set sig [encoding convertfrom ascii [read $f 3]]
 set ver [encoding convertfrom ascii [read $f 3]]
 if {($sig ne {GIF}) \
   || ![string is integer -strict [string range $ver 0 1]] \
   || ![string is alpha   -strict [string index $ver 2]]} {
   error "not a valid GIF"
   }
 lappend result [list \
   type      {GIF Header} \
   version   $ver \
   ]

 # ................................................ Logical Screen Descriptor
 gif.LoadBlock $f {
   unsigned           width
   unsigned           height
   packed {
     7                iscolormap
     color.resolution colorres
     3                issorted
     color.table.size size
     }
   byte               bgcidx
   aspect             aspect
   }
 if {$iscolormap} {                                  # GIF Global Color Table
   lappend result [list \
     type      {Color Table} \
     sorted?   $issorted \
     colors    [gif.LoadColorTable $f $size] \
     ]
   }
 lappend result [list \
   type                     {Logical Screen Descriptor} \
   width                    $width \
   height                   $height \
   {color resolution}       $colorres \
   {background color index} $bgcidx \
   {pixel aspect ratio}     $aspect \
   ]

 # ..........................................................................
 while {true} {
   gif.LoadBlock $f {byte blocktype}
   if {$blocktype == 0x21} {
     gif.LoadBlock $f {byte exttype}
     set blocktype ext-$exttype
     }

   switch -glob -- $blocktype {
     44 { # ................................................ Image Descriptor
       gif.LoadBlock $f {
         unsigned           left
         unsigned           top
         unsigned           width
         unsigned           height
         packed {
           7                iscolormap
           6                isinterlaced
           5                issorted
           43               reserved
           color.table.size size
           }
         }
       if {$iscolormap} {                             # GIF Local Color Table
         lappend result [list \
           type    {Color Table} \
           sorted? $issorted \
           colors  [gif.LoadColorTable $f $size] \
           ]
         }
       gif.LoadBlock $f {byte codesize}
       lappend result [list \
         type                    {Image Descriptor} \
         left                    $left \
         top                     $top \
         width                   $width \
         height                  $height \
         interlaced?             $isinterlaced \
         reserved                $reserved \
         {lzw minimum code size} $codesize \
         data                    [gif.LoadSubBlocks $f unpack] \
         ]
       }

     ext-249 { # .................................. Graphic Control Extension
       gif.LoadBlock $f {
         byte     size
         packed {
           75     reserved
           42     method
           1      isui
           0      istransp
           }
         unsigned delay
         byte     transidx
         byte     term
         }
       set temp [list \
         type              {Graphic Control} \
         reserved          $reserved \
         {disposal method} $method \
         {user input?}     $isui \
         {delay time}      $delay \
         ]
       if {$istransp} {lappend temp {transparent color index} $transidx}
       lappend result $temp
       }

     ext-254 { # .......................................... Comment Extension
       lappend result [list \
         type Comment \
         text [encoding convertfrom ascii [gif.LoadSubBlocks $f unpack]] \
         ]
       }

     ext-1 { # ......................................... Plain Text Extension
       gif.LoadBlock $f {
         byte     size
         unsigned left
         unsigned top
         unsigned width
         unsigned height
         byte     cellwidth
         byte     cellheight
         byte     fgcidx
         byte     bgcidx
         }
       lappend result [list \
         type                     {Plain Text} \
         left                     $left \
         top                      $top \
         width                    $width \
         height                   $height \
         {cell width}             $cellwidth \
         {cell height}            $cellheight \
         {foreground color index} $fgcidx \
         {background color index} $bgcidx \
         text    [encoding convertfrom ascii [gif.LoadSubBlocks $f unpack]] \
         ]
       }

     ext-255 { # ...................................... Application Extension
       read $f 1
       set id [encoding convertfrom ascii [read $f 8]]
       gif.LoadBlock $f {
         byte a0
         byte a1
         byte a2
         }
       set datablocks [gif.LoadSubBlocks $f leavepacked]
       lappend result [list \
         type                  Application \
         identifier            $id \
         {authentication code} [list $a0 $a1 $a2] \
         datablocks            $datablocks \
         ]
       }

     ext-* { # ....................................... Unknown extension type
       lappend result [list \
         type "Extension $exttype" \
         datablocks [gif.LoadSubBlocks $f leavepacked] \
         ]
       }

     59 { # ..................................................... GIF Trailer
       break
       }

     default {
       error {cannot understand block types not listed in the GIF89a specification}
       }
     }
   }

 } errmsg]

 close $f

 if {$iserror} {return -code error $errmsg}

 return ;# $result
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.save {varName filename} {
#-----------------------------------------------------------------------------
 upvar 1 $varName blocks

 set count [llength $blocks]
 if {$count == 0} return

 set f [open $filename w]
 fconfigure $f -translation binary

 set iserror [catch {

 # Every block must have a proper type
 gif.blocknames blocks

 # ............................................................... GIF Header
 array set block {type {} version {}}
 array set block [lindex $blocks 0]
 if {$block(type) ne {GIF Header}} {error {first block must be "GIF Header"}}
 if {$block(version) eq {}} {
   set version 87a
   set blocknames [gif.blocknames blocks]
   foreach pattern {
     {Graphic Control} Comment {Plain Text} Application Extension*
     } {
     if {[lsearch -glob $blocknames $pattern] >= 0} {
       set version 89a
       break
       }
     }
   set block(version) $version
   }

 puts -nonewline $f [encoding convertto ascii GIF]
 puts -nonewline $f [encoding convertto ascii $block(version)]

 # ..........................................................................
 set cntr 0
 while {[incr cntr] < $count} {
   array unset block
   array set block [lindex $blocks $cntr]
   switch -glob -- $block(type) {

     {Color Table} { # .......................................... Color Table
       gif.ValidateBlock block colors {{{sorted?} 0}}
       set len [llength $block(colors)]
       if {($len < 2) || ($len > 256)} {
         error {color table must have from 2 to 256 entries}
         }
       array set colorblock [lindex $blocks $cntr]
       }

     {Logical Screen Descriptor} { # .............. Logical Screen Descriptor
       if {$cntr > (1 +[info exists colorblock])} {
         error {invalid index for the Logical Screen Descriptor}
         }
       gif.ValidateBlock block {width height} {
         {{color resolution}       4}
         {{background color index} 0}
         {{pixel aspect ratio}     0}
         }
       set iscolormap [info exists colorblock]
       if {$iscolormap} \
         then {
           set issorted [expr {!!$colorblock(sorted?)}]
           set size     [gif.CalcColorTableSize [llength $colorblock(colors)]]
           } \
         else {
           set issorted 0
           set size     0
           }
       gif.WriteBlock $f [list \
         unsigned           $block(width) \
         unsigned           $block(height) \
         packed             [list \
           7                $iscolormap \
           color.resolution ${block(color resolution)} \
           3                $issorted \
           20               $size \
           ] \
         byte               ${block(background color index)} \
         aspect             ${block(pixel aspect ratio)} \
         ]
       if {$iscolormap} {
         gif.WriteColorTable $f $colorblock(colors) $size
         array unset colorblock
         }
       }

     {Image Descriptor} { # ................................ Image Descriptor
       gif.ValidateBlock block {
         width height {lzw minimum code size} data} {
         {left 0} {top 0} {interlaced? 0} {reserved 0}
         }
       set iscolormap [info exists colorblock]
       if {$iscolormap} \
         then {
           set issorted [expr {!!$colorblock(sorted?)}]
           set size     [gif.CalcColorTableSize [llength $colorblock(colors)]]
           } \
         else {
           set issorted 0
           set size     0
           }
       gif.WriteBlock $f [list \
         byte           44 \
         unsigned       $block(left) \
         unsigned       $block(top) \
         unsigned       $block(width) \
         unsigned       $block(height) \
         packed         [list \
           7            $iscolormap \
           6            $block(interlaced?) \
           5            $issorted \
           43           $block(reserved) \
           20           $size \
           ] \
         ]
       if {$iscolormap} {
         gif.WriteColorTable $f $colorblock(colors) $size
         array unset colorblock
         }
       gif.WriteBlock $f "byte ${block(lzw minimum code size)}"
       gif.WriteSubBlocks $f $block(data) pack
       gif.WriteBlock $f {byte 0}
       }

     {Graphic Control} { # ........................ Graphic Control Extension
       gif.ValidateBlock block {} {
         { reserved                  0}
         {{disposal method}          0}
         {{user input?}              0}
         {{delay time}               0}
         {{transparent color index} -1}
         }
       set istransidx [expr {${block(transparent color index)} >= 0}]
       if {!$istransidx} {set {block(transparent color index)} 0}
       gif.WriteBlock $f [list \
         byte              0x21 \
         byte              249 \
         byte              4 \
         packed            [list \
           75              $block(reserved) \
           42              ${block(disposal method)} \
           1               ${block(user input?)} \
           0               $istransidx \
           ] \
         unsigned          ${block(delay time)} \
         byte              ${block(transparent color index)} \
         byte              0 \
         ]
       }

     Comment { # .......................................... Comment Extension
       gif.ValidateBlock block text {}
       gif.WriteBlock $f [list \
         byte 0x21 \
         byte 254 \
         ]
       gif.WriteSubBlocks $f [encoding convertto ascii $block(text)] pack
       gif.WriteBlock $f {byte 0}
       }

     {Plain Text} { # .................................. Plain Text Extension
       gif.ValidateBlock block {
         left top width height {cell width} {cell height}
         {foreground color index} {background color index} text
         } {}
       gif.WriteBlock $f [list \
         byte     0x21 \
         byte     1 \
         byte     12 \
         unsigned $block(left) \
         unsigned $block(top) \
         unsigned $block(width) \
         unsigned $block(height) \
         byte     ${block(cell width)} \
         byte     ${block(cell height)} \
         byte     ${block(foreground color index)} \
         byte     ${block(background color index)} \
         ]
       gif.WriteSubBlocks $f $block(text) pack
       gif.WriteBlock $f {byte 0}
       }

     Application { # .................................. Application Extension
       gif.ValidateBlock block {identifier {authentication code}} {{datablocks {}}}
       gif.WriteBlock $f [list \
         byte 0x21 \
         byte 255 \
         byte 11 \
         ]
       if {[llength ${block(authentication code)}] < 3} {
         error {application authentication code must be a list of three 8-bit integers}
         }
       puts -nonewline $f [encoding convertto ascii $block(identifier)]
       puts -nonewline $f [eval binary format ccc ${block(authentication code)}]
       gif.WriteSubBlocks $f $block(datablocks) prepacked
       gif.WriteBlock $f {byte 0}
       }

     Extension* { # .................................. Unknown extension type
       gif.ValidateBlock block datablocks {}
       if {![llength $block(datablocks)]} {
         error "$block(type) must specify datablocks"
         }
       gif.WriteBlock $f [list byte 0x21 byte [lindex $block(type) 1]]
       gif.WriteSubBlocks $f $block(datablocks) prepacked
       gif.WriteBlock $f {byte 0}
       }

     default {
       error {cannot understand block types not listed in the GIF89a specification}
       }
     }
   }

 # .............................................................. GIF Trailer
 gif.WriteBlock $f {byte 59}

 } errmsg]

 close $f

 if {$iserror} {return -code error $errmsg}
 return
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.set {varName index args} {
#-----------------------------------------------------------------------------
 upvar 1 $varName blocks

 foreach {index args} [eval gif.IndexBlock blocks [list $index] $args] break
 array set block [lindex $blocks $index]

 if {([llength $args] % 2) == 1} {
   unset block([lindex $args end])
   set args [lrange $args 0 end-1]
   }
 foreach {element value} $args {set block($element) $value}

 lset blocks $index [array get block]
 return
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.CalcColorTableSize size {                      # gif.save
#-----------------------------------------------------------------------------
 if {($size < 2) || ($size > 256)} {
   return -code error {color table must have from 2 to 256 entries}
   }
 foreach min {2 4 8 16 32 64 128 256} value {0 1 2 3 4 5 6 7} {
   if {$size <= $min} {return $value}
   }
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.IndexBlock {varName index args} {   # gif.index,.get,.set
#-----------------------------------------------------------------------------
 upvar 1 $varName blocks

 if {![info exists blocks]} {
   return -code error "can't read \"$varName\": no such variable"
   }

 set count [llength $blocks]

 if {![string is integer -strict $index]} {
   if {![string is integer -strict [lindex $args 0]]} {
     return -code error {incorrect args: should be "gif.index varName ?type? index"}
     }
   set type  $index
   set index [lindex $args 0]
   set args  [lrange $args 1 end]
   for {set cntr 0} {$cntr < $count} {incr cntr} {
     array set block [lindex $blocks $cntr]
     if {$block(type) eq $type} {if {[incr index -1] < 0} break}
     }
   if {$block(type) ne $type} {return -1}
   set index $cntr
   }
 if {$index >= $count} {return -1}

 return [list $index $args]
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.LoadBlock {f fargs} {                          # gif.load
#-----------------------------------------------------------------------------
 foreach {format varName} $fargs {
   switch -exact -- $format {
     unsigned {
       binary scan [read $f 2] s val
       set val [expr {$val & 0xFFFF}]
       uplevel 1 [list set $varName $val]
       }
     byte {
       binary scan [read $f 1] c val
       set val [expr {$val & 0xFF}]
       uplevel 1 [list set $varName $val]
       }
     packed {
       uplevel 1 [list gif.LoadPacked $f $varName]
       }
     aspect {
       binary scan [read $f 1] c val
       set val [expr {$val & 0xFF}]
       set n [expr {($val) ? (($val +15) /64.0) : 0}]
       uplevel 1 [list set $varName $n]
       }
     }
   }
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.LoadColorTable {f size} {                      # gif.load
#-----------------------------------------------------------------------------
 set result {}
 incr size
 while {[incr size -1]} {
   gif.LoadBlock $f {
     byte red
     byte green
     byte blue
     }
   lappend result [list $red $green $blue]
   }
 return $result
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.LoadPacked {f fargs} {                         # gif.load
#-----------------------------------------------------------------------------
 binary scan [read $f 1] c data
 foreach {format varName} $fargs {
   switch -exact -- $format {
     0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 {
       set n [expr (($data >> $format) & 1) ? true : false]
       }
     color.resolution { set n [expr {(($data >> 4) & 0x7) +1}] }
     color.table.size { set n [expr {int( pow( 2, ($data & 0x7) +1 ))}] }
     default {
       if {![string is integer -strict $format] || (10 > $format) || ($format > 76)} {
         return -code error {invalid packed bitfield specification}
         }
       set length [expr [string index $format 0] -[string index $format 1] +1]
       set index [lsearch -exact {2 3 4 5 6 7} $length]
       if {$index < 0} {
         return -code error {invalid packed bitfield specification}
         }
       set mask [lindex {0x3 0x7 0xF 0x1F 0x3F 0x7F} $index]
       set n [expr ($data >> [string index $format 1]) & $mask]
       }
     }
   uplevel 1 [list set $varName $n]
   }
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.LoadSubBlocks {f mode} {                       # gif.load
#-----------------------------------------------------------------------------
# mode := unpack | leavepacked
 set result {}
 for {gif.LoadBlock $f {byte size}} {$size} {gif.LoadBlock $f {byte size}} {
   set data [read $f $size]
   if {$mode eq {unpack}} \
     then { append result $data} \
     else {lappend result $data}
   }
 return $result
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.ValidateBlock {varName requireds optionals} {  # gif.save
#-----------------------------------------------------------------------------
 upvar 1 $varName block
 foreach name $requireds {
   if {![info exists block($name)] || ($block($name) eq {})} {
     return -code error "$block(type) requires element '$name'"
     }
   }
 foreach name $optionals {
   set elt [lindex $name 0]
   set val [lindex $name 1]
   if {![info exists block($elt)] || ($block($elt) eq {})} {
     set block($elt) $val
     }
   }
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.WriteBlock {f fargs} {                         # gif.save
#-----------------------------------------------------------------------------
 foreach {format value} $fargs {
   switch -exact -- $format {
     unsigned {puts -nonewline $f [binary format s $value]}
     byte     {puts -nonewline $f [binary format c $value]}
     packed   {gif.WritePacked $f $value}
     aspect   {
       if {$value != 0} {
         set value [expr {(int( ($value *64.0) +0.5 ) -15) & 0xFF}]
         }
       puts -nonewline $f [binary format c $value]
       }
     default  {return -code error {invalid field specification}}
     }
   }
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.WriteColorTable {f colors size} {              # gif.save
#-----------------------------------------------------------------------------
 set count [llength $colors]
 for {set cntr 0} {$cntr < $count} {incr cntr} {
   gif.WriteBlock $f "
     byte [lindex $colors $cntr 0]
     byte [lindex $colors $cntr 1]
     byte [lindex $colors $cntr 2]
     "
   }
 while {$cntr < $size} {
   gif.WriteBlock $f {byte 0 byte 0 byte 0}
   incr cntr
   }
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.WritePacked {f fargs} {                        # gif.save
#-----------------------------------------------------------------------------
 set result 0
 foreach {format value} $fargs {
   switch -exact -- $format {
     0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 {
       set value [expr {!!$value}]
       set result [expr {$result | ($value << $format)}]
       }
     color.resolution {
       set result [expr {$result | ((($value -1) & 0x7) << 4)}]
       }
     default {
       if {![string is integer -strict $format] || (10 > $format) || ($format > 76)} {
         return -code error {invalid packed bitfield specification}
         }
       set length [expr [string index $format 0] -[string index $format 1] +1]
       set index [lsearch -exact {2 3 4 5 6 7} $length]
       if {$index < 0} {
         return -code error {invalid packed bitfield specification}
         }
       set mask [lindex {0x3 0x7 0xF 0x1F 0x3F 0x7F} $index]
       set result [expr {$result | (($value & $mask) << [string index $format 1])}]
       }
     }
   }
 puts -nonewline $f [binary format c $result]
 }

#-----------------------------------------------------------------------------
proc ::gifblock::gif.WriteSubBlocks {f data mode} {                 # gif.save
#-----------------------------------------------------------------------------
# mode := pack | prepacked
# Does NOT write a sub-block terminator
#
 if {$mode eq {pack}} {
   set length [string length $data]
   while {$length > 0} {
     if {$length >= 255} \
       then {
         gif.WriteBlock $f {byte 255}
         puts -nonewline $f [string range $data 0 254]
         set data [string range $data 255 end]
         incr length -255
         } \
       else {
         gif.WriteBlock $f "byte $length"
         puts -nonewline $f $data
         set length 0
         }
     }
   return
   }

 foreach subblock $data {
   gif.WriteBlock $f [string length $subblock]
   puts -nonewline $f $subblock
   }
 }

#-----------------------------------------------------------------------------
#  Add a new block by cloning an existing one. Obtain the block
#  using gif.get.
#
proc ::gifblock::gif.add {varName block} {
#-----------------------------------------------------------------------------
   upvar 1 $varName result
   lappend result $block
}

#-----------------------------------------------------------------------------
#  Create a Graphic Control block. Delay in 1/100 of second.
#
proc ::gifblock::gif.createGC {varName {delay 50}} {
#-----------------------------------------------------------------------------
   upvar 1 $varName result

   set temp [list \
                type              {Graphic Control} \
                reserved          0 \
                {disposal method} 0 \
                {user input?}     0 \
                {delay time}      $delay \
               ]
   lappend result $temp
}
