proc ParseFile {fileName n} {set fd [open $fileName {RDONLY}]
set lineNum 1
puts "static char script_$n\[\] = \{"
set N [format \n]
set T [format \t]
set NTS [format "\n\t\ "]
set sep ""
while {![eof $fd]} {set line [gets $fd]
regsub -all $N $line " " foo
append foo \na
set foo [subst -nocommands -novariables $foo]
if [regexp $N $foo] {set cmd "$line\n"
} else {regsub -all \\\\\[$NTS\]*$ $line " " line
set cmd "$line"
}
set cmd '[join [split $cmd ""] ',']'
regsub -all \\\\ $cmd \\\\\\\\ cmd
regsub -all $N $cmd \\n cmd
regsub -all $T $cmd \\t cmd
regsub -all ''' $cmd '\\'' cmd
regsub -all '\"' $cmd '\\\"' cmd
puts -nonewline $sep$cmd
set sep ,\n
}
puts "$sep'\\0'\};"
close $fd
}
proc tclc_Main {} {global argv argv0
set files [lrange $argv 0 end]
set n 0
foreach fileName $argv {ParseFile $fileName $n
incr n
}
puts "static int LoadScripts(interp)"
puts "    Tcl_Interp * interp;"
puts "\{"
if {$n > 0} {puts "    char *scripts\[$n\];"
puts "    int i;"
for {set k 0} {$k < $n} {incr k} {puts "    scripts\[$k\] = script_$k;"
}
puts "    for (i=0; i<$n; i++) \{"
puts "        if (Tcl_Eval(interp, scripts\[i\]) != TCL_OK) \{"
puts "            return TCL_ERROR;"
puts "        \}"
puts "    \}"
}
puts "    return TCL_OK;"
puts "\}"
}

proc jdb_ParseFile {fileName rewriteProc} {global jdbLines
if [info exists jdbLines] {unset jdbLines
}
set data ""
set fd [open $fileName {RDONLY}]
set fileLineNum  1
set scapLineNum  1
set fileLineNumx 1
set newLine [format \n]
set NTS [format "\n\t\ "]
while {![eof $fd]} {set line [gets $fd]
regsub -all $newLine $line " " foo
append foo \na
set foo [subst -nocommands -novariables $foo]
if [regexp $newLine $foo] {append data "$line\n"
set jdbLines($scapLineNum) $fileLineNumx
incr scapLineNum
incr fileLineNum
set fileLineNumx $fileLineNum
} else {regsub -all \\\\\[$NTS\]*$ $line " " line
append data "$line"
incr fileLineNum
}
}
close $fd
global jdb_rewProc
set jdb_rewProc $rewriteProc
return [jdb_Rewrite $fileName 1 $data]
}
proc jdb_BreakCommand {lineNum script wordsName typesName lnumsName} {upvar $wordsName words
upvar $typesName types
upvar $lnumsName lnums
set i 0
set word ""
set sep ""
set N [format %s \n]
foreach item [split $script " "] {append word $sep$item
if ![string comp [string trim $word] ""] {continue
}
if [info complete $word] {set n [regsub -all $N $word "" dummy]
set tmp [string trim $word]
set len [string len $tmp]
if {[string index $tmp 0] == "\{" &&
		    [string index $tmp [expr $len-1]] == "\}"} {set word [string range $tmp 1 [expr $len-2]]
set itemType brace
} elseif {[string index $tmp 0] == "\"" &&
		    [string index $tmp [expr $len-1]] == "\""} {set word [string range $tmp 1 [expr $len-2]]
set itemType quote
} else {set itemType none
}
set lnums($i) $lineNum
set types($i) $itemType
set words($i) $word
incr lineNum $n
incr i
set word ""
set sep ""
} else {set sep " "
}
}
if [string comp [string trim $word] ""] {error "badly formatted script\n$script"
}
}
proc jdb_Recurse {file lineNum script} {global builtin
set cmd [lindex [split $script " "]  0]
if [info exists builtin($cmd)] {set script [$builtin($cmd) $file $lineNum $script]
}
return $script
}
proc jdb_JoinCommand {wordsName typesName} {upvar $wordsName words
upvar $typesName types
set rwt ""
set sep ""
foreach i [lsort -integer [array names words]] {case $types($i) {
	brace {append rwt "$sep\{"
append rwt "$words($i)"
append rwt "\}"
} quote {append rwt "$sep\""
append rwt "$words($i)"
append rwt "\""
} default {append rwt "$sep$words($i)"
}}
set sep " "
}
return $rwt
}
proc jdb_Rewrite {file lineNum script} {global jdb_rewProc xxx
set rewritten ""
set cmd ""
foreach line [split $script \n] {append cmd $line\n
if [info complete $cmd] {append rewritten [$jdb_rewProc $file $lineNum $cmd]
incr lineNum [regsub -all [format %s \n] $cmd "" dummy]
set cmd ""
}
}
if [string comp $cmd {}] {error "Script is not complete: \n$script"
} else {return $rewritten
}
}
set builtin(catch)   jdb_RewriteCatch
set builtin(case)    jdb_RewriteCase
set builtin(for)     jdb_RewriteFor
set builtin(foreach) jdb_RewriteForeach
set builtin(if)      jdb_RewriteIf
set builtin(proc)    jdb_RewriteProc
set builtin(while)   jdb_RewriteWhile
proc jdb_RewriteCatch {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
if [info exists words(1)] {set words(1) [jdb_Rewrite $file $lnums(1) $words(1)]
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteCase {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
set indices [lsort -integer [array names words]]
if [info exists words(2)] {if ![string comp $words(2) in] {set list [lrange $indices 3 end]
} else {set list [lrange $indices 2 end]
}
if {[llength $list] > 1} {set len [llength $list]
for {set x 1} {$x < $len} {incr x 2} {set i [lindex $list $x]
set words($i) [jdb_Rewrite $file $lnums($i) $words($i)]
}
} else {set i [lindex $list 0]
set words($i) [jdb_RewriteCaseBodyList $file $lnums($i) $words($i)]
}
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteCaseBodyList {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
set indices [lsort -integer [array names words]]
set len [llength $indices]
for {set x 1} {$x < $len} {incr x 2} {set i [lindex $indices $x]
set words($i) [jdb_Rewrite $file $lnums($i) $words($i)]
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteIf {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
set expected if
foreach i [lsort -integer [array names words]] {set ln   $lnums($i)
set item $words($i)
case $expected {
	if {set expected expr
} expr {set expected stmt
} stmt {if [string comp [string trim $item] "then"] {set words($i) [jdb_Rewrite $file $ln $item]
set expected el_elif
}
} el_elif {if {$item == "elseif"} {set expected expr
} else {set expected stmt
}
}}
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteProc {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
if [info exists words(3)] {set words(3) [jdb_Rewrite $file $lnums(3) $words(3)]
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteWhile {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
if [info exists words(2)] {set words(2) [jdb_Rewrite $file $lnums(2) $words(2)]
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteFor {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
if [info exists words(4)] {set words(4) [jdb_Rewrite $file $lnums(4) $words(4)]
}
return [jdb_JoinCommand words types]
}
proc jdb_RewriteForeach {file lineNum script} {jdb_BreakCommand $lineNum $script words types lnums
if [info exists words(3)] {set words(3) [jdb_Rewrite $file $lnums(3) $words(3)]
}
return [jdb_JoinCommand words types]
}

tclc_Main
