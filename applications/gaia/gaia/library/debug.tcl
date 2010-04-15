#  Code for stack trace at a point.
for { set i [info level] } { $i > -1 } { incr i -1 } {
   puts "$i: [info level $i]"
}

# Example trap of function usage (wm calls)
rename wm wm_orig

proc wm {args} {
   puts "wm args + $args"
   if { [string match "geom*" $args] } {
      #  Code for stack trace at a point.
      for { set i [info level] } { $i > -1 } { incr i -1 } {
         puts "$i: [info level $i]"
      }
   }
   eval wm_orig $args
}
