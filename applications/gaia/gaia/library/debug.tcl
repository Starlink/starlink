#  Code for stack trace at a point.
for { set i [info level] } { $i > -1 } { incr i -1 } { 
   puts "$i: [info level $i]"
}
