proc cgs4drStrRev {input} {
#+
# Reverses an input string
#-
  set string [string trim $input]
  set strlen [string length $string]
  set newstr ""
  while {$strlen>=0} {
    append newstr [string index $string $strlen]
    incr strlen -1
  }
  return $newstr
}
