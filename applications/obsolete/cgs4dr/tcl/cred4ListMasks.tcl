proc cred4ListMasks {taskname} {
#+
# Lists bad pixel masks
#-
  global env

# Set lists for DST and NDF masks
  set bpmd \
    [list baddy.dst goody.dst fpa61.dst fpa61_75.dst fpa61_lc.dst mic33k.dst morebad.dst noisy33k.dst noisy35k.dst short33k.dst]
  set bpmn \
    [list baddy.sdf goody.sdf fpa61.sdf fpa61_75.sdf fpa61_lc.sdf mic33k.sdf morebad.sdf noisy33k.sdf noisy35k.sdf short33k.sdf]

# Check for different DST masks
  foreach file [glob $env(CGS4DR_ROOT)/dst/*.dst] {
   set filename [file rootname [file tail $file]]
   if {[file exists $env(CGS4DR_ROOT)/ndf/${filename}.sdf] != 1} {set bpmd "${bpmd} [list ${filename}.dst]"}
  }

# Check for different NDF masks
  foreach file [glob $env(CGS4DR_ROOT)/ndf/*.sdf] {
   set filename [file rootname [file tail $file]]
   if {[file exists $env(CGS4DR_ROOT)/dst/${filename}.dst] != 1} {set bpmn "${bpmn} [list ${filename}.sdf]"}
  }

# Output the results
  cgs4drInform $taskname "Bad Pixel or Window Masks are:\n"
  cgs4drInform $taskname "DST: [lsort $bpmd]\n"
  cgs4drInform $taskname "NDF: [lsort $bpmn]\n"
}
