#!/bin/tcsh

# This script relies on having two versions of exercise.f - one (exerciseo)
# linked against a 'known good' versio of the HDS library. The second
# (exercise) should be linked against the current development version
# 
# This script runs all the tests and compares the output.
# NB This avoids the problems in the reference output supplied by RFWS
# in that some fine details of the output formatting are machine dependent!
#
# BKM, Starlink, RAL

foreach i (*_input)
exerciseo <$i >/dev/null
mv exercise.result reference.result
echo $i
exercise <$i >/dev/null
diff exercise.result reference.result
end
