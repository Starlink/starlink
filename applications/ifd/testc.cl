# IRAF package initialisation script for the  ADAM test package
# Created automatically from test.ifd using ifd2iraf
# 18Feb97

package test

cl < "starlink$irafstar/zzsetenv.def"

task test1 = "test$test.e"
task test2 = "test$test.e"

clbye()
