#  "sed" script to filter direct test output to remove machine dependencies.
#
#  Remove directory names of the form /.../.../ from the start of file names.
s|/.*/||
#
#  Remove locator value and length information from error messages about
#  invalid locators.
s|locator invalid: value='.*', length=..|locator invalid: value='?', length=?|
#
#  Some platforms format some numbers differently:
s|2.E-02|0.02|
#
#  Remove redundant trailing spaces from lines (some Fortran compilers may add
#  them).
s|  *$||
