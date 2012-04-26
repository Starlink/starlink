#  "sed" script to filter regression test output to remove variable values.
#
#  Remove locator value information from error messages about invalid locators.
s|locator invalid: value='.*', length=|locator invalid: value='?', length=|
#
#  Remove redundant trailing spaces from lines (some Fortran compilers may add
#  them).
s|  *$||
