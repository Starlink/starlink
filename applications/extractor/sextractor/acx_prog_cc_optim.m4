dnl @synopsis ACX_PROG_CC_OPTIM
dnl
dnl Enables a reasonable set of optimization flags for the C compiler. 
dnl
dnl Currently this macro knows about GCC, Solaris C compiler,
dnl Digital Unix C compiler, C for AIX Compiler, HP-UX C compiler,
dnl IRIX C compiler, NEC SX-5 (Super-UX 10) C compiler, and Cray J90
dnl (Unicos 10.0.0.8) C compiler.
dnl
dnl This macro is a modification of Ville Laurikari's VL_PROG_CC_WARNINGS
dnl @version 1.0 (2002-04-15)
dnl @authors Emmanuel Bertin <bertin@iap.fr> Ville Laurikari <vl@iki.fi>
dnl
AC_DEFUN([ACX_PROG_CC_OPTIM], [
  msg="for C compiler optimization flags"
  AC_CACHE_CHECK($msg, prog_cc_optim_flags, [
    if test -n "$CC"; then
      cat > conftest.c <<EOF
int main(int argc, char **argv) { return 0; }
EOF

      dnl GCC
      if test "$GCC" = "yes"; then
        prog_cc_optim_flags="-funroll-loops -O3 -g"

      dnl Most compilers print some kind of a version string with some command
      dnl line options (often "-V").  The version string should be checked
      dnl before doing a test compilation run with compiler-specific flags.
      dnl This is because some compilers (like the Cray compiler) only
      dnl produce a warning message for unknown flags instead of returning
      dnl an error, resulting in a false positive.  Also, compilers may do
      dnl erratic things when invoked with flags meant for a different
      dnl compiler.

      dnl Solaris C compiler
      elif $CC -V 2>&1 | grep -i "WorkShop" > /dev/null 2>&1 &&
           $CC -c -O conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_flags="-O"

      dnl Digital Unix/Compaq C compiler
      elif ($CC -V 2>&1 | grep -i "Digital UNIX Compiler"> /dev/null 2>&1 ||
	   $CC -V 2>&1 | grep -i "Compaq C"> /dev/null 2>&1) &&
           $CC -c -fast conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
         prog_cc_optim_flags="-fast -tune host"

      dnl C for AIX Compiler
      elif $CC 2>&1 | grep -i "C for AIX Compiler" > /dev/null 2>&1 &&
           $CC -c -qinfo=all -O2 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_flags="-O2"

      dnl IRIX C compiler
      elif $CC -version 2>&1 | grep -i "MIPSpro Compilers" > /dev/null 2>&1 &&
           $CC -c -fullwarn -O3 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_flags="-O3"

      dnl HP-UX C compiler
      elif what $CC 2>&1 | grep -i "HP C Compiler" > /dev/null 2>&1 &&
           $CC -c -Aa +O3 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_flags="+O3"

      dnl The NEC SX-5 (Super-UX 10) C compiler
      elif $CC -V 2>&1 | grep "/SX" > /dev/null 2>&1 &&
           $CC -c -Xc -O conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_flags="-O"

      dnl The Cray C compiler (Unicos)
      elif $CC -V 2>&1 | grep -i "Cray" > /dev/null 2>&1 &&
           $CC -c -h conform -O3 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_flags="-O3"

      fi
      rm -f conftest.*
    fi
    if test -n "$prog_cc_optim_flags"; then
      CFLAGS="$CFLAGS $prog_cc_optim_flags"
    else
      prog_cc_optim_flags="unknown"
    fi
  ])
])dnl
