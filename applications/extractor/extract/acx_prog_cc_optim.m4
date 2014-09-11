dnl
dnl				acx_prog_cc_optim.m4
dnl
dnl Enable a reasonable set of optimization flags for the C compiler. 
dnl
dnl %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dnl
dnl	This file part of:	AstrOmatic software
dnl
dnl	Copyright:		(C) 2002-2013 Emmanuel Bertin -- IAP/CNRS/UPMC
dnl				(C) 2002 Ville Lauriki (original version)
dnl
dnl	Licenses:		GPL (this version)
dnl				MIT AllPermissive (original script)
dnl
dnl	AstrOmatic software is free software: you can redistribute it and/or
dnl	modify it under the terms of the GNU General Public License as
dnl	published by the Free Software Foundation, either version 3 of the
dnl	License, or (at your option) any later version.
dnl	AstrOmatic software is distributed in the hope that it will be useful,
dnl	but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl	GNU General Public License for more details.
dnl	You should have received a copy of the GNU General Public License
dnl	along with AstrOmatic software.
dnl	If not, see <http://www.gnu.org/licenses/>.
dnl
dnl	Last modified:		13/03/2013
dnl
dnl %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dnl
dnl @synopsis ACX_PROG_CC_OPTIM
dnl
dnl Currently this macro knows about GCC, Solaris C compiler,
dnl Digital Unix C compiler, C for AIX Compiler, HP-UX C compiler,
dnl IRIX C compiler, NEC SX-5 (Super-UX 10) C compiler, and Cray J90
dnl (Unicos 10.0.0.8) C compiler.
dnl
dnl This macro is a modification of Ville Laurikari's VL_PROG_CC_WARNINGS
dnl

AC_DEFUN([ACX_PROG_CC_OPTIM], [
  msg="for C compiler optimization flags"
  AC_CACHE_CHECK($msg, prog_cc_optim_cv_flags, [
    if test -n "$CC"; then
      cat > conftest.c <<EOF
int main(int argc, char **argv) { return 0; }
EOF

      dnl Most compilers print some kind of a version string with some command
      dnl line options (often "-V").  The version string should be checked
      dnl before doing a test compilation run with compiler-specific flags.
      dnl This is because some compilers (like the Cray compiler) only
      dnl produce a warning message for unknown flags instead of returning
      dnl an error, resulting in a false positive.  Also, compilers may do
      dnl erratic things when invoked with flags meant for a different
      dnl compiler.

      dnl INTEL C 64bits compiler
      if $CC -V 2>&1 | grep -i "Intel(R) 64" > /dev/null 2>&1 &&
           $CC -c -O conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O3 -axSSSE3,SSE4.1,SSE4.2,AVX,CORE-AVX2,CORE-AVX-I -no-prec-div -unroll"
        prog_ld_optim_cv_flags=""

      dnl INTEL C 32bits compiler
      elif $CC -V 2>&1 | grep -i "Intel(R)" > /dev/null 2>&1 &&
           $CC -c -O conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O3 -axSSE2,SSE3,SSE4.1,SSE4.2,AVX,CORE-AVX2,CORE-AVX-I -no-prec-div -unroll"
        prog_ld_optim_cv_flags=""

      dnl GCC
      elif test "$GCC" = "yes"; then
        prog_cc_optim_cv_flags="-O3 -g -funroll-loops -fomit-frame-pointer -Wall"
        prog_ld_optim_cv_flags=""

      dnl Solaris C compiler
      elif $CC -V 2>&1 | grep -i "WorkShop" > /dev/null 2>&1 &&
           $CC -c -O conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O"
        prog_ld_optim_cv_flags=""

      dnl Digital Unix/Compaq C compiler
      elif ($CC -V 2>&1 | grep -i "Digital UNIX Compiler"> /dev/null 2>&1 ||
	   $CC -V 2>&1 | grep -i "Compaq C"> /dev/null 2>&1) &&
           $CC -c -fast conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
         prog_cc_optim_flags="-fast"
        prog_ld_optim_cv_flags=""

      dnl C for AIX Compiler
      elif $CC 2>&1 | grep -i "C for AIX Compiler" > /dev/null 2>&1 &&
           $CC -c -qinfo=all -O2 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O2"
        prog_ld_optim_cv_flags=""

      dnl IRIX C compiler
      elif $CC -version 2>&1 | grep -i "MIPSpro Compilers" > /dev/null 2>&1 &&
           $CC -c -fullwarn -O3 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O3"
        prog_ld_optim_cv_flags=""

      dnl HP-UX C compiler
      elif what $CC 2>&1 | grep -i "HP C Compiler" > /dev/null 2>&1 &&
           $CC -c -Aa +O3 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="+O3"
        prog_ld_optim_cv_flags=""

      dnl The NEC SX-5 (Super-UX 10) C compiler
      elif $CC -V 2>&1 | grep "/SX" > /dev/null 2>&1 &&
           $CC -c -Xc -O conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O"
        prog_ld_optim_cv_flags=""

      dnl The Cray C compiler (Unicos)
      elif $CC -V 2>&1 | grep -i "Cray" > /dev/null 2>&1 &&
           $CC -c -h conform -O3 conftest.c > /dev/null 2>&1 &&
           test -f conftest.o; then
        prog_cc_optim_cv_flags="-O3"
        prog_ld_optim_cv_flags=""

      fi
      rm -f conftest.*
    fi
    if test -n "$prog_cc_optim_cv_flags"; then
      AM_CFLAGS="$CFLAGS $prog_cc_optim_cv_flags"
      AM_LDFLAGS="$LDFLAGS $prog_ld_optim_cv_flags"
    else
      prog_cc_optim_cv_flags=""
      prog_ld_optim_cv_flags=""
    fi
  ])
])dnl

