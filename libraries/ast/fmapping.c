
/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */

/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */

/* wchar_t uses Unicode 7.0.0.  Version 7.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2012, plus Amendments 1 (published
   on April, 2013) and 2 (not yet published as of February, 2015).
   Additionally, it includes the accelerated publication of U+20BD
   RUBLE SIGN.  Therefore Unicode 7.0.0 is between 10646:2012 and
   10646:2014, and so we use the date ISO/IEC 10646:2012 Amd.1 was
   published.  */


/*  We do not support C11 <threads.h>. */

/*
*+
*  Name:
*     fmapping.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Mapping class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Mapping class.

*  Routines Defined:
*     AST_DECOMPOSE
*     AST_INVERT
*     AST_ISAMAPPING
*     AST_LINEARMAPPING
*     AST_REBIN<X>
*     AST_REBINSEQ<X>
*     AST_MAPBOX
*     AST_MAPSPLIT
*     AST_RATE
*     AST_REMOVEREGIONS
*     AST_RESAMPLE<X>
*     AST_SIMPLIFY
*     AST_TRAN1
*     AST_TRAN2
*     AST_TRANGRID
*     AST_TRANN
*     AST_RATE

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     11-JUL-1996 (RFWS):
*        Original version.
*     13-DEC-1996 (RFWS)
*        Added AST_SIMPLIFY.
*     28-MAY-1998 (RFWS):
*        Added AST_MAPBOX.
*     12-NOV-1998 (RFWS):
*        Added AST_RESAMPLE<X>.
*     22-NOV-2000 (DSB):
*        Pass the "flags" argument by reference instead of by value in the
*        MAKE_AST_RESAMPLE_UINTERP macro.
*     9-JAN-2001 (DSB):
*        Changed in and out arguments for TranN from type "double (*)[]"
*        to "double *".
*     26-SEP-2001 (DSB):
*        Added AST_DECOMPOSE.
*     16-JUL-2003 (DSB):
*        Added AST_RATE.
*     30-JUN-2005 (DSB):
*        Added AST_REBIN<X>.
*     1-SEP-2005 (DSB):
*        Added AST_REBINSEQ<X>.
*     8-MAR-2006 (DSB):
*        Added AST_TRANGRID.
*     5-MAY-2009 (DSB):
*        Added AST_REMOVEREGIONS.
*     4-MAY-2010 (DSB):
*        Add support for AST__VARWGT flag to AST_REBINSEQ<X>.
*/

/* Define the astFORTRAN77 macro which prevents error messages from
   AST C functions from reporting the file and line number where the
   error occurred (since these would refer to this file, they would
   not be useful). */


/*  Header files. */


/*  ============= */

/*
*+
*  Name:
*     f77.h and cnf.h

*  Purpose:
*     C - FORTRAN interace macros and prototypes

*  Language:
*     C (part ANSI, part not)

*  Type of Module:
*     C include file

*  Description:
*     For historical reasons two files, F77.h and cnf.h are required
*     but the have now been combined and for new code, only one is
*     necessary.
*
*     This file defines the macros needed to write C functions that are
*     designed to be called from FORTRAN programs, and to do so in a
*     portable way. Arguments are normally passed by reference from a
*     FORTRAN program, and so the F77 macros arrange for a pointer to
*     all arguments to be available. This requires no work on most
*     machines, but will actually generate the pointers on a machine
*     that passes FORTRAN arguments by value.

*  Notes:
*     -  Macros are provided to handle the conversion of logical data
*        values between the way that FORTRAN represents a value and the
*        way that C represents it.
*     -  Macros are provided to convert variables between the FORTRAN and
*        C method of representing them. In most cases there is no
*        conversion required, the macros just arrange for a pointer to
*        the FORTRAN variable to be set appropriately. The possibility that
*        FORTRAN and C might use different ways of representing integer
*        and floating point values is considered remote, the macros are
*        really only there for completeness and to assist in the automatic
*        generation of C interfaces.
*     -  For character variables the macros convert between
*        the FORTRAN method of representing them (fixed length, blank
*        filled strings) and the C method (variable length, null
*        terminated strings) using calls to the CNF functions.

*  Implementation Deficiencies:
*     -  The macros support the K&R style of function definition, but
*        this file may not work with all K&R compilers as it contains
*        "#if defined" statements. These could be replaced with #ifdef's
*        if necessary. This has not been done as is would make the code
*        less clear and the need for support for K&R sytle definitions
*        should disappear as ANSI compilers become the default.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007,2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (PMA):
*        Original version.
*     19-JUN-1991 (PMA):
*        Removed VMS versions of IM(EX)PORT_LOGICAL macros that tried
*        to convert data representations.
*     24-JUN-1991 (PMA):
*        Changed the names of IMPORT macros to GENPTR.
*        Removed the EXPORT macros.
*     27-JUN-1991 (PMA):
*        Modified DECstation specific stuff to allow use of the c89
*        compiler.
*      8-JUL-1991 (PMA):
*        Added macros to call FORTRAN from C.
*     16-OCT-1991 (PMA):
*        Remove type_ARRAY2 definitions.
*        Remove the length argument from CHARACTER_ARRAY and the
*        dimension specifier from GENPTR_type_ARRAY.
*        Add extra brackets to F77_ISFALSE and F77_ISTRUE.
*     25-OCT-1991 (PMA):
*        Changed "if defined(sun4)" to "if defined(sun)"
*     2-JUN-1992 (PMA):
*        Changed "if defined(mips)" to "if defined(ultrix)" to prevent
*        those definitions being used on a Silicon Graphics machine.
*     11-JUN-1992 (PMA):
*        Changed "if defined(ultrix)" back to "if defined(mips)" so that
*        it still works on OSF/1 on a DECstation.
*        Add support for general non-ANSI compilers, but not basic K&R
*        ones.
*     12-JUN-1992 (PMA):
*        Change declaration of dummy scalar arguments to be const
*        pointers.  Change declaration of dummy array arguments to be
*        const pointers.
*     5-JAN-1993 (PMA):
*        Changed "if defined(mips)" so that it will recognise a
*        DECstation running Ultrix or OSF/1, but not a Silicon Graphics
*        workstation.
*        Change the definition of F77_BYTE_TYPE to add "signed".
*        Redefine this on VMS where signed is invalid syntax.
*        Add new types of UBYTE and UWORD.
*     8-JAN-1993 (PMA):
*        Fix bug in the definition of CHARACTER_RETURN_VALUE. There was
*        an extraneous space.
*        Add a macro F77_POINTER_TYPE and use it to define POINTER.
*     13-JAN-1993 (PMA):
*        Start to add support for K&R function definitions. These are
*        done on a per machine basis.
*     16-APR-1993 (PMA):
*        Change the definition of F77_POINTER_TYPE from int to unsigned
*        int.
*     7-MAY-1993 (PMA):
*        Change from using a null comment as a token concatenation
*        operator to using the internal macro _f77_x on non-ANSI
*        systems.
*     10-MAY-1993 (PMA):
*        Finish adding K&R support. This will form version 2.0 of F77.
*     10-MAY-1993 (PMA):
*        Add support for Alpha OSF/1.
*     9-JUL-1993 (PMA):
*        Add further POINTER macros: POINTER_ARRAY,
*        GENPTR_POINTER_ARRAY, DECLARE_POINTER, DECLARE_POINTER_ARRAY,
*        POINTER_ARG, POINTER_ARRAY_ARG, F77_POINTER_FUNCTION,
*        KR_POINTER_ARRAY.
*     24-AUG-1993 (PMA):
*        Add const to the VMS definitions of CHARACTER and CHARACTER_ARRAY.
*     3-NOV-1993 (PMA):
*        Remove K&R stuff to a separate file.
*        Released on Unix as version 2.0 of CNF.
*     11-NOV-1993 (PMA):
*        Return to using the null comment to concatenate text on non-ANSI
*        systems as _f77_x caused problems with the c89 -common flag on
*        DECstations.
*     23-JAN-1996 (AJC):
*        Add SUBROUTINE, type_FUNCTION, SUBROUTINE_ARG,
*        type_FUNCTION_ARG, GENPTR_SUBROUTINE and GENPTR_type_FUNCTION
*        required for passed subroutine and function name.
*     29-JAN-1996 (AJC):
*        Add the dynamic CHARACTER_ macros
*        and CHARACTER_ARG_TYPE
*     22-FEB-1996 (AJC):
*        Add CHARACTER_RETURN_ARG
*     23-MAY-1996 (AJC):
*        Add DECLARE_CHARACTER_ARRAY_DYN
*            F77_CREATE_CHARACTER_ARRAY
*            F77_CHARACTER_ARG_TYPE
*     14-JUN-1996 (AJC):
*        Add DECLARE_LOGICAL_ARRAY_DYN
*            F77_CREATE_LOGICAL_ARRAY
*     21-JUN-1996 (AJC):
*        Add cast to _ARRAY_ARGs to allow multidimensional arrays
*     17-MAR-1998 (AJC):
*        Add DECLARE, CREATE and FREE dynamic array macros for all types
*        Changed CREATE_CHARACTER_ARRAY and CREATE_LOGICAL_ARRAY to use
*         number of elements rather than dimensions.
*        Add IMPORT, EXPORT and ASSOC macros
*     22-JUL-1998 (AJC):
*        Combined F77.h and cnf.h
*     23-SEP-1998 (AJC):
*        Input strings for cnf -> const char *
*        Input int arrays for cnf -> const int *
*      4-NOV-1998 (AJC):
*        Bring cnf prototypes in line with .c routines
*      8-FEB-1999 (AJC):
*        Added cnf_mem stuff
*      9-FEB-1999 (AJC):
*        Use cnf_cptr/fptr for IMPORT/EXPORT_POINTER
*     16-FEB-1999 (AJC):
*        Added missing cnf_fptr prototype
*     23-JUN-1999 (AJC):
*        Change cnf_name to cnfName
*        and add macros for cnf_name
*      1-DEC-1999 (AJC):
*        Add define cnf_free
*      7-JAN-2000 (AJC):
*        Correct omission of F77_ASSOC_UBYTE_ARRAY
*        Correct F77_EXPORT_UWORD_ARRAY
*      25-AUG-2005 (TIMJ):
*        Add cnfInitRTL
*      23-FEB-2006 (TIMJ):
*        Add cnfRealloc
*        Use starMalloc rather than malloc in F77_CREATE_POINTER_ARRAY
*        (since it needs to match what goes on in cnfFree)
*      21-JUN-2006 (PWD):
*        Changed to use a different return type for REAL functions. This
*        effects g77 under 64-bit, when the f2c bindings expect the return
*        value of a REAL function to be a double, not a float.
*      25-SEP-2006 (PWD):
*        Introduced F77_CREATE_IMPORT_CHARACTER. Match length of
*        F77_CREATE_CHARACTER to result from cnfCref.
*      13-JUL-2007 (PWD):
*        Parameterise the type of Fortran character string lengths. Can
*        be long.
*      7-OCT-2008 (TIMJ):
*        Initialise pointers.
*      11-MAY-2011 (DSB):
*        Added F77_LOCK
*     {enter_further_changes_here}
*

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.20 General utilities	<stdlib.h>
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* These are defined by the user (or the compiler)
   to specify the desired environment:

   __STRICT_ANSI__	ISO Standard C.
   _ISOC99_SOURCE	Extensions to ISO C89 from ISO C99.
   _ISOC11_SOURCE	Extensions to ISO C99 from ISO C11.
   _POSIX_SOURCE	IEEE Std 1003.1.
   _POSIX_C_SOURCE	If ==1, like _POSIX_SOURCE; if >=2 add IEEE Std 1003.2;
			if >=199309L, add IEEE Std 1003.1b-1993;
			if >=199506L, add IEEE Std 1003.1c-1995;
			if >=200112L, all of IEEE 1003.1-2004
			if >=200809L, all of IEEE 1003.1-2008
   _XOPEN_SOURCE	Includes POSIX and XPG things.  Set to 500 if
			Single Unix conformance is wanted, to 600 for the
			sixth revision, to 700 for the seventh revision.
   _XOPEN_SOURCE_EXTENDED XPG things and X/Open Unix extensions.
   _LARGEFILE_SOURCE	Some more functions for correct standard I/O.
   _LARGEFILE64_SOURCE	Additional functionality from LFS for large files.
   _FILE_OFFSET_BITS=N	Select default filesystem interface.
   _ATFILE_SOURCE	Additional *at interfaces.
   _GNU_SOURCE		All of the above, plus GNU extensions.
   _DEFAULT_SOURCE	The default set of features (taking precedence over
			__STRICT_ANSI__).
   _REENTRANT		Select additionally reentrant object.
   _THREAD_SAFE		Same as _REENTRANT, often used by other systems.
   _FORTIFY_SOURCE	If set to numeric value > 0 additional security
			measures are defined, according to level.

   The `-ansi' switch to the GNU C compiler, and standards conformance
   options such as `-std=c99', define __STRICT_ANSI__.  If none of
   these are defined, or if _DEFAULT_SOURCE is defined, the default is
   to have _POSIX_SOURCE set to one and _POSIX_C_SOURCE set to
   200809L, as well as enabling miscellaneous functions from BSD and
   SVID.  If more than one of these are defined, they accumulate.  For
   example __STRICT_ANSI__, _POSIX_SOURCE and _POSIX_C_SOURCE together
   give you ISO C, 1003.1, and 1003.2, but nothing else.

   These are defined by this file and are used by the
   header files to decide what to declare or define:

   __USE_ISOC11		Define ISO C11 things.
   __USE_ISOC99		Define ISO C99 things.
   __USE_ISOC95		Define ISO C90 AMD1 (C95) things.
   __USE_POSIX		Define IEEE Std 1003.1 things.
   __USE_POSIX2		Define IEEE Std 1003.2 things.
   __USE_POSIX199309	Define IEEE Std 1003.1, and .1b things.
   __USE_POSIX199506	Define IEEE Std 1003.1, .1b, .1c and .1i things.
   __USE_XOPEN		Define XPG things.
   __USE_XOPEN_EXTENDED	Define X/Open Unix things.
   __USE_UNIX98		Define Single Unix V2 things.
   __USE_XOPEN2K        Define XPG6 things.
   __USE_XOPEN2KXSI     Define XPG6 XSI things.
   __USE_XOPEN2K8       Define XPG7 things.
   __USE_XOPEN2K8XSI    Define XPG7 XSI things.
   __USE_LARGEFILE	Define correct standard I/O things.
   __USE_LARGEFILE64	Define LFS things with separate names.
   __USE_FILE_OFFSET64	Define 64bit interface as default.
   __USE_MISC		Define things from 4.3BSD or System V Unix.
   __USE_ATFILE		Define *at interfaces and AT_* constants for them.
   __USE_GNU		Define GNU extensions.
   __USE_REENTRANT	Define reentrant/thread-safe *_r functions.
   __USE_FORTIFY_LEVEL	Additional security measures used, according to level.

   The macros `__GNU_LIBRARY__', `__GLIBC__', and `__GLIBC_MINOR__' are
   defined by this file unconditionally.  `__GNU_LIBRARY__' is provided
   only for compatibility.  All new code should use the other symbols
   to test for features.

   All macros listed above as possibly being defined by this file are
   explicitly undefined if they are not explicitly defined.
   Feature-test macros that are not defined by the user or compiler
   but are implied by the other feature-test macros defined (or by the
   lack of any definitions) are defined by the file.  */


/*  Undefine everything, so we get a clean slate. */

/* Suppress kernel-name space pollution unless user expressedly asks
   for it.  */

/* Convenience macros to test the versions of glibc and gcc.
   Use them like this:
   #if __GNUC_PREREQ (2,8)
   ... code requiring gcc 2.8 or later ...
   #endif
   Note - they won't work for gcc1 or glibc1, since the _MINOR macros
   were not defined then.  */

/* _BSD_SOURCE and _SVID_SOURCE are deprecated aliases for
   _DEFAULT_SOURCE.  If _DEFAULT_SOURCE is present we do not
   issue a warning; the expectation is that the source is being
   transitioned to use the new macro.  */


/*  If _GNU_SOURCE was defined by the user, turn on all the other
    features. */

/* If nothing (other than _GNU_SOURCE and _DEFAULT_SOURCE) is defined,
   define _DEFAULT_SOURCE.  */


/*  This is to enable the ISO C11 extension. */


/*  This is to enable the ISO C99 extension. */


/*  This is to enable the ISO C90 Amendment 1:1995 extension. */

/* This is to enable compatibility for ISO C++11.

   So far g++ does not provide a macro.  Check the temporary macro for
   now, too.  */

/* If none of the ANSI/POSIX macros are defined, or if _DEFAULT_SOURCE
   is defined, use POSIX.1-2008 (or another version depending on
   _XOPEN_SOURCE).  */

/* Get definitions of __STDC_* predefined macros, if the compiler has
   not preincluded this header automatically.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* This macro indicates that the installed library is the GNU C Library.
   For historic reasons the value now is 6 and this will stay from now
   on.  The use of this variable is deprecated.  Use __GLIBC__ and
   __GLIBC_MINOR__ now (see below) when you want to test for a specific
   GNU C library version and use the values in <gnu/lib-names.h> to get
   the sonames of the shared libraries.  */

/* Major and minor version number of the GNU C library package.  Use
   these macros to test for features in specific releases.  */


/*  This is here only because every header file already includes this
    one. */

/* Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  We are almost always included from features.h. */

/* The GNU libc does not support any K&R compilers or the traditional mode
   of ISO C compilers anymore.  Check for some of the combinations not
   anymore supported.  */


/*  Some user header file might have defined this before. */

/* All functions, except those with callbacks or those that
   synchronize memory, are leaf functions.  */

/* GCC can always grok prototypes.  For C++ programs we add throw()
   to help it optimize the function calls.  But this works only with
   gcc 2.8.x and egcs.  For gcc 3.2 and up we even mark C functions
   as non-throwing using a function attribute since programs can use
   the -fexceptions options for C code as well.  */

/* These two macros are not used in glibc anymore.  They are kept here
   only because some other projects expect the macros to be defined.  */

/* For these things, GCC behaves the ANSI way normally,
   and the non-ANSI way under -traditional.  */


/*  This is not a typedef so `const __ptr_t' does the right thing. */


/*  C++ needs to know that types and declarations are C, not C++. */

/* The standard library needs the functions from the ISO C90 standard
   in the std namespace.  At the same time we want to be safe for
   future changes and we include the ISO C99 code in the non-standard
   namespace __c99.  The C++ wrapper header take case of adding the
   definitions to the global namespace.  */

/* For compatibility we do not add the declarations into any
   namespace.  They will end up in the global namespace which is what
   old code expects.  */


/*  Fortify support. */


/*  Support for flexible arrays. */


/*  GCC 2.97 supports C99 flexible array members. */

/* __asm__ ("xyz") is used throughout the headers to rename functions
   at the assembly language level.  This is wrapped by the __REDIRECT
   macro, in order to support compilers that can do this some other
   way.  When compilers don't support asm-names at all, we have to do
   preprocessor tricks instead (which don't have exactly the right
   semantics, but it's the best we can do).

   Example:
   int __REDIRECT(setpgrp, (__pid_t pid, __pid_t pgrp), setpgid); */

/*
#elif __SOME_OTHER_COMPILER__

# define __REDIRECT(name, proto, alias) name proto; 	_Pragma("let " #name " = " #alias)
)
*/

/* GCC has various useful declarations that can be made with the
   `__attribute__' syntax.  All of the ways we use this do fine if
   they are omitted for compilers that don't understand it. */

/* At some point during the gcc 2.96 development the `malloc' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */

/* Tell the compiler which arguments to an allocation function
   indicate the size of the allocation.  */

/* At some point during the gcc 2.96 development the `pure' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */


/*  This declaration tells the compiler that the value is constant. */

/* At some point during the gcc 3.1 development the `used' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */


/*  gcc allows marking deprecated functions. */

/* At some point during the gcc 2.8 development the `format_arg' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.
   If several `format_arg' attributes are given for the same function, in
   gcc-3.0 and older, all but the last one are ignored.  In newer gccs,
   all designated arguments are considered.  */

/* At some point during the gcc 2.97 development the `strfmon' format
   attribute for functions was introduced.  We don't want to use it
   unconditionally (although this would be possible) since it
   generates warnings.  */

/* The nonull function attribute allows to mark pointer parameters which
   must not be NULL.  */

/* If fortification mode, we warn about unused results of certain
   function calls which can lead to problems.  */


/*  Forces a function to be always inlined. */

/* Associate error messages with the source location of the call site rather
   than with the source location inside the function.  */

/* GCC 4.3 and above with -std=c99 or -std=gnu99 implements ISO C99
   inline semantics, unless -fgnu89-inline is used.  Using __GNUC_STDC_INLINE__
   or __GNUC_GNU_INLINE is not a good enough check for gcc because gcc versions
   older than 4.3 may define these macros and still not guarantee GNU inlining
   semantics.

   clang++ identifies itself as gcc-4.2, but has support for GNU inlining
   semantics, that can be checked fot by using the __GNUC_STDC_INLINE_ and
   __GNUC_GNU_INLINE__ macro definitions.  */

/* GCC 4.3 and above allow passing all anonymous arguments of an
   __extern_always_inline function to some other vararg function.  */

/* It is possible to compile containing GCC extensions even if GCC is
   run in pedantic mode if the uses are carefully marked using the
   `__extension__' keyword.  But this is not generally available before
   version 2.8.  */


/*  __restrict is known in EGCS 1.2 and above. */

/* ISO C99 also allows to declare arrays as non-overlapping.  The syntax is
     array_name[restrict]
   GCC 3.1 supports this.  */


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */

/* If we don't have __REDIRECT, prototypes will be missing if
   __USE_FILE_OFFSET64 but not __USE_LARGEFILE[64]. */


/*  Decide whether we can define 'extern inline' functions in headers. */

/* This is here only because every header file already includes this one.
   Get the definitions of all the appropriate `__stub_FUNCTION' symbols.
   <gnu/stubs.h> contains `#define __stub_FUNCTION' when FUNCTION is a stub
   that will always return failure (and set errno to ENOSYS).  */

/* This file is automatically generated.
   This file selects the right generated file of `__stub_FUNCTION' macros
   based on the architecture being compiled for.  */

/* This file is automatically generated.
   It defines a symbol `__stub_FUNCTION' for each function
   in the C library which is a stub, meaning it will fail
   every time called, usually setting errno to ENOSYS.  */


/*  Get size_t, wchar_t and NULL from <stddef.h>. */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef long unsigned int size_t;

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
   instead of _WCHAR_T_, and _BSD_RUNE_T_ (which, unlike the other
   symbols in the _FOO_T_ family, stays defined even after its
   corresponding type is defined).  If we define wchar_t, then we
   must undef _WCHAR_T_; for BSD/386 1.1 (and perhaps others), if
   we undef _WCHAR_T_, then we must also define rune_t, since 
   headers like runetype.h assume that if machine/ansi.h is included,
   and _BSD_WCHAR_T_ is not defined, then rune_t is available.
   machine/ansi.h says, "Note that _WCHAR_T_ and _RUNE_T_ must be of
   the same type." */

/* FreeBSD 5 can't be handled well using "traditional" logic above
   since it no longer defines _BSD_RUNE_T_ yet still desires to export
   rune_t in some cases... */
typedef int wchar_t;

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */



/*  XPG requires a few symbols from <sys/wait.h> being defined. */

/* Definitions of flag bits for `waitpid' et al.
   Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Bits in the third argument to `waitpid'. */


/*  Bits in the fourth argument to `waitid'. */

/* Definitions of status bits for `wait' et al.
   Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Everything extant so far uses these same bits. */


/*  If WIFEXITED(STATUS), the low-order 8 bits of the status. */


/*  If WIFSIGNALED(STATUS), the terminating signal. */


/*  If WIFSTOPPED(STATUS), the signal that stopped the child. */


/*  Nonzero if STATUS indicates normal termination. */


/*  Nonzero if STATUS indicates termination by a signal. */


/*  Nonzero if STATUS indicates the child is stopped. */

/* Nonzero if STATUS indicates the child continued after a stop.  We only
   define this if <bits/waitflags.h> provides the WCONTINUED flag bit.  */


/*  Nonzero if STATUS indicates the child dumped core. */


/*  Macros for constructing status values. */

/* Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Definitions for byte order, according to significance of bytes,
   from low addresses to high addresses.  The value is what you get by
   putting '4' in the most significant byte, '3' in the second most
   significant byte, '2' in the second least significant byte, and '1'
   in the least significant byte, and then writing down one digit for
   each byte, starting with the byte at the lowest address at the left,
   and proceeding to the byte with the highest address at the right.  */


/*  This file defines `__BYTE_ORDER' for the particular machine. */


/*  i386/x86_64 are little-endian. */

/* Some machines may need to use a different endianness for floating point
   values.  */


/*  Conversion interfaces. */

/* Macros to swap the order of bytes in integer values.
   Copyright (C) 1997-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */


/*  Convenience types. */
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


/*  Fixed-size types, underlying types depend on word size and compiler. */
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;


/*  quad_t is also 64 bits. */
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;

/* The machine-dependent file <bits/typesizes.h> defines __*_T_TYPE
   macros for each of the OS types we define below.  The definitions
   of those macros must use the following macros for underlying types.
   We define __S<SIZE>_TYPE and __U<SIZE>_TYPE for the signed and unsigned
   variants of each of the following integer types on this machine.

	16		-- "natural" 16-bit type (always short)
	32		-- "natural" 32-bit type (always int)
	64		-- "natural" 64-bit type (long or long long)
	LONG32		-- 32-bit type, traditionally long
	QUAD		-- 64-bit type, always long long
	WORD		-- natural type of __WORDSIZE bits (int or long)
	LONGWORD	-- type of __WORDSIZE bits, traditionally long

   We distinguish WORD/LONGWORD, 32/LONG32, and 64/QUAD so that the
   conventional uses of `long' or `long long' type modifiers match the
   types we define, even when a less-adorned type would be the same size.
   This matters for (somewhat) portably writing printf/scanf formats for
   these types, where using the appropriate l or ll format modifiers can
   make the typedefs and the formats match up across all GNU platforms.  If
   we used `long' when it's 64 bits where `long long' is expected, then the
   compiler would warn about the formats not matching the argument types,
   and the programmer changing them to shut up the compiler would break the
   program's portability.

   Here we assume what is presently the case in all the GCC configurations
   we support: long long is always 64 bits, long is always word/address size,
   and int is always 32 bits.  */


/*  No need to mark the typedef with __extension__. */

/* bits/typesizes.h -- underlying types for *_t.  Linux/x86-64 version.
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* See <bits/types.h> for the meaning of these macros.  This file exists so
   that <bits/types.h> need not vary across different GNU platforms.  */


/*  X32 kernel interface is 64-bit. */

/* Tell the libc code that off_t and off64_t are actually the same type
   for all ABI purposes, even if possibly expressed as different base types
   for C type-checking purposes.  */


/*  Same for ino_t and ino64_t. */


/*  Number of descriptors that can fit in an `fd_set'. */
typedef unsigned long int __dev_t;

/*  Type of device numbers. */
typedef unsigned int __uid_t;

/*  Type of user identifications. */
typedef unsigned int __gid_t;

/*  Type of group identifications. */
typedef unsigned long int __ino_t;

/*  Type of file serial numbers. */
typedef unsigned long int __ino64_t;

/*  Type of file serial numbers (LFS). */
typedef unsigned int __mode_t;

/*  Type of file attribute bitmasks. */
typedef unsigned long int __nlink_t;

/*  Type of file link counts. */
typedef long int __off_t;

/*  Type of file sizes and offsets. */
typedef long int __off64_t;

/*  Type of file sizes and offsets (LFS). */
typedef int __pid_t;

/*  Type of process identifications. */
typedef struct {
    int __val[2];
} __fsid_t;

/*  Type of file system IDs. */
typedef long int __clock_t;

/*  Type of CPU usage counts. */
typedef unsigned long int __rlim_t;

/*  Type for resource measurement. */
typedef unsigned long int __rlim64_t;

/*  Type for resource measurement (LFS). */
typedef unsigned int __id_t;

/*  General type for IDs. */
typedef long int __time_t;

/*  Seconds since the Epoch. */
typedef unsigned int __useconds_t;

/*  Count of microseconds. */
typedef long int __suseconds_t;

/*  Signed count of microseconds. */
typedef int __daddr_t;

/*  The type of a disk address. */
typedef int __key_t;

/*  Type of an IPC key. */


/*  Clock ID used in clock and timer functions. */
typedef int __clockid_t;


/*  Timer ID returned by `timer_create'. */
typedef void *__timer_t;


/*  Type to represent block size. */
typedef long int __blksize_t;


/*  Types from the Large File Support interface. */


/*  Type to count number of disk blocks. */
typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


/*  Type to count file system blocks. */
typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


/*  Type to count file system nodes. */
typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


/*  Type of miscellaneous file system fields. */
typedef long int __fsword_t;
typedef long int __ssize_t;

/*  Type of a byte count, or error. */


/*  Signed long type used in system calls. */
typedef long int __syscall_slong_t;


/*  Unsigned long type used in system calls. */
typedef unsigned long int __syscall_ulong_t;

/* These few don't really vary by system, they always correspond
   to one of the other defined types.  */
typedef __off64_t __loff_t;

/*  Type of file sizes and offsets (LFS). */
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


/*  Duplicates info from stdint.h but this is used in unistd.h. */
typedef long int __intptr_t;


/*  Duplicate info from sys/socket.h. */
typedef unsigned int __socklen_t;


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */


/*  Swap bytes in 16 bit value. */


/*  Get __bswap_16. */

/* Macros to swap the order of bytes in 16-bit integer values.
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Swap bytes in 32 bit value. */
static __inline unsigned int __bswap_32(unsigned int __bsx)
{
    return __builtin_bswap32(__bsx);
}



/*  Swap bytes in 64 bit value. */
static __inline __uint64_t __bswap_64(__uint64_t __bsx)
{
    return __builtin_bswap64(__bsx);
}

union wait {
    int w_status;
    struct {
	unsigned int __w_termsig:7;

/*  Terminating signal. */
	unsigned int __w_coredump:1;

/*  Set if dumped core. */
	unsigned int __w_retcode:8;

/*  Return code if exited normally. */
	unsigned int:16;
    } __wait_terminated;
    struct {
	unsigned int __w_stopval:8;

/*  W_STOPPED if stopped. */
	unsigned int __w_stopsig:8;

/*  Stopping signal. */
	unsigned int:16;
    } __wait_stopped;
};

/* Lots of hair to allow traditional BSD use of `union wait'
   as well as POSIX.1 use of `int' for the status word.  */

/* This is the type of the argument to `wait'.  The funky union
   causes redeclarations with either `int *' or `union wait *' to be
   allowed without complaint.  __WAIT_STATUS_DEFN is the type used in
   the actual function definitions.  */


/*  This works in GCC 2.6.1 and later. */
typedef union {
    union wait *__uptr;
    int *__iptr;
} __WAIT_STATUS __attribute__ ((__transparent_union__));


/*  Define the macros <sys/wait.h> also would define this way. */



/*  Returned by `div'. */
typedef struct {
    int quot;

/*  Quotient. */
    int rem;

/*  Remainder. */
} div_t;


/*  Returned by `ldiv'. */
typedef struct {
    long int quot;

/*  Quotient. */
    long int rem;

/*  Remainder. */
} ldiv_t;




/*  Returned by `lldiv'. */
__extension__ typedef struct {
    long long int quot;

/*  Quotient. */
    long long int rem;

/*  Remainder. */
} lldiv_t;



/*  The largest number rand will return (same as INT_MAX). */

/* We define these the same for all machines.
   Changes from this to the outside world should be done in `_exit'.  */


/*  Maximum length of a multibyte character in the current locale. */
extern size_t __ctype_get_mb_cur_max(void)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;



/*  Convert a string to a floating-point number. */
extern double atof(const char *__nptr)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;


/*  Convert a string to an integer. */
extern int atoi(const char *__nptr)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;


/*  Convert a string to a long integer. */
extern long int atol(const char *__nptr)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;




/*  Convert a string to a long long integer. */
__extension__ extern long long int atoll(const char *__nptr)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;




/*  Convert a string to a floating-point number. */
extern double strtod(const char *__restrict __nptr,
		     char **__restrict __endptr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));




/*  Likewise for `float' and `long double' sizes of floating-point
    numbers. */
extern float strtof(const char *__restrict __nptr,
		    char **__restrict __endptr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));
extern long double strtold(const char *__restrict __nptr,
			   char **__restrict __endptr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));




/*  Convert a string to a long integer. */
extern long int strtol(const char *__restrict __nptr,
		       char **__restrict __endptr, int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Convert a string to an unsigned long integer. */
extern unsigned long int strtoul(const char *__restrict __nptr,
				 char **__restrict __endptr, int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));



/*  Convert a string to a quadword integer. */
__extension__
    extern long long int strtoq(const char *__restrict __nptr,
				char **__restrict __endptr, int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Convert a string to an unsigned quadword integer. */
__extension__
    extern unsigned long long int strtouq(const char *__restrict __nptr,
					  char **__restrict __endptr,
					  int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));



/*  Convert a string to a quadword integer. */
__extension__
    extern long long int strtoll(const char *__restrict __nptr,
				 char **__restrict __endptr, int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Convert a string to an unsigned quadword integer. */
__extension__
    extern unsigned long long int strtoull(const char *__restrict __nptr,
					   char **__restrict __endptr,
					   int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/* Convert N to base 64 using the digits "./0-9A-Za-z", least-significant
   digit first.  Returns a pointer to static storage overwritten by the
   next call.  */
extern char *l64a(long int __n)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/*  Read a number from a string S in base 64 as above. */
extern long int a64l(const char *__s)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	POSIX Standard: 2.6 Primitive System Data Types	<sys/types.h>
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
typedef __loff_t loff_t;
typedef __ino_t ino_t;
typedef __dev_t dev_t;
typedef __gid_t gid_t;
typedef __mode_t mode_t;
typedef __nlink_t nlink_t;
typedef __uid_t uid_t;
typedef __off_t off_t;
typedef __pid_t pid_t;
typedef __id_t id_t;
typedef __ssize_t ssize_t;
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
typedef __key_t key_t;

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */



/*  Returned by `clock'. */
typedef __clock_t clock_t;



/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */



/*  Returned by `time'. */
typedef __time_t time_t;



/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */


/*  Clock ID used in clock and timer functions. */
typedef __clockid_t clockid_t;

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */


/*  Timer ID returned by `timer_create'. */
typedef __timer_t timer_t;

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */


/*  Old compatibility names for C types. */
typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;


/*  These size-specific names are used by some of the inet code. */


/*  For GCC 2.7 and later, we can use specific type-size attributes. */
typedef int int8_t __attribute__ ((__mode__(__QI__)));
typedef int int16_t __attribute__ ((__mode__(__HI__)));
typedef int int32_t __attribute__ ((__mode__(__SI__)));
typedef int int64_t __attribute__ ((__mode__(__DI__)));
typedef unsigned int u_int8_t __attribute__ ((__mode__(__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__(__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__(__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__(__DI__)));
typedef int register_t __attribute__ ((__mode__(__word__)));

/* Some code from BIND tests this macro to see if the types above are
   defined.  */


/*  In BSD <sys/types.h> is expected to define BYTE_ORDER. */

/* Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  It also defines `fd_set' and the FD_* macros for `select'. */

/* `fd_set' type and related macros, and `select'/`pselect' declarations.
   Copyright (C) 1996-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  POSIX 1003.1g: 6.2 Select from File Descriptor Sets <sys/select.h> */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Get definition of needed basic types. */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */


/*  Get __FD_* definitions. */

/* Copyright (C) 1997-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */


/*  Get __sigset_t. */

/* __sig_atomic_t, __sigset_t, and related definitions.  Linux version.
   Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
typedef int __sig_atomic_t;


/*  A `sigset_t' has a bit for each signal. */
typedef struct {
    unsigned long int __val[(1024 / (8 * sizeof(unsigned long int)))];
} __sigset_t;

/* We only want to define these functions if <signal.h> was actually
   included; otherwise we were included just to define the types.  Since we
   are namespace-clean, it wouldn't hurt to define extra macros.  But
   trouble can be caused by functions being defined (e.g., any global
   register vars declared later will cause compilation errors).  */
typedef __sigset_t sigset_t;


/*  Get definition of timer specification structures. */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */

/* POSIX.1b structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
struct timespec {
    __time_t tv_sec;

/*  Seconds. */
    __syscall_slong_t tv_nsec;

/*  Nanoseconds. */
};

/* System-dependent timing definitions.  Linux version.
   Copyright (C) 1996-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <time.h> instead.
 */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */

/* A time value that is accurate to the nearest
   microsecond but also has a range of years.  */
struct timeval {
    __time_t tv_sec;

/*  Seconds. */
    __suseconds_t tv_usec;

/*  Microseconds. */
};
typedef __suseconds_t suseconds_t;


/*  The fd_set member is required to be an array of longs. */
typedef long int __fd_mask;


/*  Some versions of <linux/posix_types.h> define this macros. */


/*  It's easier to assume 8-bit bytes than to get CHAR_BIT. */


/*  fd_set for select and pselect. */
typedef struct {

/* XPG4.2 requires this member name.  Otherwise avoid the name
       from the global namespace.  */
    __fd_mask __fds_bits[1024 / (8 * (int) sizeof(__fd_mask))];
} fd_set;


/*  Maximum number of file descriptors in `fd_set'. */


/*  Sometimes the fd_set member is assumed to have this type. */
typedef __fd_mask fd_mask;


/*  Number of bits per word of `fd_set' (some code assumes this is 32). */


/*  Access macros for `fd_set'. */


/* Check the first NFDS descriptors each in READFDS (if not NULL) for read
   readiness, in WRITEFDS (if not NULL) for write readiness, and in EXCEPTFDS
   (if not NULL) for exceptional conditions.  If TIMEOUT is not NULL, time out
   after waiting the interval specified therein.  Returns the number of ready
   descriptors, or -1 for errors.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int select(int __nfds, fd_set * __restrict __readfds,
		  fd_set * __restrict __writefds,
		  fd_set * __restrict __exceptfds,
		  struct timeval *__restrict __timeout);

/* Same as above only that the TIMEOUT value is given with higher
   resolution and a sigmask which is been set temporarily.  This version
   should be used.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int pselect(int __nfds, fd_set * __restrict __readfds,
		   fd_set * __restrict __writefds,
		   fd_set * __restrict __exceptfds,
		   const struct timespec *__restrict __timeout,
		   const __sigset_t * __restrict __sigmask);


/*  Define some inlines helping to catch common problems. */



/*  BSD defines these symbols, so we follow. */

/* Definitions of macros to access `dev_t' values.
   Copyright (C) 1996-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

__extension__
    extern unsigned int gnu_dev_major(unsigned long long int __dev)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__const__));
__extension__
    extern unsigned int gnu_dev_minor(unsigned long long int __dev)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__const__));
__extension__
    extern unsigned long long int gnu_dev_makedev(unsigned int __major,
						  unsigned int __minor)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__const__));



/*  Access the functions with their traditional names. */
typedef __blksize_t blksize_t;


/*  Types from the Large File Support interface. */
typedef __blkcnt_t blkcnt_t;

/*  Type to count number of disk blocks. */
typedef __fsblkcnt_t fsblkcnt_t;

/*  Type to count file system blocks. */
typedef __fsfilcnt_t fsfilcnt_t;

/*  Type to count file system inodes. */


/*  Now add the thread types. */

/* Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */

/* Thread identifiers.  The structure of the attribute type is not
   exposed on purpose.  */
typedef unsigned long int pthread_t;
union pthread_attr_t {
    char __size[56];
    long int __align;
};
typedef union pthread_attr_t pthread_attr_t;
typedef struct __pthread_internal_list {
    struct __pthread_internal_list *__prev;
    struct __pthread_internal_list *__next;
} __pthread_list_t;

/* Data structures for mutex handling.  The structure of the attribute
   type is not exposed on purpose.  */
typedef union {
    struct __pthread_mutex_s {
	int __lock;
	unsigned int __count;
	int __owner;
	unsigned int __nusers;

/* KIND must stay at this position in the structure to maintain
	   binary compatibility.  */
	int __kind;
	short __spins;
	short __elision;
	__pthread_list_t __list;


/*  Mutex __spins initializer used by PTHREAD_MUTEX_INITIALIZER. */
    } __data;
    char __size[40];
    long int __align;
} pthread_mutex_t;
typedef union {
    char __size[4];
    int __align;
} pthread_mutexattr_t;

/* Data structure for conditional variable handling.  The structure of
   the attribute type is not exposed on purpose.  */
typedef union {
    struct {
	int __lock;
	unsigned int __futex;
	__extension__ unsigned long long int __total_seq;
	__extension__ unsigned long long int __wakeup_seq;
	__extension__ unsigned long long int __woken_seq;
	void *__mutex;
	unsigned int __nwaiters;
	unsigned int __broadcast_seq;
    } __data;
    char __size[48];
    __extension__ long long int __align;
} pthread_cond_t;
typedef union {
    char __size[4];
    int __align;
} pthread_condattr_t;


/*  Keys for thread-specific data */
typedef unsigned int pthread_key_t;


/*  Once-only execution */
typedef int pthread_once_t;

/* Data structure for read-write lock variable handling.  The
   structure of the attribute type is not exposed on purpose.  */
typedef union {
    struct {
	int __lock;
	unsigned int __nr_readers;
	unsigned int __readers_wakeup;
	unsigned int __writer_wakeup;
	unsigned int __nr_readers_queued;
	unsigned int __nr_writers_queued;
	int __writer;
	int __shared;
	signed char __rwelision;
	unsigned char __pad1[7];
	unsigned long int __pad2;

/* FLAGS must stay at this position in the structure to maintain
	   binary compatibility.  */
	unsigned int __flags;
    } __data;
    char __size[56];
    long int __align;
} pthread_rwlock_t;
typedef union {
    char __size[8];
    long int __align;
} pthread_rwlockattr_t;


/*  POSIX spinlock data type. */
typedef volatile int pthread_spinlock_t;

/* POSIX barriers data type.  The structure of the type is
   deliberately not exposed.  */
typedef union {
    char __size[32];
    long int __align;
} pthread_barrier_t;
typedef union {
    char __size[4];
    int __align;
} pthread_barrierattr_t;


/* These are the functions that actually do things.  The `random', `srandom',
   `initstate' and `setstate' functions are those from BSD Unices.
   The `rand' and `srand' functions are required by the ANSI standard.
   We provide both interfaces to the same random number generator.  */


/*  Return a random long integer between 0 and RAND_MAX inclusive. */
extern long int random(void) __attribute__ ((__nothrow__, __leaf__));


/*  Seed the random number generator with the given number. */
extern void srandom(unsigned int __seed)
    __attribute__ ((__nothrow__, __leaf__));

/* Initialize the random number generator to use state buffer STATEBUF,
   of length STATELEN, and seed it with SEED.  Optimal lengths are 8, 16,
   32, 64, 128 and 256, the bigger the better; values less than 8 will
   cause an error and values greater than 256 will be rounded down.  */
extern char *initstate(unsigned int __seed, char *__statebuf,
		       size_t __statelen)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));

/* Switch the random number generator to state buffer STATEBUF,
   which should have been previously initialized by `initstate'.  */
extern char *setstate(char *__statebuf)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));

/* Reentrant versions of the `random' family of functions.
   These functions all use the following data structure to contain
   state, rather than global state variables.  */
struct random_data {
    int32_t *fptr;

/*  Front pointer. */
    int32_t *rptr;

/*  Rear pointer. */
    int32_t *state;

/*  Array of state values. */
    int rand_type;

/*  Type of random number generator. */
    int rand_deg;

/*  Degree of random number generator. */
    int rand_sep;

/*  Distance between front and rear. */
    int32_t *end_ptr;

/*  Pointer behind state table. */
};
extern int random_r(struct random_data *__restrict __buf,
		    int32_t * __restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern int srandom_r(unsigned int __seed, struct random_data *__buf)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));
extern int initstate_r(unsigned int __seed, char *__restrict __statebuf,
		       size_t __statelen,
		       struct random_data *__restrict __buf)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2, 4)));
extern int setstate_r(char *__restrict __statebuf,
		      struct random_data *__restrict __buf)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));



/*  Return a random integer between 0 and RAND_MAX inclusive. */
extern int rand(void) __attribute__ ((__nothrow__, __leaf__));


/*  Seed the random number generator with the given number. */
extern void srand(unsigned int __seed)
    __attribute__ ((__nothrow__, __leaf__));



/*  Reentrant interface according to POSIX.1. */
extern int rand_r(unsigned int *__seed)
    __attribute__ ((__nothrow__, __leaf__));


/*  System V style 48-bit random number generator functions. */


/*  Return non-negative, double-precision floating-point value in
    [0.0,1.0). */
extern double drand48(void) __attribute__ ((__nothrow__, __leaf__));
extern double erand48(unsigned short int __xsubi[3])
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return non-negative, long integer in [0,2^31). */
extern long int lrand48(void) __attribute__ ((__nothrow__, __leaf__));
extern long int nrand48(unsigned short int __xsubi[3])
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return signed, long integers in [-2^31,2^31). */
extern long int mrand48(void) __attribute__ ((__nothrow__, __leaf__));
extern long int jrand48(unsigned short int __xsubi[3])
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Seed random number generator. */
extern void srand48(long int __seedval)
    __attribute__ ((__nothrow__, __leaf__));
extern unsigned short int *seed48(unsigned short int __seed16v[3])
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));
extern void lcong48(unsigned short int __param[7])
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));

/* Data structure for communication with thread safe versions.  This
   type is to be regarded as opaque.  It's only exported because users
   have to allocate objects of this type.  */
struct drand48_data {
    unsigned short int __x[3];

/*  Current state. */
    unsigned short int __old_x[3];

/*  Old state. */
    unsigned short int __c;

/*  Additive const. in congruential formula. */
    unsigned short int __init;

/*  Flag for initializing. */
    __extension__ unsigned long long int __a;
/* Factor in congruential
						   formula.  */
};


/*  Return non-negative, double-precision floating-point value in
    [0.0,1.0). */
extern int drand48_r(struct drand48_data *__restrict __buffer,
		     double *__restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern int erand48_r(unsigned short int __xsubi[3],
		     struct drand48_data *__restrict __buffer,
		     double *__restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Return non-negative, long integer in [0,2^31). */
extern int lrand48_r(struct drand48_data *__restrict __buffer,
		     long int *__restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern int nrand48_r(unsigned short int __xsubi[3],
		     struct drand48_data *__restrict __buffer,
		     long int *__restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Return signed, long integers in [-2^31,2^31). */
extern int mrand48_r(struct drand48_data *__restrict __buffer,
		     long int *__restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern int jrand48_r(unsigned short int __xsubi[3],
		     struct drand48_data *__restrict __buffer,
		     long int *__restrict __result)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Seed random number generator. */
extern int srand48_r(long int __seedval, struct drand48_data *__buffer)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));
extern int seed48_r(unsigned short int __seed16v[3],
		    struct drand48_data *__buffer)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern int lcong48_r(unsigned short int __param[7],
		     struct drand48_data *__buffer)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));



/*  Allocate SIZE bytes of memory. */
extern void *malloc(size_t __size) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__malloc__))
/* Ignore */ ;


/*  Allocate NMEMB elements of SIZE bytes each, all initialized to 0. */
extern void *calloc(size_t __nmemb, size_t __size)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__malloc__))
/* Ignore */ ;



/* Re-allocate the previously allocated block
   in PTR, making the new block SIZE bytes long.  */

/* __attribute_malloc__ is not used, because if realloc returns
   the same pointer that was passed to it, aliasing needs to be allowed
   between objects pointed by the old and new pointers.  */
extern void *realloc(void *__ptr, size_t __size)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__warn_unused_result__));


/*  Free a block allocated by `malloc', `realloc' or `calloc'. */
extern void free(void *__ptr) __attribute__ ((__nothrow__, __leaf__));



/*  Free a block. An alias for `free'. (Sun Unices). */
extern void cfree(void *__ptr) __attribute__ ((__nothrow__, __leaf__));

/* Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */



/*  Remove any previous definitions. */


/*  Allocate a block that will be freed when the calling function exits. */
extern void *alloca(size_t __size) __attribute__ ((__nothrow__, __leaf__));



/*  Allocate SIZE bytes on a page boundary. The storage cannot be freed. */
extern void *valloc(size_t __size) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__malloc__))
/* Ignore */ ;


/*  Allocate memory of SIZE bytes with an alignment of ALIGNMENT. */
extern int posix_memalign(void **__memptr, size_t __alignment,
			  size_t __size)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;


/*  ISO C variant of aligned allocation. */
extern void *aligned_alloc(size_t __alignment, size_t __size)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__malloc__))
    __attribute__ ((__alloc_size__(2)))
/* Ignore */ ;



/*  Abort execution and generate a core-dump. */
extern void abort(void) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__noreturn__));


/*  Register a function to be called when `exit' is called. */
extern int atexit(void (*__func) (void))
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Register a function to be called when `quick_exit' is called. */
extern int at_quick_exit(void (*__func) (void))
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/* Register a function to be called with the status
   given to `exit' and the given argument.  */
extern int on_exit(void (*__func) (int __status, void *__arg), void *__arg)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/* Call all functions registered with `atexit' and `on_exit',
   in the reverse of the order in which they were registered,
   perform stdio cleanup, and terminate program execution with STATUS.  */
extern void exit(int __status) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__noreturn__));

/* Call all functions registered with `at_quick_exit' in the reverse
   of the order in which they were registered and terminate program
   execution with STATUS.  */
extern void quick_exit(int __status)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__noreturn__));



/* Terminate the program with STATUS without calling any of the
   functions registered with `atexit' or `on_exit'.  */
extern void _Exit(int __status) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__noreturn__));




/*  Return the value of envariable NAME, or NULL if it doesn't exist. */
extern char *getenv(const char *__name)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;



/*  The SVID says this is in <stdio.h>, but this seems a better place. */

/* Put STRING, which is of the form "NAME=VALUE", in the environment.
   If there is no `=', remove NAME from the environment.  */
extern int putenv(char *__string) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));

/* Set NAME to VALUE in the environment.
   If REPLACE is nonzero, overwrite an existing value.  */
extern int setenv(const char *__name, const char *__value, int __replace)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));


/*  Remove the variable NAME from the environment. */
extern int unsetenv(const char *__name)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));

/* The `clearenv' was planned to be added to POSIX.1 but probably
   never made it.  Nevertheless the POSIX.9 standard (POSIX bindings
   for Fortran 77) requires this function.  */
extern int clearenv(void) __attribute__ ((__nothrow__, __leaf__));

/* Generate a unique temporary file name from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the file name unique.
   Always returns TEMPLATE, it's either a temporary file name or a null
   string if it cannot get a unique file name.  */
extern char *mktemp(char *__template)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));

/* Generate a unique temporary file name from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the filename unique.
   Returns a file descriptor open on the file for reading and writing,
   or -1 if it cannot create a uniquely-named file.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int mkstemp(char *__template)
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;

/* Similar to mkstemp, but the template can have a suffix after the
   XXXXXX.  The length of the suffix is specified in the second
   parameter.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int mkstemps(char *__template, int __suffixlen)
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;

/* Create a unique temporary directory from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the directory name unique.
   Returns TEMPLATE, or a null pointer if it cannot get a unique name.
   The directory is created mode 700.  */
extern char *mkdtemp(char *__template)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;


/* Execute the given line as a shell command.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int system(const char *__command)
/* Ignore */ ;


/* Return the canonical absolute name of file NAME.  If RESOLVED is
   null, the result is malloc'd; otherwise, if the canonical name is
   PATH_MAX chars or more, returns null with `errno' set to
   ENAMETOOLONG; if the name fits in fewer than PATH_MAX chars,
   returns the name in RESOLVED.  */
extern char *realpath(const char *__restrict __name,
		      char *__restrict __resolved)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/*  Shorthand for type of comparison functions. */
typedef int (*__compar_fn_t) (const void *, const void *);


/* Do a binary search for KEY in BASE, which consists of NMEMB elements
   of SIZE bytes each, using COMPAR to perform the comparisons.  */
extern void *bsearch(const void *__key, const void *__base,
		     size_t __nmemb, size_t __size, __compar_fn_t __compar)
    __attribute__ ((__nonnull__(1, 2, 5)))
/* Ignore */ ;

/* Sort NMEMB elements of BASE, of SIZE bytes each,
   using COMPAR to perform the comparisons.  */
extern void qsort(void *__base, size_t __nmemb, size_t __size,
		  __compar_fn_t __compar)
    __attribute__ ((__nonnull__(1, 4)));


/*  Return the absolute value of X. */
extern int abs(int __x) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__))
/* Ignore */ ;
extern long int labs(long int __x) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__))
/* Ignore */ ;

__extension__ extern long long int llabs(long long int __x)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__))
/* Ignore */ ;


/* Return the `div_t', `ldiv_t' or `lldiv_t' representation
   of the value of NUMER over DENOM. */


/*  GCC may have built-ins for these someday. */
extern div_t div(int __numer, int __denom)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__))
/* Ignore */ ;
extern ldiv_t ldiv(long int __numer, long int __denom)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__))
/* Ignore */ ;


__extension__ extern lldiv_t lldiv(long long int __numer,
				   long long int __denom)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__))
/* Ignore */ ;


/* Convert floating point numbers to strings.  The returned values are
   valid only until another call to the same function.  */

/* Convert VALUE to a string with NDIGIT digits and return a pointer to
   this.  Set *DECPT with the position of the decimal character and *SIGN
   with the sign of the number.  */
extern char *ecvt(double __value, int __ndigit, int *__restrict __decpt,
		  int *__restrict __sign)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4)))
/* Ignore */ ;

/* Convert VALUE to a string rounded to NDIGIT decimal digits.  Set *DECPT
   with the position of the decimal character and *SIGN with the sign of
   the number.  */
extern char *fcvt(double __value, int __ndigit, int *__restrict __decpt,
		  int *__restrict __sign)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4)))
/* Ignore */ ;

/* If possible convert VALUE to a string with NDIGIT significant digits.
   Otherwise use exponential representation.  The resulting string will
   be written to BUF.  */
extern char *gcvt(double __value, int __ndigit, char *__buf)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3)))
/* Ignore */ ;


/*  Long double versions of above functions. */
extern char *qecvt(long double __value, int __ndigit,
		   int *__restrict __decpt, int *__restrict __sign)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4)))
/* Ignore */ ;
extern char *qfcvt(long double __value, int __ndigit,
		   int *__restrict __decpt, int *__restrict __sign)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4)))
/* Ignore */ ;
extern char *qgcvt(long double __value, int __ndigit, char *__buf)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3)))
/* Ignore */ ;

/* Reentrant version of the functions above which provide their own
   buffers.  */
extern int ecvt_r(double __value, int __ndigit, int *__restrict __decpt,
		  int *__restrict __sign, char *__restrict __buf,
		  size_t __len) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4, 5)));
extern int fcvt_r(double __value, int __ndigit, int *__restrict __decpt,
		  int *__restrict __sign, char *__restrict __buf,
		  size_t __len) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4, 5)));
extern int qecvt_r(long double __value, int __ndigit,
		   int *__restrict __decpt, int *__restrict __sign,
		   char *__restrict __buf, size_t __len)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4, 5)));
extern int qfcvt_r(long double __value, int __ndigit,
		   int *__restrict __decpt, int *__restrict __sign,
		   char *__restrict __buf, size_t __len)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3, 4, 5)));


/* Return the length of the multibyte character
   in S, which is no longer than N.  */
extern int mblen(const char *__s, size_t __n)
    __attribute__ ((__nothrow__, __leaf__));

/* Return the length of the given multibyte character,
   putting its `wchar_t' representation in *PWC.  */
extern int mbtowc(wchar_t * __restrict __pwc,
		  const char *__restrict __s, size_t __n)
    __attribute__ ((__nothrow__, __leaf__));

/* Put the multibyte character represented
   by WCHAR in S, returning its length.  */
extern int wctomb(char *__s, wchar_t __wchar)
    __attribute__ ((__nothrow__, __leaf__));


/*  Convert a multibyte string to a wide char string. */
extern size_t mbstowcs(wchar_t * __restrict __pwcs,
		       const char *__restrict __s, size_t __n)
    __attribute__ ((__nothrow__, __leaf__));


/*  Convert a wide char string to multibyte string. */
extern size_t wcstombs(char *__restrict __s,
		       const wchar_t * __restrict __pwcs, size_t __n)
    __attribute__ ((__nothrow__, __leaf__));


/* Determine whether the string value of RESPONSE matches the affirmation
   or negative response expression as specified by the LC_MESSAGES category
   in the program's current locale.  Returns 1 if affirmative, 0 if
   negative, and -1 if not matching.  */
extern int rpmatch(const char *__response)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)))
/* Ignore */ ;

/* Parse comma separated suboption from *OPTIONP and match against
   strings in TOKENS.  If found return index and set *VALUEP to
   optional value introduced by an equal sign.  If the suboption is
   not part of TOKENS return in *VALUEP beginning of unknown
   suboption.  On exit *OPTIONP is set to the beginning of the next
   token or at the terminating NUL character.  */
extern int getsubopt(char **__restrict __optionp,
		     char *const *__restrict __tokens,
		     char **__restrict __valuep)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2, 3)))
/* Ignore */ ;


/*  X/Open pseudo terminal handling. */

/* Put the 1 minute, 5 minute and 15 minute load averages into the first
   NELEM elements of LOADAVG.  Return the number written (never more than
   three, but may be less than NELEM), or -1 if an error occurred.  */
extern int getloadavg(double __loadavg[], int __nelem)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));

/* Floating-point inline functions for stdlib.h.
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Define some macros helping to catch buffer overflows. */


/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.21 String handling	<string.h>
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */



/*  Get size_t and NULL from <stddef.h>. */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */

/* Provide correct C++ prototypes, and indicate this to the caller.  This
   requires a compatible C++ standard library.  As a heuristic, we provide
   these when the compiler indicates full conformance with C++98 or later,
   and for older GCC versions that are known to provide a compatible
   libstdc++.  */



/*  Copy N bytes of SRC to DEST. */
extern void *memcpy(void *__restrict __dest, const void *__restrict __src,
		    size_t __n) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));

/* Copy N bytes of SRC to DEST, guaranteeing
   correct behavior for overlapping strings.  */
extern void *memmove(void *__dest, const void *__src, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/* Copy no more than N bytes of SRC to DEST, stopping when C is found.
   Return the position in DEST one byte past where C was copied,
   or NULL if C was not found in the first N bytes of SRC.  */
extern void *memccpy(void *__restrict __dest, const void *__restrict __src,
		     int __c, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));



/*  Set N bytes of S to C. */
extern void *memset(void *__s, int __c, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Compare N bytes of S1 and S2. */
extern int memcmp(const void *__s1, const void *__s2, size_t __n)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Search N bytes of S for C. */
extern void *memchr(const void *__s, int __c, size_t __n)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));




/*  Copy SRC to DEST. */
extern char *strcpy(char *__restrict __dest, const char *__restrict __src)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Copy no more than N characters of SRC to DEST. */
extern char *strncpy(char *__restrict __dest,
		     const char *__restrict __src, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Append SRC onto DEST. */
extern char *strcat(char *__restrict __dest, const char *__restrict __src)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Append no more than N characters from SRC onto DEST. */
extern char *strncat(char *__restrict __dest, const char *__restrict __src,
		     size_t __n) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Compare S1 and S2. */
extern int strcmp(const char *__s1, const char *__s2)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Compare N characters of S1 and S2. */
extern int strncmp(const char *__s1, const char *__s2, size_t __n)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Compare the collated forms of S1 and S2. */
extern int strcoll(const char *__s1, const char *__s2)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Put a transformation of SRC into no more than N bytes of DEST. */
extern size_t strxfrm(char *__restrict __dest,
		      const char *__restrict __src, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));


/* The following functions are equivalent to the both above but they
   take the locale they use for the collation as an extra argument.
   This is not standardsized but something like will come.  */

/* Definition of locale datatype.
   Copyright (C) 1997-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Structure for reentrant locale using functions.  This is an
   (almost) opaque type for the user level programs.  The file and
   this data structure is not standardized.  Don't rely on it.  It can
   go away without warning.  */
typedef struct __locale_struct {


/*  Note: LC_ALL is not a valid index into this array. */
    struct __locale_data *__locales[13];

/*  13 = __LC_LAST. */


/*  To increase the speed of this solution we add some special members. */
    const unsigned short int *__ctype_b;
    const int *__ctype_tolower;
    const int *__ctype_toupper;


/*  Note: LC_ALL is not a valid index into this array. */
    const char *__names[13];
} *__locale_t;


/*  POSIX 2008 makes locale_t official. */
typedef __locale_t locale_t;


/*  Compare the collated forms of S1 and S2 using rules from L. */
extern int strcoll_l(const char *__s1, const char *__s2, __locale_t __l)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2, 3)));


/*  Put a transformation of SRC into no more than N bytes of DEST. */
extern size_t strxfrm_l(char *__dest, const char *__src, size_t __n,
			__locale_t __l)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2, 4)));


/*  Duplicate S, returning an identical malloc'd string. */
extern char *strdup(const char *__s)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__malloc__))
    __attribute__ ((__nonnull__(1)));

/* Return a malloc'd copy of at most N bytes of STRING.  The
   resultant string is terminated even if no null terminator
   appears before STRING[N].  */
extern char *strndup(const char *__string, size_t __n)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__malloc__))
    __attribute__ ((__nonnull__(1)));



/*  Find the first occurrence of C in S. */
extern char *strchr(const char *__s, int __c)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));


/*  Find the last occurrence of C in S. */
extern char *strrchr(const char *__s, int __c)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));



/* Return the length of the initial segment of S which
   consists entirely of characters not in REJECT.  */
extern size_t strcspn(const char *__s, const char *__reject)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));

/* Return the length of the initial segment of S which
   consists entirely of characters in ACCEPT.  */
extern size_t strspn(const char *__s, const char *__accept)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Find the first occurrence in S of any character in ACCEPT. */
extern char *strpbrk(const char *__s, const char *__accept)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Find the first occurrence of NEEDLE in HAYSTACK. */
extern char *strstr(const char *__haystack, const char *__needle)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Divide S into tokens separated by characters in DELIM. */
extern char *strtok(char *__restrict __s, const char *__restrict __delim)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));


/* Divide S into tokens separated by characters in DELIM.  Information
   passed between calls are stored in SAVE_PTR.  */
extern char *__strtok_r(char *__restrict __s,
			const char *__restrict __delim,
			char **__restrict __save_ptr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2, 3)));
extern char *strtok_r(char *__restrict __s, const char *__restrict __delim,
		      char **__restrict __save_ptr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2, 3)));



/*  Return the length of S. */
extern size_t strlen(const char *__s)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));


/* Find the length of STRING, but scan at most MAXLEN characters.
   If no '\0' terminator is found in that many characters, return MAXLEN.  */
extern size_t strnlen(const char *__string, size_t __maxlen)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));



/*  Return a string describing the meaning of the `errno' code in
    ERRNUM. */
extern char *strerror(int __errnum)
    __attribute__ ((__nothrow__, __leaf__));


/* Reentrant version of `strerror'.
   There are 2 flavors of `strerror_r', GNU which returns the string
   and may or may not use the supplied temporary buffer and POSIX one
   which fills the string into the buffer.
   To use the POSIX version, -D_XOPEN_SOURCE=600 or -D_POSIX_C_SOURCE=200112L
   without -D_GNU_SOURCE is needed, otherwise the GNU version is
   preferred.  */

/* Fill BUF with a string describing the meaning of the `errno' code in
   ERRNUM.  */
extern int strerror_r(int __errnum, char *__buf,
		      size_t __buflen) __asm__("" "__xpg_strerror_r")
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));


/*  Translate error number to string according to the locale L. */
extern char *strerror_l(int __errnum, __locale_t __l)
    __attribute__ ((__nothrow__, __leaf__));

/* We define this function always since `bzero' is sometimes needed when
   the namespace rules does not allow this.  */
extern void __bzero(void *__s, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Copy N bytes of SRC to DEST (like memmove, but args reversed). */
extern void bcopy(const void *__src, void *__dest, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set N bytes of S to 0. */
extern void bzero(void *__s, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Compare N bytes of S1 and S2 (same as memcmp). */
extern int bcmp(const void *__s1, const void *__s2, size_t __n)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Find the first occurrence of C in S (same as strchr). */
extern char *index(const char *__s, int __c)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));


/*  Find the last occurrence of C in S (same as strrchr). */
extern char *rindex(const char *__s, int __c)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1)));

/* Return the position of the first bit set in I, or 0 if none are set.
   The least-significant bit is position 1, the most-significant 32.  */
extern int ffs(int __i) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__));

/* The following two functions are non-standard but necessary for non-32 bit
   platforms.  */


/*  Compare S1 and S2, ignoring case. */
extern int strcasecmp(const char *__s1, const char *__s2)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Compare no more than N chars of S1 and S2, ignoring case. */
extern int strncasecmp(const char *__s1, const char *__s2, size_t __n)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__pure__))
    __attribute__ ((__nonnull__(1, 2)));

/* Return the next DELIM-delimited token from *STRINGP,
   terminating it with a '\0', and update *STRINGP to point past it.  */
extern char *strsep(char **__restrict __stringp,
		    const char *__restrict __delim)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Return a string describing the meaning of the signal number in SIG. */
extern char *strsignal(int __sig) __attribute__ ((__nothrow__, __leaf__));


/*  Copy SRC to DEST, returning the address of the terminating '\0' in
    DEST. */
extern char *__stpcpy(char *__restrict __dest,
		      const char *__restrict __src)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern char *stpcpy(char *__restrict __dest, const char *__restrict __src)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));

/* Copy no more than N characters of SRC to DEST, returning the address of
   the last character written into DEST.  */
extern char *__stpncpy(char *__restrict __dest,
		       const char *__restrict __src, size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));
extern char *stpncpy(char *__restrict __dest, const char *__restrict __src,
		     size_t __n)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));



/*  This initial sections defines values for all macros. These are the */


/*  values that are generally appropriate to an ANSI C compiler on Unix. */


/*  For macros that have different values on other systems, the macros */


/*  should be undefined and then redefined in the system specific
    sections. */


/*  At the end of this section, some macros are redefined if the
    compiler */


/*  is non-ANSI. */


/*  ----- Macros common to calling C from FORTRAN and FORTRAN from C
    ---- */


/*  --- External Names --- */


/*  Macro to define the name of a Fortran routine or common block. This */


/*  ends in an underscore on many Unix systems. */


/*  --- Logical Values --- */


/*  Define the values that are used to represent the logical values TRUE */


/*  and FALSE in Fortran. */


/*  Define macros that evaluate to C logical values, given a FORTRAN */


/*  logical value. */


/*  --- Common Blocks --- */


/*  Macros used in referring to FORTRAN common blocks. */


/*  ------------------ Calling C from FORTRAN
    --------------------------- */


/*  --- Data Types --- */


/*  Define macros for all the Fortran data types (except COMPLEX, which
    is */


/*  not handled by this package). */


/*  Define macros for the type of a CHARACTER and CHARACTER_ARRAY
    argument */


/*  Define a macro to use when passing arguments that STARLINK FORTRAN */


/*  treats as a pointer. From the point of view of C, this type should
    be */


/*  (void *), but it is declared as type unsigned int as we actually
    pass */


/*  an INTEGER from the FORTRAN routine. The distinction is important
    for */


/*  architectures where the size of an INTEGER is not the same as the
    size */


/*  of a pointer. */


/*  --- Subroutine Names --- */


/*  This declares that the C function returns a value of void. */


/*  --- Function Names --- */


/*  Macros to define the types and names of functions that return
    values. */


/*  Due the the different ways that function return values could be */


/*  implemented, it is better not to use functions, but to stick to
    using */


/*  subroutines. */


/*  Character functions are implemented, but in a way that cannot be */


/*  guaranteed to be portable although it will work on VMS, SunOS,
    Ultrix */


/*  and DEC OSF/1. It would be better to return the character value as a */


/*  subroutine argument where possible, rather than use a character */


/*  function. */


/*  --- Character return value for a function --- */


/*  --- Dummy Arguments --- */


/*  Macros for defining subroutine arguments. All these macros take a */


/*  single argument; the name of the parameter. On most systems, a
    numeric */


/*  argument is passed as a pointer. */


/*  Pointer arguments. Define a pointer type for passing pointer values */


/*  between subroutines. */


/*  EXTERNAL arguments. Define a passed subroutine or function name */


/*  Array arguments. */


/*  Macros to handle character arguments. */


/*  Character arguments can be passed in many ways. The purpose of these */


/*  macros and the GENPTR_CHARACTER macro (defined in the next section)
    is */


/*  to generate a pointer to a character variable called ARG and an
    integer */


/*  ARG_length containing the length of ARG. If these two variables are */


/*  available directly from the argument list of the routine, then the */


/*  GENPTR_CHARACTER macro is null, otherwise it works on intermediate */


/*  variables. */


/*  --- Getting Pointers to Arguments --- */


/*  Macros that ensure that a pointer to each argument is available for
    the */


/*  programmer to use. Usually this means that these macros are null. On */


/*  VMS, a pointer to a character variable has to be generated. If a */


/*  particular machine were to pass arguments by reference, rather than
    by */


/*  value, then these macros would construct the appropriate pointers. */


/*  ------------------ Calling FORTRAN from C
    --------------------------- */


/*  --- Declare variables --- */


/*  --- Declare arrays --- */


/*  --- Declare and construct dynamic CHARACTER arguments --- */


/*  Declare Dynamic Fortran arrays */


/*  Create arrays dynamic Fortran arrays for those types which require */


/*  Separate space for Fortran and C arrays */


/*  Character and logical are already defined */


/*  For most types there is nothing to do */


/*  Associate Fortran arrays with C arrays */


/*  These macros ensure that there is space somewhere for the Fortran */


/*  array. They are complemetary to the CREATE_type_ARRAY macros */


/*  Free created dynamic arrays */


/*  Character and logical are already defined */


/*  For most types there is nothing to do */


/*  --- IMPORT and EXPORT of values --- */


/*  Export C variables to Fortran variables */

/* Allow for character strings to be NULL, protects strlen. Note this
 * does not allow lengths to differ. */


/*  Export C arrays to Fortran */


/*  Arrays are assumed to be 1-d so just the number of elements is given */


/*  This may be OK for n-d arrays also */


/*  CHARACTER arrays may be represented in C as arrays of arrays of char
    or */


/*  as arrays of pointers to char (the _P variant) */


/*  Import Fortran variables to C */


/*  Import Fortran arrays to C */


/*  Arrays are assumed to be 1-d so just the number of elements is given */


/*  This may be OK for n-d arrays also */


/*  CHARACTER arrays may be represented in C as arrays of arrays of char
    or */


/*  as arrays of pointers to char (the _P variant) */


/*  --- Call a FORTRAN routine --- */


/*  --- Execute code synchronised by the CNF global mutex */


/*  --- Pass arguments to a FORTRAN routine --- */


/*  Put the 64-bit INT support in one place */


/*  ------------------------ Non-ansi section
    ------------------------------ */


/*  The difference between ANSI and non-ANSI compilers, as far as macro */


/*  definition is concerned, is that non-ANSI compilers do not support
    the */


/*  token concatenation operator (##). To work around this, we use the
    fact */


/*  that the null comment is preprocessed to produce no characters at
    all */


/*  by our non-ANSI compilers. */


/*  This section does not deal with the fact that some non-ANSI
    compilers */


/*  cannot handle function prototypes. That is handled in the machine */


/*  specific sections. */


/* 
    ----------------------------------------------------------------------- */


/*  The standard macros defined above are known to work with the
    following */


/*  systems: */

/*--------
|   Sun   |
---------*/


/*  On SunOS, the ANSI definitions work with the acc and gcc compilers. */


/*  The cc compiler uses the non ANSI definitions. It also needs the K&R */


/*  definitions in the file kr.h. */


/*  On Solaris, the standard definitions work with the cc compiler. */


/*  -------------------- System dependent sections
    ------------------------- */

/*------------
|   VAX/VMS   |
-------------*/


/*  Many macros need to be changed due to the way that VMS handles
    external */


/*  names, passes character arguments and handles logical values. */


/* 
    ----------------------------------------------------------------------- */

/*--------------------------
|   DECstation Ultrix (cc)  |
|   DECstation Ultrix (c89) |
|   DECstation OSF/1        |
|   Alpha OSF/1             |
 --------------------------*/


/*  Do this complicated set of definitions as a single #if cannot be */


/*  continued across multiple lines. */

/*
*+
*  Name:
*     cnf.h

*  Purpose:
*     Function prototypes for cnf routines

*  Language:
*     ANSI C

*  Type of Module:
*     C include file

*  Description:
*     These are the prototype definitions for the functions in the CNF
*     library. They are used used in mixing C and FORTRAN programs.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (PMA):
*        Original version.
*     12-JAN-1996 (AJC):
*        Add cnf_cref and cnf_freef
*     14-JUN-1996 (AJC):
*        Add cnf_crefa, imprta, exprta
*                crela, impla, expla
*     18-JUL-1996 (AJC):
*        Add impch and expch
*     17-MAR-1998 (AJC):
*        Add imprtap and exprtap
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
------------------------------------------------------------------------------
*/
void cnfInitRTL(int, char **);
void *cnfCalloc(size_t, size_t);
void cnfCopyf(const char *source_f, int source_len, char *dest_f,
	      int dest_len);
void *cnfCptr(unsigned int);
char *cnfCreat(int length);
char *cnfCref(int length);
char *cnfCrefa(int length, int ndims, const int *dims);
char *cnfCreib(const char *source_f, int source_len);
char *cnfCreim(const char *source_f, int source_len);
int *cnfCrela(int ndims, const int *dims);
void cnfExpch(const char *source_c, char *dest_f, int nchars);
void cnfExpla(const int *source_c, int *dest_f, int ndims,
	      const int *dims);
void cnfExpn(const char *source_c, int max, char *dest_f, int dest_len);
void cnfExprt(const char *source_c, char *dest_f, int dest_len);
void cnfExprta(const char *source_c, int source_len, char *dest_f,
	       int dest_len, int ndims, const int *dims);
void cnfExprtap(char *const *source_c, char *dest_f, int dest_len,
		int ndims, const int *dims);
unsigned int cnfFptr(void *cpointer);
void cnfFree(void *);
void cnfFreef(char *temp);
void cnfImpb(const char *source_f, int source_len, char *dest_c);
void cnfImpbn(const char *source_f, int source_len, int max, char *dest_c);
void cnfImpch(const char *source_f, int nchars, char *dest_c);
void cnfImpla(const int *source_f, int *dest_c,
	      int ndims, const int *dims);
void cnfImpn(const char *source_f, int source_len, int max, char *dest_c);
void cnfImprt(const char *source_f, int source_len, char *dest_c);
void cnfImprta(const char *source_f, int source_len, char *dest_c,
	       int dest_len, int ndims, const int *dims);
void cnfImprtap(const char *source_f, int source_len, char *const *dest_c,
		int dest_len, int ndims, const int *dims);
int cnfLenc(const char *source_c);
int cnfLenf(const char *source_f, int source_len);
void *cnfMalloc(size_t);
void *cnfRealloc(void *, size_t);
int cnfRegp(void *);
void cnfUregp(void *);
void cnfLock(void);
void cnfUnlock(void);


/*  Define old names to be new names */

/*
*+
*  Name:
*     c2f77.h

*  Purpose:
*     Define the interface to the c2f77 module.

*  Description:
*     This file defines language-specific functions which support the
*     FORTRAN 77 interface to the AST library.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Functions Defined:
*     Public:
*        None.
*
*     Protected:
*        astStringExport
*           Export a C string to a FORTRAN string.

*  Macros Defined:
*     Public:
*        None.
*
*     Protected:
*        astWatchSTATUS
*           Execute C code while watching a FORTRAN STATUS variable.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     15-NOV-1996 (RFWS):
*        Original version.
*     16-JUL-1997 (RFWS):
*        Added astWatchSTATUS.
*     13-JUN-2001 (DSB):
*        Make astStringExport available to F77 interface modules as well
*        as AST classes.
*-
*/


/*  Macros. */


/*  ======= */

/*
*+
*  Name:
*     astWatchSTATUS

*  Type:
*     Protected macro.

*  Purpose:
*     Execute C code while watching a FORTRAN STATUS variable.

*  Synopsis:
*     #include "c2f77.h"
*     astWatchSTATUS(code)

*  Description:
*     This macro expands to code which executes the C code supplied
*     via the "code" argument in a new C scope (delimited by
*     {...}). The code supplied executes while the AST error status is
*     equated to a variable called STATUS, which is an error status
*     argument passed from a FORTRAN routine using the macros defined
*     in the "f77.h" include file.
*
*     The effect of this is roughly as if the astWatch function had
*     been used to locally declare the FORTRAN STATUS argument as a
*     new AST error status variable, except that this macro also works
*     if STATUS is not an int.

*  Parameters:
*     code
*        The C code to be executed.

*  Examples:
*     F77_SUBROUTINE(ast_doit)( INTEGER(STATUS) ) {
*        astWatchSTATUS(
*           astDoit();
*        )
*     }
*        Causes the astDoit function to be invoked as if the AST error
*        status were equated to the STATUS argument passed from
*        FORTRAN.  Typically, if STATUS is set to an error value,
*        astDoit would detect this by means of the astOK macro and
*        would not then execute.  If an error occurs in astDoit,
*        causing the AST error status to be set, then that value is
*        transferred to STATUS after the C code has executed (i.e. at
*        the end of the astWatchSTATUS macro).

*  Notes:
*     - The FORTRAN argument must be called STATUS and must appear in
*     the C function's parameter list as an argument of the INTEGER()
*     macro defined in the "f77.h" include file.
*     - The C code supplied executes in a new scope, in which
*     automatic variables may be declared. However, such variables
*     will not exist after the macro's expansion has been executed.
*     - The AST error status variable and its value remain unchanged
*     after the expansion of this macro has executed.
*-
*/


/*  Define the macro. */


/*  Function prototypes. */


/*  ==================== */
void astStringExport_(const char *, char *, int);


/*  Function interfaces. */


/*  ==================== */

/* These wrap up the functions defined by this module to make them
   easier to use. */

/*
*+
*  Name:
*     error.h

*  Purpose:
*     Define the interface to the Error module.

*  Description:
*     This module defines functions which implement error handling and
*     reporting of error messages from within the AST library. A
*     simple public interface is included to allow the AST error
*     status to be tested and cleared after an error.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S Berry (Starlink)
*     NG: Norman Gray (Starlink)

*  History:
*     2-JAN-1996 (RFWS):
*        Original version.
*     26-JAN-1996 (RFWS):
*        Added function interfaces.
*     14-JUN-1996 (RFWS):
*        Added AST__FAC and astAt.
*     20-JUN-1996 (RFWS):
*        Added astSetStatus.
*     16-JUL-1996 (RFWS):
*        Added astWatch.
*     18-MAR-1998 (RFWS):
*        Make interface available for writing foreign language and
*        graphics interfaces, etc.
*     27-NOV-2002 (DSB):
*        Added astReporting.
*     14-MAR-2005 (NG):
*        Added astAssert
*     20-MAY-2005 (DSB):
*        Modified astAssert so that it does nothing if the AST error
*        status is already set, and also so that does nothing unless
*        the DEBUG macro is defined.
*     16-FEB-2006 (DSB):
*        Improve efficiency by replacing the astOK_ function with a macro
*        which tests the value of status variable. The pointer which points
*        to the status variable are now global rather than static.
*     1-MAR-2006 (DSB):
*        Remove astAssert.
*     19-SEP-2008 (DSB)
*        Big changes for thread-safe version of AST.
*-
*/

/* Suppress "operands are evaluated in unspecified order" warnings from
   the Intel icc compiler. These are caused by the astGetStatusPtr_
   function being called several times within each of the macros that
   form the public interface for AST. */


/*  Include files. */


/*  ============== */

/* Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1992-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Definitions for POSIX 1003.1b-1993 (aka POSIX.4) scheduling interface.
   Copyright (C) 1996-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Get type definitions. */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */


/*  Get system specific constant and data structure definitions. */

/* Definitions of constants and data structure for POSIX 1003.1b-1993
   scheduling interface.
   Copyright (C) 1996-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Scheduling algorithms. */


/*  The official definition. */
struct sched_param {
    int __sched_priority;
};




/*  Data structure to describe a process' schedulability. */
struct __sched_param {
    int __sched_priority;
};


/*  Size definition for CPU sets. */


/*  Type for array elements in 'cpu_set_t'. */
typedef unsigned long int __cpu_mask;


/*  Basic access functions. */


/*  Data structure to describe CPU mask. */
typedef struct {
    __cpu_mask __bits[1024 / (8 * sizeof(__cpu_mask))];
} cpu_set_t;


/*  Access functions for CPU masks. */

extern int __sched_cpucount(size_t __setsize, const cpu_set_t * __setp)
    __attribute__ ((__nothrow__, __leaf__));
extern cpu_set_t *__sched_cpualloc(size_t __count)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;
extern void __sched_cpufree(cpu_set_t * __set)
    __attribute__ ((__nothrow__, __leaf__));



/*  Define the real names for the elements of `struct sched_param'. */



/*  Set scheduling parameters for a process. */
extern int sched_setparam(__pid_t __pid, const struct sched_param *__param)
    __attribute__ ((__nothrow__, __leaf__));


/*  Retrieve scheduling parameters for a particular process. */
extern int sched_getparam(__pid_t __pid, struct sched_param *__param)
    __attribute__ ((__nothrow__, __leaf__));


/*  Set scheduling algorithm and/or parameters for a process. */
extern int sched_setscheduler(__pid_t __pid, int __policy,
			      const struct sched_param *__param)
    __attribute__ ((__nothrow__, __leaf__));


/*  Retrieve scheduling algorithm for a particular purpose. */
extern int sched_getscheduler(__pid_t __pid)
    __attribute__ ((__nothrow__, __leaf__));


/*  Yield the processor. */
extern int sched_yield(void) __attribute__ ((__nothrow__, __leaf__));


/*  Get maximum priority value for a scheduler. */
extern int sched_get_priority_max(int __algorithm)
    __attribute__ ((__nothrow__, __leaf__));


/*  Get minimum priority value for a scheduler. */
extern int sched_get_priority_min(int __algorithm)
    __attribute__ ((__nothrow__, __leaf__));


/*  Get the SCHED_RR interval for the named process. */
extern int sched_rr_get_interval(__pid_t __pid, struct timespec *__t)
    __attribute__ ((__nothrow__, __leaf__));


/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */



/*  Get size_t and NULL from <stddef.h>. */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */

/* This defines CLOCKS_PER_SEC, which is the number of processor clock
   ticks per second.  */

/* System-dependent timing definitions.  Linux version.
   Copyright (C) 1996-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <time.h> instead.
 */

/* ISO/IEC 9899:1999 7.23.1: Components of time
   The macro `CLOCKS_PER_SEC' is an expression with type `clock_t' that is
   the number per second of the value returned by the `clock' function.  */

/* CAE XSH, Issue 4, Version 2: <time.h>
   The value of CLOCKS_PER_SEC is required to be 1 million on all
   XSI-conformant systems. */


/*  Identifier for system-wide realtime clock. */


/*  Monotonic system-wide clock. */


/*  High-resolution timer from the CPU. */


/*  Thread-specific CPU-time clock. */


/*  Monotonic system-wide clock, not adjusted for frequency scaling. */


/*  Identifier for system-wide realtime clock, updated only on ticks. */


/*  Monotonic system-wide clock, updated only on ticks. */


/*  Monotonic system-wide clock that includes time spent in suspension. */


/*  Like CLOCK_REALTIME but also wakes suspended system. */


/*  Like CLOCK_BOOTTIME but also wakes suspended system. */


/*  Like CLOCK_REALTIME but in International Atomic Time. */


/*  Flag to indicate time is absolute. */


/*  This is the obsolete POSIX.1-1988 name for the same constant. */



/*  Used by other time functions. */
struct tm {
    int tm_sec;

/*  Seconds. [0-60] (1 leap second) */
    int tm_min;

/*  Minutes. [0-59] */
    int tm_hour;

/*  Hours. [0-23] */
    int tm_mday;

/*  Day. [1-31] */
    int tm_mon;

/*  Month. [0-11] */
    int tm_year;

/*  Year - 1900. */
    int tm_wday;

/*  Day of week. [0-6] */
    int tm_yday;

/*  Days in year.[0-365] */
    int tm_isdst;

/*  DST. [-1/0/1] */
    long int tm_gmtoff;

/*  Seconds east of UTC. */
    const char *tm_zone;

/*  Timezone abbreviation. */
};




/*  POSIX.1b structure for timer start values and intervals. */
struct itimerspec {
    struct timespec it_interval;
    struct timespec it_value;
};


/*  We can use a simple forward declaration. */
struct sigevent;


/*  Time base values for timespec_get. */


/* Time used by the program so far (user time + system time).
   The result / CLOCKS_PER_SECOND is program time in seconds.  */
extern clock_t clock(void) __attribute__ ((__nothrow__, __leaf__));


/*  Return the current time and put it in *TIMER if TIMER is not NULL. */
extern time_t time(time_t * __timer)
    __attribute__ ((__nothrow__, __leaf__));


/*  Return the difference between TIME1 and TIME0. */
extern double difftime(time_t __time1, time_t __time0)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__const__));


/*  Return the `time_t' representation of TP and normalize TP. */
extern time_t mktime(struct tm *__tp)
    __attribute__ ((__nothrow__, __leaf__));

/* Format TP into S according to FORMAT.
   Write no more than MAXSIZE characters and return the number
   of characters written, or 0 if it would exceed MAXSIZE.  */
extern size_t strftime(char *__restrict __s, size_t __maxsize,
		       const char *__restrict __format,
		       const struct tm *__restrict __tp)
    __attribute__ ((__nothrow__, __leaf__));


/* Similar to the two functions above but take the information from
   the provided locale and not the global locale.  */

/* Definition of locale datatype.
   Copyright (C) 1997-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
extern size_t strftime_l(char *__restrict __s, size_t __maxsize,
			 const char *__restrict __format,
			 const struct tm *__restrict __tp,
			 __locale_t __loc)
    __attribute__ ((__nothrow__, __leaf__));


/* Return the `struct tm' representation of *TIMER
   in Universal Coordinated Time (aka Greenwich Mean Time).  */
extern struct tm *gmtime(const time_t * __timer)
    __attribute__ ((__nothrow__, __leaf__));

/* Return the `struct tm' representation
   of *TIMER in the local timezone.  */
extern struct tm *localtime(const time_t * __timer)
    __attribute__ ((__nothrow__, __leaf__));


/* Return the `struct tm' representation of *TIMER in UTC,
   using *TP to store the result.  */
extern struct tm *gmtime_r(const time_t * __restrict __timer,
			   struct tm *__restrict __tp)
    __attribute__ ((__nothrow__, __leaf__));

/* Return the `struct tm' representation of *TIMER in local time,
   using *TP to store the result.  */
extern struct tm *localtime_r(const time_t * __restrict __timer,
			      struct tm *__restrict __tp)
    __attribute__ ((__nothrow__, __leaf__));


/* Return a string of the form "Day Mon dd hh:mm:ss yyyy\n"
   that is the representation of TP in this format.  */
extern char *asctime(const struct tm *__tp)
    __attribute__ ((__nothrow__, __leaf__));


/*  Equivalent to `asctime (localtime (timer))'. */
extern char *ctime(const time_t * __timer)
    __attribute__ ((__nothrow__, __leaf__));



/*  Reentrant versions of the above functions. */

/* Return in BUF a string of the form "Day Mon dd hh:mm:ss yyyy\n"
   that is the representation of TP in this format.  */
extern char *asctime_r(const struct tm *__restrict __tp,
		       char *__restrict __buf)
    __attribute__ ((__nothrow__, __leaf__));


/*  Equivalent to `asctime_r (localtime_r (timer, *TMP*), buf)'. */
extern char *ctime_r(const time_t * __restrict __timer,
		     char *__restrict __buf)
    __attribute__ ((__nothrow__, __leaf__));


/*  Defined in localtime.c. */
extern char *__tzname[2];

/*  Current timezone names. */
extern int __daylight;

/*  If daylight-saving time is ever in use. */
extern long int __timezone;

/*  Seconds west of UTC. */


/*  Same as above. */
extern char *tzname[2];

/* Set time conversion information from the TZ environment variable.
   If TZ is not defined, a locale-dependent default is used.  */
extern void tzset(void) __attribute__ ((__nothrow__, __leaf__));
extern int daylight;
extern long int timezone;

/* Set the system time to *WHEN.
   This call is restricted to the superuser.  */
extern int stime(const time_t * __when)
    __attribute__ ((__nothrow__, __leaf__));

/* Nonzero if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 400th is).  */

/* Miscellaneous functions many Unices inherited from the public domain
   localtime package.  These are included only for compatibility.  */


/*  Like `mktime', but for TP represents Universal Time, not local time. */
extern time_t timegm(struct tm *__tp)
    __attribute__ ((__nothrow__, __leaf__));


/*  Another name for `mktime'. */
extern time_t timelocal(struct tm *__tp)
    __attribute__ ((__nothrow__, __leaf__));


/*  Return the number of days in YEAR. */
extern int dysize(int __year) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__));

/* Pause execution for a number of nanoseconds.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int nanosleep(const struct timespec *__requested_time,
		     struct timespec *__remaining);


/*  Get resolution of clock CLOCK_ID. */
extern int clock_getres(clockid_t __clock_id, struct timespec *__res)
    __attribute__ ((__nothrow__, __leaf__));


/*  Get current value of clock CLOCK_ID and store it in TP. */
extern int clock_gettime(clockid_t __clock_id, struct timespec *__tp)
    __attribute__ ((__nothrow__, __leaf__));


/*  Set clock CLOCK_ID to value TP. */
extern int clock_settime(clockid_t __clock_id, const struct timespec *__tp)
    __attribute__ ((__nothrow__, __leaf__));

/* High-resolution sleep with the specified clock.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int clock_nanosleep(clockid_t __clock_id, int __flags,
			   const struct timespec *__req,
			   struct timespec *__rem);


/*  Return clock ID for CPU-time clock. */
extern int clock_getcpuclockid(pid_t __pid, clockid_t * __clock_id)
    __attribute__ ((__nothrow__, __leaf__));


/*  Create new per-process timer using CLOCK_ID. */
extern int timer_create(clockid_t __clock_id,
			struct sigevent *__restrict __evp,
			timer_t * __restrict __timerid)
    __attribute__ ((__nothrow__, __leaf__));


/*  Delete timer TIMERID. */
extern int timer_delete(timer_t __timerid)
    __attribute__ ((__nothrow__, __leaf__));


/*  Set timer TIMERID to VALUE, returning old value in OVALUE. */
extern int timer_settime(timer_t __timerid, int __flags,
			 const struct itimerspec *__restrict __value,
			 struct itimerspec *__restrict __ovalue)
    __attribute__ ((__nothrow__, __leaf__));


/*  Get current value of timer TIMERID and store it in VALUE. */
extern int timer_gettime(timer_t __timerid, struct itimerspec *__value)
    __attribute__ ((__nothrow__, __leaf__));


/*  Get expiration overrun for timer TIMERID. */
extern int timer_getoverrun(timer_t __timerid)
    __attribute__ ((__nothrow__, __leaf__));


/*  Set TS to calendar time based in time base BASE. */
extern int timespec_get(struct timespec *__ts, int __base)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/* Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Copyright (C) 2001-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Define the machine-dependent type `jmp_buf'. x86-64 version. */


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */
typedef long int __jmp_buf[8];


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */


/*  Detach state. */
enum {
    PTHREAD_CREATE_JOINABLE,
    PTHREAD_CREATE_DETACHED
};


/*  Mutex types. */
enum {
    PTHREAD_MUTEX_TIMED_NP,
    PTHREAD_MUTEX_RECURSIVE_NP,
    PTHREAD_MUTEX_ERRORCHECK_NP,
    PTHREAD_MUTEX_ADAPTIVE_NP,
    PTHREAD_MUTEX_NORMAL = PTHREAD_MUTEX_TIMED_NP,
    PTHREAD_MUTEX_RECURSIVE = PTHREAD_MUTEX_RECURSIVE_NP,
    PTHREAD_MUTEX_ERRORCHECK = PTHREAD_MUTEX_ERRORCHECK_NP,
    PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL
};


/*  Robust mutex or not flags. */
enum {
    PTHREAD_MUTEX_STALLED,
    PTHREAD_MUTEX_STALLED_NP = PTHREAD_MUTEX_STALLED,
    PTHREAD_MUTEX_ROBUST,
    PTHREAD_MUTEX_ROBUST_NP = PTHREAD_MUTEX_ROBUST
};


/*  Mutex protocols. */
enum {
    PTHREAD_PRIO_NONE,
    PTHREAD_PRIO_INHERIT,
    PTHREAD_PRIO_PROTECT
};


/*  Read-write lock types. */
enum {
    PTHREAD_RWLOCK_PREFER_READER_NP,
    PTHREAD_RWLOCK_PREFER_WRITER_NP,
    PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP,
    PTHREAD_RWLOCK_DEFAULT_NP = PTHREAD_RWLOCK_PREFER_READER_NP
};

/* Define __PTHREAD_RWLOCK_INT_FLAGS_SHARED to 1 if pthread_rwlock_t
   has the shared field.  All 64-bit architectures have the shared field
   in pthread_rwlock_t.  */


/*  Read-write lock initializers. */


/*  Scheduler inheritance. */
enum {
    PTHREAD_INHERIT_SCHED,
    PTHREAD_EXPLICIT_SCHED
};


/*  Scope handling. */
enum {
    PTHREAD_SCOPE_SYSTEM,
    PTHREAD_SCOPE_PROCESS
};


/*  Process shared or private flag. */
enum {
    PTHREAD_PROCESS_PRIVATE,
    PTHREAD_PROCESS_SHARED
};


/*  Conditional variable handling. */


/*  Cleanup buffers */
struct _pthread_cleanup_buffer {
    void (*__routine) (void *);

/*  Function to call. */
    void *__arg;

/*  Its argument. */
    int __canceltype;

/*  Saved cancellation type. */
    struct _pthread_cleanup_buffer *__prev;

/*  Chaining of cleanup functions. */
};


/*  Cancellation */
enum {
    PTHREAD_CANCEL_ENABLE,
    PTHREAD_CANCEL_DISABLE
};
enum {
    PTHREAD_CANCEL_DEFERRED,
    PTHREAD_CANCEL_ASYNCHRONOUS
};


/*  Single execution handling. */

/* Value returned by 'pthread_barrier_wait' for one of the threads after
   the required number of threads have called this function.
   -1 is distinct from 0 and all errno constants */


/* Create a new thread, starting with execution of START-ROUTINE
   getting passed ARG.  Creation attributed come from ATTR.  The new
   handle is stored in *NEWTHREAD.  */
extern int pthread_create(pthread_t * __restrict __newthread,
			  const pthread_attr_t * __restrict __attr,
			  void *(*__start_routine) (void *),
			  void *__restrict __arg)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 3)));

/* Terminate calling thread.

   The registered cleanup handlers are called via exception handling
   so we cannot mark this function with __THROW.*/
extern void pthread_exit(void *__retval) __attribute__ ((__noreturn__));

/* Make calling thread wait for termination of the thread TH.  The
   exit status of the thread is stored in *THREAD_RETURN, if THREAD_RETURN
   is not NULL.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int pthread_join(pthread_t __th, void **__thread_return);

/* Indicate that the thread TH is never to be joined with PTHREAD_JOIN.
   The resources of TH will therefore be freed immediately when it
   terminates, instead of waiting for another thread to perform PTHREAD_JOIN
   on it.  */
extern int pthread_detach(pthread_t __th)
    __attribute__ ((__nothrow__, __leaf__));


/*  Obtain the identifier of the current thread. */
extern pthread_t pthread_self(void) __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__const__));


/*  Compare two thread identifiers. */
extern int pthread_equal(pthread_t __thread1, pthread_t __thread2)
    __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__const__));


/*  Thread attribute handling. */

/* Initialize thread attribute *ATTR with default attributes
   (detachstate is PTHREAD_JOINABLE, scheduling policy is SCHED_OTHER,
    no user-provided stack).  */
extern int pthread_attr_init(pthread_attr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy thread attribute *ATTR. */
extern int pthread_attr_destroy(pthread_attr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get detach state attribute. */
extern int pthread_attr_getdetachstate(const pthread_attr_t * __attr,
				       int *__detachstate)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set detach state attribute. */
extern int pthread_attr_setdetachstate(pthread_attr_t * __attr,
				       int __detachstate)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get the size of the guard area created for stack overflow
    protection. */
extern int pthread_attr_getguardsize(const pthread_attr_t * __attr,
				     size_t * __guardsize)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the size of the guard area created for stack overflow
    protection. */
extern int pthread_attr_setguardsize(pthread_attr_t * __attr,
				     size_t __guardsize)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return in *PARAM the scheduling parameters of *ATTR. */
extern int pthread_attr_getschedparam(const pthread_attr_t *
				      __restrict __attr,
				      struct sched_param *__restrict
				      __param)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set scheduling parameters (priority, etc) in *ATTR according to
    PARAM. */
extern int pthread_attr_setschedparam(pthread_attr_t * __restrict __attr,
				      const struct sched_param *__restrict
				      __param)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Return in *POLICY the scheduling policy of *ATTR. */
extern int pthread_attr_getschedpolicy(const pthread_attr_t * __restrict
				       __attr, int *__restrict __policy)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set scheduling policy in *ATTR according to POLICY. */
extern int pthread_attr_setschedpolicy(pthread_attr_t * __attr,
				       int __policy)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return in *INHERIT the scheduling inheritance mode of *ATTR. */
extern int pthread_attr_getinheritsched(const pthread_attr_t * __restrict
					__attr, int *__restrict __inherit)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set scheduling inheritance mode in *ATTR according to INHERIT. */
extern int pthread_attr_setinheritsched(pthread_attr_t * __attr,
					int __inherit)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return in *SCOPE the scheduling contention scope of *ATTR. */
extern int pthread_attr_getscope(const pthread_attr_t * __restrict __attr,
				 int *__restrict __scope)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set scheduling contention scope in *ATTR according to SCOPE. */
extern int pthread_attr_setscope(pthread_attr_t * __attr, int __scope)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return the previously set address for the stack. */
extern int pthread_attr_getstackaddr(const pthread_attr_t * __restrict
				     __attr, void **__restrict __stackaddr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2))) __attribute__ ((__deprecated__));

/* Set the starting address of the stack of the thread to be created.
   Depending on whether the stack grows up or down the value must either
   be higher or lower than all the address in the memory block.  The
   minimal size of the block must be PTHREAD_STACK_MIN.  */
extern int pthread_attr_setstackaddr(pthread_attr_t * __attr,
				     void *__stackaddr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1))) __attribute__ ((__deprecated__));


/*  Return the currently used minimal stack size. */
extern int pthread_attr_getstacksize(const pthread_attr_t * __restrict
				     __attr,
				     size_t * __restrict __stacksize)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));

/* Add information about the minimum stack size needed for the thread
   to be started.  This size must never be less than PTHREAD_STACK_MIN
   and must also not exceed the system limits.  */
extern int pthread_attr_setstacksize(pthread_attr_t * __attr,
				     size_t __stacksize)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return the previously set address for the stack. */
extern int pthread_attr_getstack(const pthread_attr_t * __restrict __attr,
				 void **__restrict __stackaddr,
				 size_t * __restrict __stacksize)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2, 3)));

/* The following two interfaces are intended to replace the last two.  They
   require setting the address as well as the size since only setting the
   address will make the implementation on some architectures impossible.  */
extern int pthread_attr_setstack(pthread_attr_t * __attr,
				 void *__stackaddr, size_t __stacksize)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Functions for scheduling control. */

/* Set the scheduling parameters for TARGET_THREAD according to POLICY
   and *PARAM.  */
extern int pthread_setschedparam(pthread_t __target_thread, int __policy,
				 const struct sched_param *__param)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(3)));


/*  Return in *POLICY and *PARAM the scheduling parameters for
    TARGET_THREAD. */
extern int pthread_getschedparam(pthread_t __target_thread,
				 int *__restrict __policy,
				 struct sched_param *__restrict __param)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2, 3)));


/*  Set the scheduling priority for TARGET_THREAD. */
extern int pthread_setschedprio(pthread_t __target_thread, int __prio)
    __attribute__ ((__nothrow__, __leaf__));


/*  Functions for handling initialization. */

/* Guarantee that the initialization function INIT_ROUTINE will be called
   only once, even if pthread_once is executed several times with the
   same ONCE_CONTROL argument. ONCE_CONTROL must point to a static or
   extern variable initialized to PTHREAD_ONCE_INIT.

   The initialization functions might throw exception which is why
   this function is not marked with __THROW.  */
extern int pthread_once(pthread_once_t * __once_control,
			void (*__init_routine) (void))
    __attribute__ ((__nonnull__(1, 2)));

/* Functions for handling cancellation.

   Note that these functions are explicitly not marked to not throw an
   exception in C++ code.  If cancellation is implemented by unwinding
   this is necessary to have the compiler generate the unwind information.  */

/* Set cancelability state of current thread to STATE, returning old
   state in *OLDSTATE if OLDSTATE is not NULL.  */
extern int pthread_setcancelstate(int __state, int *__oldstate);

/* Set cancellation state of current thread to TYPE, returning the old
   type in *OLDTYPE if OLDTYPE is not NULL.  */
extern int pthread_setcanceltype(int __type, int *__oldtype);


/*  Cancel THREAD immediately or at the next possibility. */
extern int pthread_cancel(pthread_t __th);

/* Test for pending cancellation for the current thread and terminate
   the thread as per pthread_exit(PTHREAD_CANCELED) if it has been
   cancelled.  */
extern void pthread_testcancel(void);


/*  Cancellation handling with integration into exception handling. */
typedef struct {
    struct {
	__jmp_buf __cancel_jmp_buf;
	int __mask_was_saved;
    } __cancel_jmp_buf[1];
    void *__pad[4];
} __pthread_unwind_buf_t __attribute__ ((__aligned__));


/*  No special attributes by default. */


/*  Structure to hold the cleanup handler information. */
struct __pthread_cleanup_frame {
    void (*__cancel_routine) (void *);
    void *__cancel_arg;
    int __do_it;
    int __cancel_type;
};

/* Install a cleanup handler: ROUTINE will be called with arguments ARG
   when the thread is canceled or calls pthread_exit.  ROUTINE will also
   be called with arguments ARG when the matching pthread_cleanup_pop
   is executed with non-zero EXECUTE argument.

   pthread_cleanup_push and pthread_cleanup_pop are macros and must always
   be used in matching pairs at the same nesting level of braces.  */
extern void __pthread_register_cancel(__pthread_unwind_buf_t * __buf);

/* Remove a cleanup handler installed by the matching pthread_cleanup_push.
   If EXECUTE is non-zero, the handler function is called. */
extern void __pthread_unregister_cancel(__pthread_unwind_buf_t * __buf);


/*  Internal interface to initiate cleanup. */
extern void __pthread_unwind_next(__pthread_unwind_buf_t * __buf)
    __attribute__ ((__noreturn__))
    __attribute__ ((__weak__));


/*  Function used in the macros. */
struct __jmp_buf_tag;
extern int __sigsetjmp(struct __jmp_buf_tag *__env, int __savemask)
    __attribute__ ((__nothrow__));


/*  Mutex handling. */


/*  Initialize a mutex. */
extern int pthread_mutex_init(pthread_mutex_t * __mutex,
			      const pthread_mutexattr_t * __mutexattr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy a mutex. */
extern int pthread_mutex_destroy(pthread_mutex_t * __mutex)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Try locking a mutex. */
extern int pthread_mutex_trylock(pthread_mutex_t * __mutex)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Lock a mutex. */
extern int pthread_mutex_lock(pthread_mutex_t * __mutex)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Wait until lock becomes available, or specified time passes. */
extern int pthread_mutex_timedlock(pthread_mutex_t * __restrict __mutex,
				   const struct timespec *__restrict
				   __abstime) __attribute__ ((__nothrow__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Unlock a mutex. */
extern int pthread_mutex_unlock(pthread_mutex_t * __mutex)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Get the priority ceiling of MUTEX. */
extern int pthread_mutex_getprioceiling(const pthread_mutex_t *
					__restrict __mutex,
					int *__restrict __prioceiling)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));

/* Set the priority ceiling of MUTEX to PRIOCEILING, return old
   priority ceiling value in *OLD_CEILING.  */
extern int pthread_mutex_setprioceiling(pthread_mutex_t *
					__restrict __mutex,
					int __prioceiling,
					int *__restrict __old_ceiling)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 3)));


/*  Declare the state protected by MUTEX as consistent. */
extern int pthread_mutex_consistent(pthread_mutex_t * __mutex)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Functions for handling mutex attributes. */

/* Initialize mutex attribute object ATTR with default attributes
   (kind is PTHREAD_MUTEX_TIMED_NP).  */
extern int pthread_mutexattr_init(pthread_mutexattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy mutex attribute object ATTR. */
extern int pthread_mutexattr_destroy(pthread_mutexattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get the process-shared flag of the mutex attribute ATTR. */
extern int pthread_mutexattr_getpshared(const pthread_mutexattr_t *
					__restrict __attr,
					int *__restrict __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the process-shared flag of the mutex attribute ATTR. */
extern int pthread_mutexattr_setpshared(pthread_mutexattr_t * __attr,
					int __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return in *KIND the mutex kind attribute in *ATTR. */
extern int pthread_mutexattr_gettype(const pthread_mutexattr_t * __restrict
				     __attr, int *__restrict __kind)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));

/* Set the mutex kind attribute in *ATTR to KIND (either PTHREAD_MUTEX_NORMAL,
   PTHREAD_MUTEX_RECURSIVE, PTHREAD_MUTEX_ERRORCHECK, or
   PTHREAD_MUTEX_DEFAULT).  */
extern int pthread_mutexattr_settype(pthread_mutexattr_t * __attr,
				     int __kind)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return in *PROTOCOL the mutex protocol attribute in *ATTR. */
extern int pthread_mutexattr_getprotocol(const pthread_mutexattr_t *
					 __restrict __attr,
					 int *__restrict __protocol)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));

/* Set the mutex protocol attribute in *ATTR to PROTOCOL (either
   PTHREAD_PRIO_NONE, PTHREAD_PRIO_INHERIT, or PTHREAD_PRIO_PROTECT).  */
extern int pthread_mutexattr_setprotocol(pthread_mutexattr_t * __attr,
					 int __protocol)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return in *PRIOCEILING the mutex prioceiling attribute in *ATTR. */
extern int pthread_mutexattr_getprioceiling(const pthread_mutexattr_t *
					    __restrict __attr,
					    int *__restrict __prioceiling)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the mutex prioceiling attribute in *ATTR to PRIOCEILING. */
extern int pthread_mutexattr_setprioceiling(pthread_mutexattr_t * __attr,
					    int __prioceiling)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get the robustness flag of the mutex attribute ATTR. */
extern int pthread_mutexattr_getrobust(const pthread_mutexattr_t * __attr,
				       int *__robustness)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the robustness flag of the mutex attribute ATTR. */
extern int pthread_mutexattr_setrobust(pthread_mutexattr_t * __attr,
				       int __robustness)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Functions for handling read-write locks. */

/* Initialize read-write lock RWLOCK using attributes ATTR, or use
   the default values if later is NULL.  */
extern int pthread_rwlock_init(pthread_rwlock_t * __restrict __rwlock,
			       const pthread_rwlockattr_t * __restrict
			       __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy read-write lock RWLOCK. */
extern int pthread_rwlock_destroy(pthread_rwlock_t * __rwlock)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Acquire read lock for RWLOCK. */
extern int pthread_rwlock_rdlock(pthread_rwlock_t * __rwlock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Try to acquire read lock for RWLOCK. */
extern int pthread_rwlock_tryrdlock(pthread_rwlock_t * __rwlock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Try to acquire read lock for RWLOCK or return after specfied time. */
extern int pthread_rwlock_timedrdlock(pthread_rwlock_t *
				      __restrict __rwlock,
				      const struct timespec *__restrict
				      __abstime)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));


/*  Acquire write lock for RWLOCK. */
extern int pthread_rwlock_wrlock(pthread_rwlock_t * __rwlock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Try to acquire write lock for RWLOCK. */
extern int pthread_rwlock_trywrlock(pthread_rwlock_t * __rwlock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Try to acquire write lock for RWLOCK or return after specfied time. */
extern int pthread_rwlock_timedwrlock(pthread_rwlock_t *
				      __restrict __rwlock,
				      const struct timespec *__restrict
				      __abstime)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1, 2)));


/*  Unlock RWLOCK. */
extern int pthread_rwlock_unlock(pthread_rwlock_t * __rwlock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Functions for handling read-write lock attributes. */


/*  Initialize attribute object ATTR with default values. */
extern int pthread_rwlockattr_init(pthread_rwlockattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy attribute object ATTR. */
extern int pthread_rwlockattr_destroy(pthread_rwlockattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return current setting of process-shared attribute of ATTR in
    PSHARED. */
extern int pthread_rwlockattr_getpshared(const pthread_rwlockattr_t *
					 __restrict __attr,
					 int *__restrict __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set process-shared attribute of ATTR to PSHARED. */
extern int pthread_rwlockattr_setpshared(pthread_rwlockattr_t * __attr,
					 int __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Return current setting of reader/writer preference. */
extern int pthread_rwlockattr_getkind_np(const pthread_rwlockattr_t *
					 __restrict __attr,
					 int *__restrict __pref)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set reader/write preference. */
extern int pthread_rwlockattr_setkind_np(pthread_rwlockattr_t * __attr,
					 int __pref)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Functions for handling conditional variables. */

/* Initialize condition variable COND using attributes ATTR, or use
   the default values if later is NULL.  */
extern int pthread_cond_init(pthread_cond_t * __restrict __cond,
			     const pthread_condattr_t *
			     __restrict __cond_attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy condition variable COND. */
extern int pthread_cond_destroy(pthread_cond_t * __cond)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Wake up one thread waiting for condition variable COND. */
extern int pthread_cond_signal(pthread_cond_t * __cond)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Wake up all threads waiting for condition variables COND. */
extern int pthread_cond_broadcast(pthread_cond_t * __cond)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));

/* Wait for condition variable COND to be signaled or broadcast.
   MUTEX is assumed to be locked before.

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int pthread_cond_wait(pthread_cond_t * __restrict __cond,
			     pthread_mutex_t * __restrict __mutex)
    __attribute__ ((__nonnull__(1, 2)));

/* Wait for condition variable COND to be signaled or broadcast until
   ABSTIME.  MUTEX is assumed to be locked before.  ABSTIME is an
   absolute time specification; zero is the beginning of the epoch
   (00:00:00 GMT, January 1, 1970).

   This function is a cancellation point and therefore not marked with
   __THROW.  */
extern int pthread_cond_timedwait(pthread_cond_t * __restrict __cond,
				  pthread_mutex_t * __restrict __mutex,
				  const struct timespec *__restrict
				  __abstime)
    __attribute__ ((__nonnull__(1, 2, 3)));


/*  Functions for handling condition variable attributes. */


/*  Initialize condition variable attribute ATTR. */
extern int pthread_condattr_init(pthread_condattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy condition variable attribute ATTR. */
extern int pthread_condattr_destroy(pthread_condattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get the process-shared flag of the condition variable attribute
    ATTR. */
extern int pthread_condattr_getpshared(const pthread_condattr_t *
				       __restrict __attr,
				       int *__restrict __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the process-shared flag of the condition variable attribute
    ATTR. */
extern int pthread_condattr_setpshared(pthread_condattr_t * __attr,
				       int __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get the clock selected for the condition variable attribute ATTR. */
extern int pthread_condattr_getclock(const pthread_condattr_t *
				     __restrict __attr,
				     __clockid_t * __restrict __clock_id)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the clock selected for the condition variable attribute ATTR. */
extern int pthread_condattr_setclock(pthread_condattr_t * __attr,
				     __clockid_t __clock_id)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Functions to handle spinlocks. */

/* Initialize the spinlock LOCK.  If PSHARED is nonzero the spinlock can
   be shared between different processes.  */
extern int pthread_spin_init(pthread_spinlock_t * __lock, int __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy the spinlock LOCK. */
extern int pthread_spin_destroy(pthread_spinlock_t * __lock)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Wait until spinlock LOCK is retrieved. */
extern int pthread_spin_lock(pthread_spinlock_t * __lock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Try to lock spinlock LOCK. */
extern int pthread_spin_trylock(pthread_spinlock_t * __lock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Release spinlock LOCK. */
extern int pthread_spin_unlock(pthread_spinlock_t * __lock)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Functions to handle barriers. */

/* Initialize BARRIER with the attributes in ATTR.  The barrier is
   opened when COUNT waiters arrived.  */
extern int pthread_barrier_init(pthread_barrier_t * __restrict __barrier,
				const pthread_barrierattr_t * __restrict
				__attr, unsigned int __count)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy a previously dynamically initialized barrier BARRIER. */
extern int pthread_barrier_destroy(pthread_barrier_t * __barrier)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Wait on barrier BARRIER. */
extern int pthread_barrier_wait(pthread_barrier_t * __barrier)
    __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__(1)));


/*  Initialize barrier attribute ATTR. */
extern int pthread_barrierattr_init(pthread_barrierattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy previously dynamically initialized barrier attribute ATTR. */
extern int pthread_barrierattr_destroy(pthread_barrierattr_t * __attr)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Get the process-shared flag of the barrier attribute ATTR. */
extern int pthread_barrierattr_getpshared(const pthread_barrierattr_t *
					  __restrict __attr,
					  int *__restrict __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1, 2)));


/*  Set the process-shared flag of the barrier attribute ATTR. */
extern int pthread_barrierattr_setpshared(pthread_barrierattr_t * __attr,
					  int __pshared)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Functions for handling thread-specific data. */

/* Create a key value identifying a location in the thread-specific
   data area.  Each thread maintains a distinct thread-specific data
   area.  DESTR_FUNCTION, if non-NULL, is called with the value
   associated to that key when the key is destroyed.
   DESTR_FUNCTION is not called if the value associated is NULL when
   the key is destroyed.  */
extern int pthread_key_create(pthread_key_t * __key,
			      void (*__destr_function) (void *))
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(1)));


/*  Destroy KEY. */
extern int pthread_key_delete(pthread_key_t __key)
    __attribute__ ((__nothrow__, __leaf__));


/*  Return current value of the thread-specific data slot identified by
    KEY. */
extern void *pthread_getspecific(pthread_key_t __key)
    __attribute__ ((__nothrow__, __leaf__));


/*  Store POINTER in the thread-specific data slot identified by KEY. */
extern int pthread_setspecific(pthread_key_t __key, const void *__pointer)
    __attribute__ ((__nothrow__, __leaf__));


/*  Get ID of CPU-time clock for thread THREAD_ID. */
extern int pthread_getcpuclockid(pthread_t __thread_id,
				 __clockid_t * __clock_id)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__nonnull__(2)));

/* Install handlers to be called when a new process is created with FORK.
   The PREPARE handler is called in the parent process just before performing
   FORK. The PARENT handler is called in the parent process just after FORK.
   The CHILD handler is called in the child process.  Each of the three
   handlers can be NULL, meaning that no handler needs to be called at that
   point.
   PTHREAD_ATFORK can be called several times, in which case the PREPARE
   handlers are called in LIFO order (last added with PTHREAD_ATFORK,
   first called before FORK), and the PARENT and CHILD handlers are called
   in FIFO (first added, first called).  */
extern int pthread_atfork(void (*__prepare) (void),
			  void (*__parent) (void), void (*__child) (void))
    __attribute__ ((__nothrow__, __leaf__));



/*  Macros. */


/*  ======= */

/* Define a facility number that is unique to this library.  The number here
 * is the facility code assigned to the AST library, but it doesn't have to
 * be this number -- it only has to be probably unique.  If that code were
 * ever to change, then you can update this number if you feel it's tidier
 * that way. */

/* Max number of messages which can be deferred when reporting is
   switched off. */

/* This macro expands to an invocation of a specified function, together
   with a call to astAt to record the file and line number at which the
   invocation occurs. These are included in public error reports. This is
   only done for invocations from outside of AST (i.e. public invocations).*/


/*  Define a dummy __attribute__ macro for use on non-GNU compilers. */


/*  Type definitions */


/*  ================ */


/*  Define a structure to hold information about an error context. */
typedef struct AstErrorContext {
    int reporting;

/*  Value of error reporting flag at start of context */
    int ok;

/*  Was the status value OK at start of context? */
    int status;

/*  The status value at the start of the context */
} AstErrorContext;

/* Define a structure holding all data items that are global within the
   error.c file. */
typedef struct AstErrorGlobals {


/*  Reporting flag: delivery of message is supressed if zero. */
    int Reporting;


/*  Error context. */
    const char *Current_File;

/*  Current file name pointer */
    const char *Current_Routine;

/*  Current routine name pointer */
    int Current_Line;

/*  Current line number */
    int Foreign_Set;

/*  Have foreign values been set? */


/*  Un-reported message stack */
    char *Message_Stack[100];
    int Mstack_Size;
} AstErrorGlobals;

/* Structure to hold the internal status variable, and the status
   pointer for a single thread. */
typedef struct AstStatusBlock {
    int internal_status;
    int *status_ptr;
} AstStatusBlock;


/*  Function Macros. */


/*  ================ */


/*  Function prototypes. */


/*  ==================== */
int *astWatch_(int *);
void astClearStatus_(int *);
int *astGetStatusPtr_(void) __attribute__ ((pure));
void astAt_(const char *, const char *, int, int, int *);
int astReporting_(int, int *);
void astError_(int, const char *, int *, ...)
    __attribute__ ((format(printf, 2, 4)));
void astBacktrace_(int *);
void astInitErrorGlobals_(AstErrorGlobals *);
void astErrorPublic_(int, const char *, ...)
    __attribute__ ((format(printf, 2, 3)));


/*  Function interfaces. */


/*  ==================== */

/* These wrap up the functions defined by this module to make them
   easier to use. */

/*
*++
*  Name:
*     mapping.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Mapping class.

*  Invocation:
*     #include "mapping.h"

*  Description:
*     This include file defines the interface to the Mapping class and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this class.
*
*     The Mapping class provides basic facilities for transforming a
*     set of points to give a new set of points and for resampling
*     grids of data. However, it does not have a constructor
*     function. This is because the class only forms a template for
*     deriving new classes which themselves implement specific forms
*     of coordinate transformation. They do this by extending the
*     protected astTransform method provided by this class.

*  Inheritance:
*     The Mapping class inherits from the Object class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Nin (integer)
*        A read-only attribute giving the number of input coordinate
*        values required per point by a Mapping (i.e. the number of
*        dimensions of the space in which input points reside).
*     Nout (integer)
*        A read-only attribute giving the number of output coordinate
*        values generated per point by a Mapping (i.e. the number of
*        dimensions of the space in which output points reside).
*     Invert (integer)
*        A boolean value (0 or 1) which controls which of a Mapping's
*        two possible coordinate transformations is considered the
*        "forward" transformation and which is the "inverse"
*        transformation. If this value is zero (the default), the
*        behaviour will be as defined when the Mapping was first
*        created.  If it is non-zero, the transformations will be
*        inter-changed, so that the Mapping displays the inverse of
*        its original behaviour.
*
*        Note that inverting the boolean sense of the Invert attribute
*        will cause the values of the Nin/Nout and
*        TranForward/TranInverse attributes to be interchanged.
*     Report (integer)
*        A boolean value (0 or 1) which controls whether to report
*        coordinate values when a Mapping is used to transform a set
*        of points. If this value is zero (the default), no report is
*        made. If it is non-zero, the coordinates of each point
*        (before and after transformation) are reported by writing
*        them to standard output.
*
*        This attribute is intended as an aid to debugging and to save
*        having to report values explicitly in simple programs.
*        Unlike other attributes, the value of the Report attribute is
*        not inherited when a Mapping is copied (its value is
*        initially undefined, and therefore defaults to zero, in any
*        copy).
*     IsSimple (boolean)
*        A read-only attribute indicating if the Mapping has been
*        simpified.
*     TranForward (integer)
*        A read-only boolean value (0 or 1) which indicates whether a
*        Mapping is able to transform coordinates in the "forward"
*        direction (i.e. converting input coordinates into output
*        coordinates).
*     TranInverse (integer)
*        A read-only boolean value (0 or 1) which indicates whether a
*        Mapping is able to transform coordinates in the "inverse"
*        direction (i.e. converting output coordinates back into input
*        coordinates).

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Mapping.
*        astGetAttrib
*           Get an attribute value for a Mapping.
*        astSetAttrib
*           Set an attribute value for a Mapping.
*        astTestAttrib
*           Test if an attribute value has been set for a Mapping.

*  New Methods Defined:
*     Public:
*        astDecompose
*           Decompose a Mapping into two component Mappings.
*        astInvert
*           Invert a Mapping.
*        astLinearApprox
*           Form a linear approximation to a Mapping
*        astMapBox
*           Find a bounding box for a Mapping.
*        astQuadApprox
*           Form a quadratic approximation to a Mapping
*        astRate
*           Find rate of change of a Mapping output
*        astRebin<X>
*           Rebin a region of a data grid.
*        astRebinSeq<X>
*           Rebin a region of a sequence of data grids.
*        astResample<X>
*           Resample a region of a data grid.
*        astSimplify
*           Simplify a Mapping.
*        astTran1
*           Transform 1-dimensional coordinates.
*        astTran2
*           Transform 2-dimensional coordinates.
*        astTranGrid
*           Transform an N-dimensional regular grid of positions.
*        astTranN
*           Transform N-dimensional coordinates.
*        astTranP (C only)
*           Transform N-dimensional coordinates held in separate arrays.
*
*     Protected:
*        astClearInvert
*           Clear the Invert attribute value for a Mapping.
*        astClearReport
*           Clear the Report attribute value for a Mapping.
*        astGetInvert
*           Get the Invert attribute value for a Mapping.
*        astGetIsSimple
*           Get the IsSimple attribute.
*        astGetNin
*           Get the number of input coordinates for a Mapping.
*        astGetNout
*           Get the number of output coordinates for a Mapping.
*        astGetReport
*           Get the Report attribute value for a Mapping.
*        astGetTranForward
*           Determine if a Mapping can perform a "forward" coordinate
*           transformation.
*        astGetTranInverse
*           Determine if a Mapping can perform an "inverse" coordinate
*           transformation.
*        astMapList
*           Decompose a Mapping into a sequence of simpler Mappings.
*        astMapSplit
*           Select a subset of Mapping inputs.
*        astMapMerge
*           Simplify a sequence of Mappings.
*        astReportPoints
*           Report the effect of transforming a set of points using a Mapping.
*        astSetInvert
*           Set the Invert attribute value for a Mapping.
*        astSetReport
*           Set the Report attribute value for a Mapping.
*        astTestInvert
*           Test if an Invert attribute value has been set for a Mapping.
*        astTestReport
*           Test if an Report attribute value has been set for a Mapping.
*        astTransform
*           Transform a set of points.

*  Other Class Functions:
*     Public:
*        astIsAMapping
*           Test class membership.
*
*     Protected:
*        astCheckMapping
*           Validate class membership.
*        astInitMapping
*           Initialise a Mapping.
*        astInitMappingVtab
*           Initialise the virtual function table for the Mapping class.
*        astLoadMapping
*           Load a Mapping.

*  Macros:
*     Public:
*        AST__BLOCKAVE
*           Block averaging interpolation.
*        AST__GAUSS
*           Use exp(-k*x*x) spreading.
*        AST__LINEAR
*           Simple linear interpolation.
*        AST__NEAREST
*           Use nearest pixel centre.
*        AST__SINC
*           Use sinc(pi*x) interpolation.
*        AST__SINCCOS
*           Use sinc(pi*x)*cos(k*pi*x) interpolation.
*        AST__SINCGAUSS
*           Use sinc(pi*x)*exp(-k*x*x) interpolation.
*        AST__SINCSINC
*           Use sinc(pi*x)*sinc(k*pi*x) interpolation.
*        AST__SOMB
*           Use somb(pi*x) interpolation.
*        AST__SOMBCOS
*           Use somb(pi*x)*cos(k*pi*x) interpolation.
*        AST__UINTERP
*           Use general user-defined sub-pixel interpolation algorithm.
*        AST__UKERN1
*           Use user-defined 1-d interpolation kernel.
*        AST__URESAMP1, 2, 3 & 4
*           Flags reserved for user-defined purposes.
*        AST__USEBAD
*           Recognise bad pixels?
*        AST__CONSERVEFLUX
*           Conserve flux in astResample?
*        AST__REBININIT
*           Initialise a new sequnece of calls to astRebinSeq<X>
*        AST__REBINEND
*           End a sequnece of calls to astRebinSeq<X>
*        AST__NOBAD
*           Leave bad output pixels unchanged in calls to astResample<X>
*        AST__USEVAR
*           Use variance arrays?

*  Type Definitions:
*     Public:
*        AstMapping
*           Mapping object type.
*
*     Protected:
*        AstMappingVtab
*           Mapping virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     MBT: Mark Taylor (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     30-JAN-1996 (RFWS):
*        Original version.
*     12-JUL-1996 (RFWS):
*        Updated to support the external interface plus various other
*        additions.
*     12-DEC-1996 (RFWS):
*        Added the astMapList method.
*     13-DEC-1996 (RFWS):
*        Added the astMapMerge method.
*     13-DEC-1996 (RFWS):
*        Added the astSimplify method.
*     28-MAY-1998 (RFWS):
*        Added the astMapBox method.
*     12-NOV-1998 (RFWS):
*        Added astResample<X> and associated code.
*     24-NOV-2000 (MBT):
*        Added AST__BLOCKAVE interpolation scheme.
*     9-JAN-2001 (DSB):
*        Changed in and out arguments for TranN from type "double (*)[]"
*        to "double *".
*     8-JAN-2003 (DSB):
*        Added protected astInitMappingVtab method.
*     10-JUL-2003 (DSB):
*        Added method astRate.
*     20-SEP-2004 (DSB):
*        Added method astLinearApprox.
*     30-JUN-2005 (DSB):
*        Added method astRebin
*     1-SEP-2005 (DSB):
*        Added method astRebinSeq
*     31-JAN-2006 (DSB):
*        Added IsSimple attribute.
*     2-MAR-2006 (DSB):
*        Use HAVE_LONG_DOUBLE in place of AST_LONG_DOUBLE
*     8-MAR-2006 (DSB):
*        Add astTranGrid.
*     5-MAY-2009 (DSB):
*        Add astRemoveRegions.
*     26-FEB-2010 (DSB):
*        Added method astQuadApprox.
*--
*/


/*  Include files. */


/*  ============== */


/*  Configuration results */


/*  --------------------- */


/*  config.h. Generated from config.h.in by configure. */


/*  config.h.in. Generated from configure.ac by autoheader. */


/*  use external PAL and ERFA libraries */


/*  #undef EXTERNAL_PAL */


/*  Define to 1 if you have the `backtrace' function. */

/* Define to 1 if you have the declaration of `isfinite', and to 0 if you
   don't. */

/* Define to 1 if you have the declaration of `isnan', and to 0 if you don't.
   */


/*  Define to 1 if you have the <dlfcn.h> header file. */


/*  Define to 1 if you have the <execinfo.h> header file. */


/*  Define to 1 if the system has the type `int64_t'. */


/*  Define to 1 if you have the <inttypes.h> header file. */


/*  Define to 1 if you have the `isfinite' function. */


/*  #undef HAVE_ISFINITE */


/*  Define to 1 if you have the `isnan' function. */


/*  Define to 1 if you have the `pthread' library (-lpthread). */


/*  Define to 1 if the system has the type `long double'. */


/*  Define to 1 if you have the <memory.h> header file. */


/*  The sscanf shows the non-ANSI behaviour reported by Bill Joye */


/*  #undef HAVE_NONANSI_SSCANF */


/*  Define to 1 if the Fortran compiler supports the VAX %LOC extension */


/*  #undef HAVE_PERCENTLOC */


/*  Use the starmem library for memory management */


/*  #undef HAVE_STAR_MEM_H */


/*  Define to 1 if you have the <stdarg.h> header file. */


/*  Define to 1 if you have the <stdint.h> header file. */


/*  Define to 1 if you have the <stdlib.h> header file. */


/*  Define to 1 if you have the `strerror_r' function. */


/*  Define to 1 if you have the <strings.h> header file. */


/*  Define to 1 if you have the <string.h> header file. */


/*  Define to 1 if you have the `strtok_r' function. */


/*  Define to 1 if you have the <sys/stat.h> header file. */


/*  Define to 1 if you have the <sys/types.h> header file. */


/*  Define to 1 if the system has the type `uint64_t'. */


/*  Define to 1 if you have the <unistd.h> header file. */


/*  Define to 1 if you have the <varargs.h> header file. */


/*  #undef HAVE_VARARGS_H */


/*  Define to 1 if you have the `vsnprintf' function. */

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */


/*  enable AST memory leak debugging functions in memory.c */


/*  #undef MEM_DEBUG */


/*  Name of package */


/*  Define to the address where bug reports for this package should be
    sent. */


/*  Define to the full name of this package. */


/*  Define to the full name and version of this package. */


/*  Define to the one symbol short name of this package. */


/*  Define to the home page for this package. */


/*  Define to the version of this package. */


/*  The size of `long', as computed by sizeof. */


/*  The size of `long long', as computed by sizeof. */


/*  The size of `void*', as computed by sizeof. */


/*  Define to 1 if you have the ANSI C header files. */


/*  Type of Fortran CNF TRAIL argument */


/*  Version number of package */


/*  Interface definitions. */


/*  ---------------------- */

/*
*++
*  Name:
*     object.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Object class.

*  Invocation:
*     #include "object.h"

*  Description:
*     This include file defines the interface to the Object class and
*     provides the type definitions, function prototypes and macros,
*     etc.  needed to use this class.

*     The Object class is the base class from which all other classes
*     in the AST library are derived. This class provides all the
*     basic Object behaviour and Object manipulation facilities
*     required throughout the library. There is no Object constructor,
*     however, as Objects on their own are not of much use.

*  Inheritance:
*     The Object base class does not inherit from any other class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Class (string)
*        This is a read-only attribute containing the name of the
*        class to which an Object belongs.
*     ID (string)
*        An identification string which may be used to identify the
*        Object (e.g.) in debugging output, or when stored in an
*        external medium such as a data file. There is no restriction
*        on the string's contents. The default is an empty string.
*     Ident (string)
*        Like ID, this is an identification string which may be used
*        to identify the Object. Unlike ID, Ident is transferred when an
*        Object is copied.
*     UseDefs (int)
*        Should default values be used for unset attributes?
*     Nobject (integer)
*        This is a read-only attribute which gives the total number of
*        Objects currently in existence in the same class as the
*        Object given. It does not include Objects which belong to
*        derived (more specialised) classes. This value is mainly
*        intended for debugging, as it can be used to show whether
*        Objects which should have been deleted have, in fact, been
*        deleted.
*     ObjSize (int)
*        The in-memory size of the Object in bytes.
*     RefCount (integer)
*        This is a read-only Attribute which gives the "reference
*        count" (the number of active pointers) associated with an
*        Object. It is modified whenever pointers are created or
*        annulled (by astClone or astAnnul/astEnd for example) and
*        includes the initial pointer issued when the Object was
*        created. If the reference count for an Object falls to zero
*        as the result of annulling a pointer to it, then the Object
*        will be deleted.

*  Methods Over-Ridden:
*     None.

*  New Methods Defined:
*     Public:
*        astAnnul
*           Annul a pointer to an Object.
*        astClear
*           Clear attribute values for an Object.
*        astClone
*           Clone a pointer to an Object.
*        astCopy
*           Copy an Object.
*        astDelete
*           Delete an Object.
*        astExempt
*           Exempt an Object pointer from AST context handling
*        astExport
*           Export an Object pointer to an outer context.
*        astGet<X>, where <X> = C, D, F, I, L
*           Get an attribute value for an Object.
*        astImport
*           Import an Object pointer into the current context.
*        astSame
*           Return true if two pointers refer to the same object.
*        astSet
*           Set attribute values for an Object.
*        astSet<X>, where <X> = C, D, F, I, L
*           Set an attribute value for an Object.
*        astShow
*           Display a textual representation of an Object on standard output.
*        astTest
*           Test if an attribute value is set for an Object.
*        astTune
*           Get or set the value of a global AST tuning parameter.
*
*     Protected:
*        astAnnulId
*           Annul an external ID for an Object (for use from protected code
*           which must handle external IDs).
*        astClearAttrib
*           Clear the value of a specified attribute for an Object.
*        astClearID
*           Clear the value of the ID attribute for an Object.
*        astClearIdent
*           Clear the value of the Ident attribute for an Object.
*        astCast
*           Return a deep copy of an object, cast into an instance of a
*           parent class.
*        astDump
*           Write an Object to a Channel.
*        astEqual
*           Are two Objects equivalent?
*        astGetAttrib
*           Get the value of a specified attribute for an Object.
*        astGetClass (deprecated synonym astClass)
*           Obtain the value of the Class attribute for an Object.
*        astGetID
*           Obtain the value of the ID attribute for an Object.
*        astGetIdent
*           Obtain the value of the Ident attribute for an Object.
*        astGetNobject
*           Obtain the value of the Nobject attribute for an Object.
*        astGetRefCount
*           Obtain the value of the RefCount attribute for an Object.
*        astSetAttrib
*           Set the value of a specified attribute for an Object.
*        astSetCopy
*           Declare a copy constructor for an Object.
*        astSetDelete
*           Declare a destructor for an Object.
*        astSetDump
*           Declare a dump function for an Object.
*        astSetVtab
*           Chaneg the virtual function table associated with an Object.
*        astSetID
*           Set the value of the ID attribute for an Object.
*        astSetIdent
*           Set the value of the Ident attribute for an Object.
*        astTestAttrib
*           Test if a specified attribute value is set for an Object.
*        astTestID
*           Test whether the ID attribute for an Object is set.
*        astTestIdent
*           Test whether the Ident attribute for an Object is set.
*        astVSet
*           Set values for an Object's attributes.

*  Other Class Functions:
*     Public:
*        astBegin
*           Begin a new AST context.
*        astEnd
*           End an AST context.
*        astIsAObject
*           Test class membership.
*        astVersion
*           Returns the AST library version number.
*        astEscapes
*           Remove escape sequences from returned text strings?
*        astP2I
*           Retrieve an int from a pointer.
*        astI2P
*           Pack an int into a pointer.
*
*     Protected:
*        astCheckObject
*           Validate class membership.
*        astInitObject
*           Initialise an Object.
*        astInitObjectVtab
*           Initialise the virtual function table for the Object class.
*        astLoadObject
*           Load an Object.
*        astMakeId
*           Issue an identifier for an Object.
*        astMakePointer
*           Obtain a true C pointer from an Object identifier.

*  Macros:
*     Public:
*        AST__NULL
*           Null Object pointer value.
*        AST__VMAJOR
*           The AST library major version number.
*        AST__VMINOR
*           The AST library minor version number.
*        AST__RELEASE
*           The AST library release number.
*
*     Protected:
*        astEQUAL
*           Compare two doubles for equality.
*        astMAX
*           Return maximum of two values.
*        astMIN
*           Return minimum of two values.
*        astMAKE_CHECK
*           Implement the astCheck<Class>_ function for a class.
*        astMAKE_CLEAR
*           Implement a method to clear an attribute value for a class.
*        astMAKE_GET
*           Implement a method to get an attribute value for a class.
*        astMAKE_ISA
*           Implement the astIsA<Class>_ function for a class.
*        astMAKE_SET
*           Implement a method to set an attribute value for a class.
*        astMAKE_TEST
*           Implement a method to test if an attribute has been set for a
*           class.
*        astMEMBER
*           Locate a member function.

*  Type Definitions:
*     Public:
*        AstObject
*           Object type.
*
*     Protected:
*        AstObjectVtab
*           Object virtual function table type.

*  Feature Test Macros:
*     AST_CHECK_CLASS
*        If the AST_CHECK_CLASS macro is defined, then Object class
*        checking is enabled for all internal function invocations
*        within the AST library. Otherwise, this checking is
*        omitted. This macro should normally be defined as a compiler
*        option during library development and debugging, but left
*        undefined for software releases, so as to improve
*        peformance. Class checking by the AST public interface is not
*        affected by this macro.
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.
*     astFORTRAN77
*        If the astFORTRAN77 macro is defined, reporting of error
*        context information is suppressed. This is necessary when
*        implementing foreign language interfaces to the AST library, as
*        otherwise the file names and line numbers given would refer
*        to the interface implementation rather than the user's own
*        code.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     30-JAN-1996 (RFWS):
*        Original version.
*     19-APR-1996 (RFWS):
*        Added macros for implementing attribute access methods.
*     3-JUL-1996 (RFWS):
*        Added new definitions to support the external interface.
*     10-SEP-1996 (RFWS):
*        Added loader and related facilities.
*     30-MAY-1997 (RFWS):
*        Add the ID attribute.
*     14-JUL-1997 (RFWS):
*        Add astExempt function.
*     20-JAN-1998 (RFWS):
*        Make the astClear and astVSet methods virtual.
*     15-SEP-1999 (RFWS):
*        Made the astAnnulId function accessible to protected code.
*     3-APR-2001 (DSB):
*        Added Ident attribute.
*     8-JAN-2003 (DSB):
*        Added protected astInitObjectVtab method.
*     30-APR-2003 (DSB):
*        Added macros AST__VMAJOR, AST__VMINOR and AST__RELEASE.
*        Added astVersion function.
*     7-FEB-2004 (DSB):
*        Added astEscapes function.
*     11-MAR-2005 (DSB):
*        Added UseDefs attribute.
*     7-FEB-2006 (DSB):
*        Added astTune function.
*     14-FEB-2006 (DSB):
*        Added ObjSize attribute.
*     23-FEB-2006 (DSB):
*        Moved AST__TUNULL from this file to memory.h.
*     10-MAY-2006 (DSB):
*        Added astEQUAL, astMAX and astMIN.
*     26-MAY-2006 (DSB):
*        Make all system includes unconditional, so that makeh is not
*        confused when creating ast.h.
*     22-JUN-2007 (DSB):
*        - Make astVSet return a pointer to dynamic memory holding the
*        expanded setting string.
*        - Add ast astSetVtab and astCast.
*     22-APR-2008 (DSB):
*        Added astSame.
*     7-APR-2010 (DSB):
*        Added astHasAttribute.
*--
*/


/*  Include files. */


/*  ============== */


/*  Configuration results. */


/*  ---------------------- */


/*  config.h. Generated from config.h.in by configure. */


/*  config.h.in. Generated from configure.ac by autoheader. */


/*  use external PAL and ERFA libraries */


/*  #undef EXTERNAL_PAL */


/*  Define to 1 if you have the `backtrace' function. */

/* Define to 1 if you have the declaration of `isfinite', and to 0 if you
   don't. */

/* Define to 1 if you have the declaration of `isnan', and to 0 if you don't.
   */


/*  Define to 1 if you have the <dlfcn.h> header file. */


/*  Define to 1 if you have the <execinfo.h> header file. */


/*  Define to 1 if the system has the type `int64_t'. */


/*  Define to 1 if you have the <inttypes.h> header file. */


/*  Define to 1 if you have the `isfinite' function. */


/*  #undef HAVE_ISFINITE */


/*  Define to 1 if you have the `isnan' function. */


/*  Define to 1 if you have the `pthread' library (-lpthread). */


/*  Define to 1 if the system has the type `long double'. */


/*  Define to 1 if you have the <memory.h> header file. */


/*  The sscanf shows the non-ANSI behaviour reported by Bill Joye */


/*  #undef HAVE_NONANSI_SSCANF */


/*  Define to 1 if the Fortran compiler supports the VAX %LOC extension */


/*  #undef HAVE_PERCENTLOC */


/*  Use the starmem library for memory management */


/*  #undef HAVE_STAR_MEM_H */


/*  Define to 1 if you have the <stdarg.h> header file. */


/*  Define to 1 if you have the <stdint.h> header file. */


/*  Define to 1 if you have the <stdlib.h> header file. */


/*  Define to 1 if you have the `strerror_r' function. */


/*  Define to 1 if you have the <strings.h> header file. */


/*  Define to 1 if you have the <string.h> header file. */


/*  Define to 1 if you have the `strtok_r' function. */


/*  Define to 1 if you have the <sys/stat.h> header file. */


/*  Define to 1 if you have the <sys/types.h> header file. */


/*  Define to 1 if the system has the type `uint64_t'. */


/*  Define to 1 if you have the <unistd.h> header file. */


/*  Define to 1 if you have the <varargs.h> header file. */


/*  #undef HAVE_VARARGS_H */


/*  Define to 1 if you have the `vsnprintf' function. */

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */


/*  enable AST memory leak debugging functions in memory.c */


/*  #undef MEM_DEBUG */


/*  Name of package */


/*  Define to the address where bug reports for this package should be
    sent. */


/*  Define to the full name of this package. */


/*  Define to the full name and version of this package. */


/*  Define to the one symbol short name of this package. */


/*  Define to the home page for this package. */


/*  Define to the version of this package. */


/*  The size of `long', as computed by sizeof. */


/*  The size of `long long', as computed by sizeof. */


/*  The size of `void*', as computed by sizeof. */


/*  Define to 1 if you have the ANSI C header files. */


/*  Type of Fortran CNF TRAIL argument */


/*  Version number of package */


/*  Interface definitions. */


/*  ---------------------- */

/*
*+
*  Name:
*     version.h

*  Purpose:
*     Declare version numbers

*  Description:
*     Defines macros which expand to the components of the AST version
*     number, namely the major and minor version numbers, and the
*     release number.  The version number as a string is available by
*     including the file config.h, which defines macros PACKAGE_STRING,
*     PACKAGE_VERSION and (equivalently to the latter) VERSION.
*
*     For example, the version string `3.2.1' corresponds to major version
*     3, minor version 2, release 1.

*  Macros defined:
*     AST__VMAJOR
*        The AST major version number
*     AST__VMINOR
*        The AST minor version number
*     AST__RELEASE
*        The AST release number
*
*     For backwards compatibility, this module also declares macros
*     AST_MAJOR_VERS, AST_MINOR_VERS and AST_RELEASE.  The AST__*
*     macros should be used in preference to these, since the latter
*     use (non-standard) single underscores.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     NG: Norman Gray (Starlink)

*  History:
*     25-NOV-2003 (NG):
*        Original version
*-
*/


/*  The current version of AST is 8.0.5 */


/*  Deprecated macros */


/*  C header files. */


/*  --------------- */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */


/*  snaroff@next.com says the NeXT needs this. */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef long int ptrdiff_t;


/*  If this symbol has done its job, get rid of it. */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */


/*  Offset of member MEMBER in a struct of type TYPE. */

/* Type whose alignment is supported in every context and is at least
   as great as that of any standard type not using alignment
   specifiers.  */
typedef struct {
    long long __max_align_ll
	__attribute__ ((__aligned__(__alignof__(long long))));
    long double __max_align_ld
	__attribute__ ((__aligned__(__alignof__(long double))));
} max_align_t;

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
 */


/*  Define __gnuc_va_list. */
typedef __builtin_va_list __gnuc_va_list;

/* Define the standard macros for the user,
   if this invocation was from the user program.  */


/*  Define va_list, if desired, from __gnuc_va_list. */

/* We deliberately do not define va_list when called from
   stdio.h, because ANSI C says that stdio.h is not supposed to define
   va_list.  stdio.h needs to have access to that data type, 
   but must not use that name.  It should use the name __gnuc_va_list,
   which is safe because it is reserved for the implementation.  */

/* The macro _VA_LIST_ is the same thing used by this file in Ultrix.
   But on BSD NET2 we must not test or define or undef it.
   (Note that the comments in NET 2's ansi.h
   are incorrect for _VA_LIST_--see stdio.h!)  */


/*  The macro _VA_LIST_DEFINED is used in Windows NT 3.5 */


/*  The macro _VA_LIST is used in SCO Unix 3.2. */


/*  The macro _VA_LIST_T_H is used in the Bull dpx2 */


/*  The macro __va_list__ is used by BeOS. */
typedef __gnuc_va_list va_list;

/* Copyright (C) 2002-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  5.2.4.2.2  Characteristics of floating types <float.h>
 */


/*  Radix of exponent representation, b. */


/*  Number of base-FLT_RADIX digits in the significand, p. */

/* Number of decimal digits, q, such that any floating-point number with q
   decimal digits can be rounded into a floating-point number with p radix b
   digits and back again without change to the q decimal digits,

	p * log10(b)			if b is a power of 10
	floor((p - 1) * log10(b))	otherwise
*/


/*  Minimum int x such that FLT_RADIX**(x-1) is a normalized float, emin */

/* Minimum negative integer such that 10 raised to that power is in the
   range of normalized floating-point numbers,

	ceil(log10(b) * (emin - 1))
*/


/*  Maximum int x such that FLT_RADIX**(x-1) is a representable float,
    emax. */

/* Maximum integer such that 10 raised to that power is in the range of
   representable finite floating-point numbers,

	floor(log10((1 - b**-p) * b**emax))
*/

/* Maximum representable finite floating-point number,

	(1 - b**-p) * b**emax
*/

/* The difference between 1 and the least value greater than 1 that is
   representable in the given floating point type, b**1-p.  */


/*  Minimum normalized positive floating-point number, b**(emin - 1). */


/*  Addition rounds to 0: zero, 1: nearest, 2: +inf, 3: -inf, -1:
    unknown. */


/*  ??? This is supposed to change with calls to fesetround in <fenv.h>. */

/* The floating-point expression evaluation method.
        -1  indeterminate
         0  evaluate all operations and constants just to the range and
            precision of the type
         1  evaluate operations and constants of type float and double
            to the range and precision of the double type, evaluate
            long double operations and constants to the range and
            precision of the long double type
         2  evaluate all operations and constants to the range and
            precision of the long double type

   ??? This ought to change with the setting of the fp control word;
   the value provided by the compiler assumes the widest setting.  */

/* Number of decimal digits, n, such that any floating-point number in the
   widest supported floating type with pmax radix b digits can be rounded
   to a floating-point number with n decimal digits and back again without
   change to the value,

	pmax * log10(b)			if b is a power of 10
	ceil(1 + pmax * log10(b))	otherwise
*/


/*  Versions of DECIMAL_DIG for each floating-point type. */


/*  Whether types support subnormal numbers. */


/*  Minimum positive values, including subnormals. */

/* Define ISO C stdio on top of C++ iostreams.
   Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard: 7.19 Input/output	<stdio.h>
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  If this symbol has done its job, get rid of it. */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */


/*  Offset of member MEMBER in a struct of type TYPE. */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */


/*  Define outside of namespace so the C++ is happy. */
struct _IO_FILE;



/*  The opaque type of streams. This is the definition used elsewhere. */
typedef struct _IO_FILE FILE;




/*  The opaque type of streams. This is the definition used elsewhere. */
typedef struct _IO_FILE __FILE;

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Written by Per Bothner <bothner@cygnus.com>.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.

   As a special exception, if you link the code in this file with
   files compiled with a GNU compiler to produce an executable,
   that does not cause the resulting executable to be covered by
   the GNU Lesser General Public License.  This exception does not
   however invalidate any other reasons why the executable file
   might be covered by the GNU Lesser General Public License.
   This exception applies to code released by its copyright holders
   in files containing the exception.  */

/* This file is needed by libio to define various configuration parameters.
   These are always the same in the GNU C library.  */


/*  Define types for libio in terms of the standard internal type names. */

/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 * Never include this file directly; use <sys/types.h> instead.
 */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */

/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */


/*  On FreeBSD 5, machine/ansi.h does not exist anymore... */

/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_.
   NetBSD defines _I386_ANSI_H_ and _X86_64_ANSI_H_ instead of _ANSI_H_ */

/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */

/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */


/*  Signed type of difference of two pointers. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */


/*  If this symbol has done its job, get rid of it. */


/*  Unsigned type of `sizeof' something. */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */

/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */

/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */


/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here. */


/*  NetBSD 5 requires the I386_ANSI_H and X86_64_ANSI_H checks here. */


/*  A null pointer constant. */


/*  Offset of member MEMBER in a struct of type TYPE. */

/* Copyright (C) 1995-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *      ISO C99 Standard: 7.24
 *	Extended multibyte and wide character utilities	<wchar.h>
 */


/*  Conversion state information. */
typedef struct {
    int __count;
    union {
	unsigned int __wch;
	char __wchb[4];
    } __value;

/*  Value so far. */
} __mbstate_t;

/* The rest of the file is only used if used if __need_mbstate_t is not
   defined.  */

/* Undefine all __need_* constants in case we are included to get those
   constants but the whole file was already read.  */
typedef struct {
    __off_t __pos;
    __mbstate_t __state;
} _G_fpos_t;
typedef struct {
    __off64_t __pos;
    __mbstate_t __state;
} _G_fpos64_t;


/*  These library features are always available in the GNU C library. */


/*  This is defined by <bits/stat.h> if `st_blksize' exists. */


/*  ALL of these should be defined in _G_config.h */


/*  This define avoids name pollution if we're using GNU stdarg.h */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
 */

/* Magic numbers and bits for the _flags field.
   The magic numbers use the high-order bits of _flags;
   the remaining bits are available for variable flags.
   Note: The magic numbers must all be negative if stdio
   emulation is desired. */


/*  These are "formatting flags" matching the iostream fmtflags enum
    values. */
struct _IO_jump_t;
struct _IO_FILE;


/*  Handle lock. */
typedef void _IO_lock_t;


/*  A streammarker remembers a position in a buffer. */
struct _IO_marker {
    struct _IO_marker *_next;
    struct _IO_FILE *_sbuf;

/* If _pos >= 0
       it points to _buf->Gbase()+_pos. FIXME comment */


/*  if _pos < 0, it points to _buf->eBptr()+_pos. FIXME comment */
    int _pos;
};


/*  This is the structure from the libstdc++ codecvt class. */
enum __codecvt_result {
    __codecvt_ok,
    __codecvt_partial,
    __codecvt_error,
    __codecvt_noconv
};
struct _IO_FILE {
    int _flags;

/*  High-order word is _IO_MAGIC; rest is flags. */


/*  The following pointers correspond to the C++ streambuf protocol. */


/*  Note: Tk uses the _IO_read_ptr and _IO_read_end fields directly. */
    char *_IO_read_ptr;

/*  Current read pointer */
    char *_IO_read_end;

/*  End of get area. */
    char *_IO_read_base;

/*  Start of putback+get area. */
    char *_IO_write_base;

/*  Start of put area. */
    char *_IO_write_ptr;

/*  Current put pointer. */
    char *_IO_write_end;

/*  End of put area. */
    char *_IO_buf_base;

/*  Start of reserve area. */
    char *_IO_buf_end;

/*  End of reserve area. */


/*  The following fields are used to support backing up and undo. */
    char *_IO_save_base;

/*  Pointer to start of non-current get area. */
    char *_IO_backup_base;

/*  Pointer to first valid character of backup area */
    char *_IO_save_end;

/*  Pointer to end of non-current get area. */
    struct _IO_marker *_markers;
    struct _IO_FILE *_chain;
    int _fileno;
    int _flags2;
    __off_t _old_offset;

/*  This used to be _offset but it's too small. */


/*  1+column number of pbase(); 0 is unknown. */
    unsigned short _cur_column;
    signed char _vtable_offset;
    char _shortbuf[1];


/*  char* _save_gptr; char* _save_egptr; */
    _IO_lock_t *_lock;
    __off64_t _offset;
    void *__pad1;
    void *__pad2;
    void *__pad3;
    void *__pad4;
    size_t __pad5;
    int _mode;


/*  Make sure we don't get into trouble again. */
    char _unused2[15 * sizeof(int) - 4 * sizeof(void *) - sizeof(size_t)];
};
typedef struct _IO_FILE _IO_FILE;
struct _IO_FILE_plus;
extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;


/*  Functions to do I/O and file management for a stream. */

/* Read NBYTES bytes from COOKIE into a buffer pointed to by BUF.
   Return number of bytes read.  */
typedef __ssize_t __io_read_fn(void *__cookie, char *__buf,
			       size_t __nbytes);

/* Write N bytes pointed to by BUF to COOKIE.  Write all N bytes
   unless there is an error.  Return number of bytes written.  If
   there is an error, return 0 and do not write anything.  If the file
   has been opened for append (__mode.__append set), then set the file
   pointer to the end of the file and then do the write; if not, just
   write at the current file pointer.  */
typedef __ssize_t __io_write_fn(void *__cookie, const char *__buf,
				size_t __n);

/* Move COOKIE's file position to *POS bytes from the
   beginning of the file (if W is SEEK_SET),
   the current position (if W is SEEK_CUR),
   or the end of the file (if W is SEEK_END).
   Set *POS to the new file position.
   Returns zero if successful, nonzero if not.  */
typedef int __io_seek_fn(void *__cookie, __off64_t * __pos, int __w);


/*  Close COOKIE. */
typedef int __io_close_fn(void *__cookie);
extern int __underflow(_IO_FILE *);
extern int __uflow(_IO_FILE *);
extern int __overflow(_IO_FILE *, int);
extern int _IO_getc(_IO_FILE * __fp);
extern int _IO_putc(int __c, _IO_FILE * __fp);
extern int _IO_feof(_IO_FILE * __fp)
    __attribute__ ((__nothrow__, __leaf__));
extern int _IO_ferror(_IO_FILE * __fp)
    __attribute__ ((__nothrow__, __leaf__));
extern int _IO_peekc_locked(_IO_FILE * __fp);


/*  This one is for Emacs. */
extern void _IO_flockfile(_IO_FILE *)
    __attribute__ ((__nothrow__, __leaf__));
extern void _IO_funlockfile(_IO_FILE *)
    __attribute__ ((__nothrow__, __leaf__));
extern int _IO_ftrylockfile(_IO_FILE *)
    __attribute__ ((__nothrow__, __leaf__));
extern int _IO_vfscanf(_IO_FILE * __restrict, const char *__restrict,
		       __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf(_IO_FILE * __restrict, const char *__restrict,
			__gnuc_va_list);
extern __ssize_t _IO_padn(_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn(_IO_FILE *, void *, size_t);
extern __off64_t _IO_seekoff(_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos(_IO_FILE *, __off64_t, int);
extern void _IO_free_backup_area(_IO_FILE *)
    __attribute__ ((__nothrow__, __leaf__));


/*  The type of the second argument to `fgetpos' and `fsetpos'. */

typedef _G_fpos_t fpos_t;



/*  The possibilities for the third argument to `setvbuf'. */


/*  Default buffer size. */

/* End of file character.
   Some things throughout the library rely on this being -1.  */

/* The possibilities for the third argument to `fseek'.
   These values should not be changed.  */


/*  Default path prefix for `tempnam' and `tmpnam'. */

/* Get the values:
   L_tmpnam	How long an array of chars must be to be passed to `tmpnam'.
   TMP_MAX	The minimum number of unique filenames generated by tmpnam
		(and tempnam when it uses tmpnam's name space),
		or tempnam (the two are separate).
   L_ctermid	How long an array to pass to `ctermid'.
   L_cuserid	How long an array to pass to `cuserid'.
   FOPEN_MAX	Minimum number of files that can be open at once.
   FILENAME_MAX	Maximum length of a filename.  */

/* Copyright (C) 1994-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Standard streams. */
extern struct _IO_FILE *stdin;

/*  Standard input stream. */
extern struct _IO_FILE *stdout;

/*  Standard output stream. */
extern struct _IO_FILE *stderr;

/*  Standard error output stream. */


/*  C89/C99 say they're macros. Make them happy. */



/*  Remove file FILENAME. */
extern int remove(const char *__filename)
    __attribute__ ((__nothrow__, __leaf__));


/*  Rename file OLD to NEW. */
extern int rename(const char *__old, const char *__new)
    __attribute__ ((__nothrow__, __leaf__));



/*  Rename file OLD relative to OLDFD to NEW relative to NEWFD. */
extern int renameat(int __oldfd, const char *__old, int __newfd,
		    const char *__new)
    __attribute__ ((__nothrow__, __leaf__));


/* Create a temporary file and open it read/write.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *tmpfile(void)
/* Ignore */ ;


/*  Generate a temporary filename. */
extern char *tmpnam(char *__s)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/* This is the reentrant variant of `tmpnam'.  The only difference is
   that it does not allow S to be NULL.  */
extern char *tmpnam_r(char *__s)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;

/* Generate a unique temporary filename using up to five characters of PFX
   if it is not NULL.  The directory to put this file in is searched for
   as follows: First the environment variable "TMPDIR" is checked.
   If it contains the name of a writable directory, that directory is used.
   If not and if DIR is not NULL, that value is checked.  If that fails,
   P_tmpdir is tried and finally "/tmp".  The storage for the filename
   is allocated by `malloc'.  */
extern char *tempnam(const char *__dir, const char *__pfx)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__malloc__))
/* Ignore */ ;


/* Close STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fclose(FILE * __stream);

/* Flush STREAM, or all streams if STREAM is NULL.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fflush(FILE * __stream);


/* Faster versions when locking is not required.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fflush_unlocked(FILE * __stream);


/* Open a file and create a new stream for it.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *fopen(const char *__restrict __filename,
		   const char *__restrict __modes)
/* Ignore */ ;

/* Open a file, replacing an existing stream with it.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *freopen(const char *__restrict __filename,
		     const char *__restrict __modes,
		     FILE * __restrict __stream)
/* Ignore */ ;



/*  Create a new stream that refers to an existing system file
    descriptor. */
extern FILE *fdopen(int __fd, const char *__modes)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/*  Create a new stream that refers to a memory buffer. */
extern FILE *fmemopen(void *__s, size_t __len, const char *__modes)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;

/* Open a stream that writes into a malloc'd buffer that is expanded as
   necessary.  *BUFLOC and *SIZELOC are updated with the buffer's location
   and the number of characters written on fflush or fclose.  */
extern FILE *open_memstream(char **__bufloc, size_t * __sizeloc)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/* If BUF is NULL, make STREAM unbuffered.
   Else make it use buffer BUF, of size BUFSIZ.  */
extern void setbuf(FILE * __restrict __stream, char *__restrict __buf)
    __attribute__ ((__nothrow__, __leaf__));

/* Make STREAM use buffering mode MODE.
   If BUF is not NULL, use N bytes of it for buffering;
   else allocate an internal buffer N bytes long.  */
extern int setvbuf(FILE * __restrict __stream, char *__restrict __buf,
		   int __modes, size_t __n)
    __attribute__ ((__nothrow__, __leaf__));


/* If BUF is NULL, make STREAM unbuffered.
   Else make it use SIZE bytes of BUF for buffering.  */
extern void setbuffer(FILE * __restrict __stream, char *__restrict __buf,
		      size_t __size)
    __attribute__ ((__nothrow__, __leaf__));


/*  Make STREAM line-buffered. */
extern void setlinebuf(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__));


/* Write formatted output to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fprintf(FILE * __restrict __stream,
		   const char *__restrict __format, ...);

/* Write formatted output to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int printf(const char *__restrict __format, ...);


/*  Write formatted output to S. */
extern int sprintf(char *__restrict __s,
		   const char *__restrict __format, ...)
    __attribute__ ((__nothrow__));

/* Write formatted output to S from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vfprintf(FILE * __restrict __s, const char *__restrict __format,
		    __gnuc_va_list __arg);

/* Write formatted output to stdout from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vprintf(const char *__restrict __format, __gnuc_va_list __arg);


/*  Write formatted output to S from argument list ARG. */
extern int vsprintf(char *__restrict __s, const char *__restrict __format,
		    __gnuc_va_list __arg) __attribute__ ((__nothrow__));




/*  Maximum chars of output to write in MAXLEN. */
extern int snprintf(char *__restrict __s, size_t __maxlen,
		    const char *__restrict __format, ...)
    __attribute__ ((__nothrow__))
    __attribute__ ((__format__(__printf__, 3, 4)));
extern int vsnprintf(char *__restrict __s, size_t __maxlen,
		     const char *__restrict __format, __gnuc_va_list __arg)
    __attribute__ ((__nothrow__))
    __attribute__ ((__format__(__printf__, 3, 0)));



/*  Write formatted output to a file descriptor. */
extern int vdprintf(int __fd, const char *__restrict __fmt,
		    __gnuc_va_list __arg)
    __attribute__ ((__format__(__printf__, 2, 0)));
extern int dprintf(int __fd, const char *__restrict __fmt, ...)
    __attribute__ ((__format__(__printf__, 2, 3)));


/* Read formatted input from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fscanf(FILE * __restrict __stream,
		  const char *__restrict __format, ...)
/* Ignore */ ;

/* Read formatted input from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int scanf(const char *__restrict __format, ...)
/* Ignore */ ;


/*  Read formatted input from S. */
extern int sscanf(const char *__restrict __s,
		  const char *__restrict __format, ...)
    __attribute__ ((__nothrow__, __leaf__));

/* For strict ISO C99 or POSIX compliance disallow %as, %aS and %a[
   GNU extension which conflicts with valid %a followed by letter
   s, S or [.  */
extern int fscanf(FILE * __restrict __stream,
		  const char *__restrict __format,
		  ...) __asm__("" "__isoc99_fscanf")
/* Ignore */ ;
extern int scanf(const char *__restrict __format,
		 ...) __asm__("" "__isoc99_scanf")
/* Ignore */ ;
extern int sscanf(const char *__restrict __s,
		  const char *__restrict __format,
		  ...) __asm__("" "__isoc99_sscanf")
    __attribute__ ((__nothrow__, __leaf__));



/* Read formatted input from S into argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vfscanf(FILE * __restrict __s, const char *__restrict __format,
		   __gnuc_va_list __arg)
    __attribute__ ((__format__(__scanf__, 2, 0)))
/* Ignore */ ;

/* Read formatted input from stdin into argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int vscanf(const char *__restrict __format, __gnuc_va_list __arg)
    __attribute__ ((__format__(__scanf__, 1, 0)))
/* Ignore */ ;


/*  Read formatted input from S into argument list ARG. */
extern int vsscanf(const char *__restrict __s,
		   const char *__restrict __format, __gnuc_va_list __arg)
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__format__(__scanf__, 2, 0)));

/* For strict ISO C99 or POSIX compliance disallow %as, %aS and %a[
   GNU extension which conflicts with valid %a followed by letter
   s, S or [.  */
extern int vfscanf(FILE * __restrict __s, const char *__restrict __format,
		   __gnuc_va_list __arg) __asm__("" "__isoc99_vfscanf")
    __attribute__ ((__format__(__scanf__, 2, 0)))
/* Ignore */ ;
extern int vscanf(const char *__restrict __format,
		  __gnuc_va_list __arg) __asm__("" "__isoc99_vscanf")
    __attribute__ ((__format__(__scanf__, 1, 0)))
/* Ignore */ ;
extern int vsscanf(const char *__restrict __s,
		   const char *__restrict __format,
		   __gnuc_va_list __arg) __asm__("" "__isoc99_vsscanf")
    __attribute__ ((__nothrow__, __leaf__))
    __attribute__ ((__format__(__scanf__, 2, 0)));



/* Read a character from STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern int fgetc(FILE * __stream);
extern int getc(FILE * __stream);

/* Read a character from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int getchar(void);


/* The C standard explicitly says this is a macro, so we always do the
   optimization for it.  */

/* These are defined in POSIX.1:1996.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern int getc_unlocked(FILE * __stream);
extern int getchar_unlocked(void);

/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fgetc_unlocked(FILE * __stream);


/* Write a character to STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.

   These functions is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fputc(int __c, FILE * __stream);
extern int putc(int __c, FILE * __stream);

/* Write a character to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int putchar(int __c);


/* The C standard explicitly says this can be a macro,
   so we always do the optimization for it.  */

/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern int fputc_unlocked(int __c, FILE * __stream);

/* These are defined in POSIX.1:1996.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */
extern int putc_unlocked(int __c, FILE * __stream);
extern int putchar_unlocked(int __c);


/*  Get a word (int) from STREAM. */
extern int getw(FILE * __stream);


/*  Write a word (int) to STREAM. */
extern int putw(int __w, FILE * __stream);


/* Get a newline-terminated string of finite length from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern char *fgets(char *__restrict __s, int __n,
		   FILE * __restrict __stream)

/* Ignore */ ;


/* Read up to (and including) a DELIMITER from STREAM into *LINEPTR
   (and null-terminate it). *LINEPTR is a pointer returned from malloc (or
   NULL), pointing to *N characters of space.  It is realloc'd as
   necessary.  Returns the number of characters read (not including the
   null terminator), or -1 on error or EOF.

   These functions are not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation they are cancellation points and
   therefore not marked with __THROW.  */
extern __ssize_t __getdelim(char **__restrict __lineptr,
			    size_t * __restrict __n, int __delimiter,
			    FILE * __restrict __stream)
/* Ignore */ ;
extern __ssize_t getdelim(char **__restrict __lineptr,
			  size_t * __restrict __n, int __delimiter,
			  FILE * __restrict __stream)
/* Ignore */ ;

/* Like `getdelim', but reads up to a newline.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */
extern __ssize_t getline(char **__restrict __lineptr,
			 size_t * __restrict __n,
			 FILE * __restrict __stream)
/* Ignore */ ;


/* Write a string to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fputs(const char *__restrict __s, FILE * __restrict __stream);

/* Write a string, followed by a newline, to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int puts(const char *__s);

/* Push a character back onto the input buffer of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int ungetc(int __c, FILE * __stream);

/* Read chunks of generic data from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern size_t fread(void *__restrict __ptr, size_t __size,
		    size_t __n, FILE * __restrict __stream)
/* Ignore */ ;

/* Write chunks of generic data to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern size_t fwrite(const void *__restrict __ptr, size_t __size,
		     size_t __n, FILE * __restrict __s);


/* Faster versions when locking is not necessary.

   These functions are not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation they are cancellation points and
   therefore not marked with __THROW.  */
extern size_t fread_unlocked(void *__restrict __ptr, size_t __size,
			     size_t __n, FILE * __restrict __stream)
/* Ignore */ ;
extern size_t fwrite_unlocked(const void *__restrict __ptr, size_t __size,
			      size_t __n, FILE * __restrict __stream);


/* Seek to a certain position on STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fseek(FILE * __stream, long int __off, int __whence);

/* Return the current position of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern long int ftell(FILE * __stream)
/* Ignore */ ;

/* Rewind to the beginning of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern void rewind(FILE * __stream);


/* The Single Unix Specification, Version 2, specifies an alternative,
   more adequate interface for the two functions above which deal with
   file offset.  `long int' is not the right type.  These definitions
   are originally defined in the Large File Support API.  */

/* Seek to a certain position on STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fseeko(FILE * __stream, __off_t __off, int __whence);

/* Return the current position of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern __off_t ftello(FILE * __stream)
/* Ignore */ ;


/* Get STREAM's position.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fgetpos(FILE * __restrict __stream, fpos_t * __restrict __pos);

/* Set STREAM's position.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int fsetpos(FILE * __stream, const fpos_t * __pos);




/*  Clear the error and EOF indicators for STREAM. */
extern void clearerr(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__));


/*  Return the EOF indicator for STREAM. */
extern int feof(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/*  Return the error indicator for STREAM. */
extern int ferror(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;



/*  Faster versions when locking is not required. */
extern void clearerr_unlocked(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__));
extern int feof_unlocked(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;
extern int ferror_unlocked(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/* Print a message describing the meaning of the value of errno.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern void perror(const char *__s);


/* Provide the declarations for `sys_errlist' and `sys_nerr' if they
   are available on this system.  Even if available, these variables
   should not be used directly.  The `strerror' function provides
   all the necessary functionality.  */

/* Declare sys_errlist and sys_nerr, or don't.  Compatibility (do) version.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  sys_errlist and sys_nerr are deprecated. Use strerror instead. */
extern int sys_nerr;
extern const char *const sys_errlist[];


/*  Return the system file descriptor for STREAM. */
extern int fileno(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/*  Faster version when locking is not required. */
extern int fileno_unlocked(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;

/* Create a new stream connected to a pipe running the given command.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern FILE *popen(const char *__command, const char *__modes)
/* Ignore */ ;

/* Close a stream opened by popen and return the status of its child.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */
extern int pclose(FILE * __stream);


/*  Return the name of the controlling terminal. */
extern char *ctermid(char *__s) __attribute__ ((__nothrow__, __leaf__));


/*  These are defined in POSIX.1:1996. */


/*  Acquire ownership of STREAM. */
extern void flockfile(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__));

/* Try to acquire ownership of STREAM but do not block if it is not
   possible.  */
extern int ftrylockfile(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__))
/* Ignore */ ;


/*  Relinquish the ownership granted for STREAM. */
extern void funlockfile(FILE * __stream)
    __attribute__ ((__nothrow__, __leaf__));

/* If we are compiling with optimizing read this file.  It contains
   several optimizing inline functions and macros.  */


/* Copyright (C) 2002-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/*  Macros. */


/*  ======= */


/*  Define a dummy __attribute__ macro for use on non-GNU compilers. */

/* Set to "1" (yes) or "0" (no) to indicate if AST was build with threads
   support. */

/* Value that indicates that two classes are not in direct line from each
   other. */

/*
*+
*  Name:
*     astINVOKE

*  Type:
*     Protected macro.

*  Purpose:
*     Invoke an AST function.

*  Synopsis:
*     #include "object.h"
*     astINVOKE(rettype,function)

*  Class Membership:
*     Defined by the Object class.

*  Description:
*     This macro expands to an invocation of an AST function, together
*     with any additional actions required to support it.  The actual
*     expansion depends on whether the macro is expanded in internal
*     code (astCLASS defined) or external code (astCLASS undefined)
*     and it therefore hides the differences between these two
*     interfaces.

*  Parameters:
*     rettype
*        A character to indicate the type of result returned by the function:
*
*        V
*           The function returns a value (including void or a pointer
*           value, but excluding an Object pointer). astINVOKE will
*           return the value unchanged.
*        O
*           The function returns an Object pointer. astINVOKE will
*           convert it to an Object identifier if necessary.
*        F
*           The function returns a function pointer. astINVOKE will
*           return it unchanged. This is typically used when the
*           function has a variable argument list. In this case the
*           function name is passed to astINVOKE without its argument
*           list and a pointer to the function is returned which can
*           then be supplied with an argument list. This avoids the
*           need to define a macro with a variable number of arguments
*           (which isn't allowed).
*     function
*        A normal invocation of the function returning the required
*        result.  In the case of a variable argument list, the
*        argument list should be omitted so that the function is not
*        invoked but a function pointer is returned that may then be
*        used to invoke it.

*  Examples:
*     #define astGetNobject(this) *             astINVOKE(V,astGetNobject_(astCheckObject(this)))
)
*        Defines a macro to invoke the astGetNobject_ function which
*        returns an int.
*     #define astClone(this) *             astINVOKE(O,astClone_(astCheckObject(this)))
)
*        Defines a macro to invoke the astClone_ function which
*        returns an Object pointer.
*     #define astSet astINVOKE(F,astSet_)
*        Defines a macro to invoke the astSet_ function which has a
*        variable argument list and returns void. The macro result is
*        a pointer to the astSet_ function. This function must perform
*        its own argument validation, as (e.g) astCheckObject cannot
*        be invoked on its arguments via a macro.

*  Notes:
*     - To avoid problems with some compilers, you should not leave
*     any white space around the macro arguments.
*-
*/

/* Define astINVOKE, which records the current file and line number
   (in case of error) using astAt, and then invokes the function
   supplied as an argument of the astRetV_, astRetO_ or astRetF_
   macro.

   Suppress reporting of the file and line number from internal code
   and from foreign language interfaces by not using astAt in these
   cases. */


/*  astRetF_ and astRetV_ currently do nothing. */


/*  However, astRetO_ converts a pointer to an ID if necessary. */

/*
*+
*  Name:
*     astINVOKE_CHECK
*     astINVOKE_ISA

*  Type:
*     Protected macros.

*  Purpose:
*     Invoke the astCheck<Class>_ and astIsA<Class>_ functions.

*  Synopsis:
*     #include "object.h"
*     astINVOKE_CHECK(class,this,force)
*     astINVOKE_ISA(class,this)

*  Class Membership:
*     Defined by the Object class.

*  Description:
*     These macros expand to invocations of the standard
*     astCheck<Class>_ and astIsA<Class>_ functions for a class.

*  Parameters:
*     class
*        The name (not the type) of the class for which the function
*        is to be invoked.
*     this
*        The "this" argument (the Object pointer) to be passed to the
*        function.
*     force
*        Type checking takes time, and so can be disabled within the
*        protected context in order to save time. Setting "force" to
*        zero causes the astINVOKE_CHECK macro to skip the class check
*        in a protected context (it assumes that AST "knows what it is
*        doing"). Setting "force" to a non-zero value forces the class
*        check even in a protected context.

*  Notes:
*     - To avoid problems with some compilers, you should not leave
*     any white space around the macro arguments.
*-
*/

/* For the public interface (and also internally if AST_CHECK_CLASS is
   defined), define astINVOKE_CHECK to invoke the astCheck<Class>
   function. */

/* For the internal interface, astINVOKE_CHECK omits the
   astCheck<class> function (unless AST_CHECK_CLASS is defined). */


/*  Define astINVOKE_ISA to invoke the astIsA<Class> function. */

/* The astEnsurePointer_ macro ensures a true C pointer, converting
   from an ID if necessary. */

/*
*+
*  Name:
*     astPROTO_CHECK
*     astPROTO_ISA

*  Type:
*     Protected macros.

*  Purpose:
*     Prototype the astCheck<Class>_ and astIsA<Class>_ functions.

*  Synopsis:
*     #include "object.h"
*     astPROTO_CHECK(class)
*     astPROTO_ISA(class)

*  Class Membership:
*     Defined by the Object class.

*  Description:
*     These macros expands to function prototypes for the
*     astCheck<Class>_ and astIsA<Class>_ functions (q.v.) which
*     validate and test for membership of a specified class.

*  Parameters:
*     class
*        The name (not the type) of the class whose membership is to
*        be validated.

*  Notes:
*     - To avoid problems with some compilers, you should not leave
*     any white space around the macro arguments.
*-
*/


/*  Define the macros. */


/*  Macros which return the maximum and minimum of two values. */

/* Check for equality of floating point values. We cannot compare bad values
   directly because of the danger of floating point exceptions, so bad
   values are dealt with explicitly. */


/*  AST__NULL. */


/*  ---------- */

/* Define the AST__NULL macro, which evaluates to a null Object
   pointer. */


/*  Type Definitions. */


/*  ================= */


/*  Object structure. */


/*  ----------------- */

/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstObject {


/*  Attributes specific to objects in this class. */
    unsigned long check;

/*  Check value to identify Objects */
    size_t size;

/*  Amount of memory used by Object */
    struct AstObjectVtab *vtab;

/*  Pointer to virtual function table */
    char dynamic;

/*  Memory allocated dynamically? */
    int ref_count;

/*  Number of active pointers to the Object */
    char *id;

/*  Pointer to ID string */
    char *ident;

/*  Pointer to Ident string */
    char usedefs;

/*  Use default attribute values? */
    int iref;

/*  Object index (unique within class) */
    void *proxy;
/* A pointer to an external object that
				   acts as a foreign language proxy for the
				   AST object */
    int locker;

/*  Thread that has locked this Object */
    pthread_mutex_t mutex1;
/* Guards access to all elements of the
				   Object except for the "locker" and
				   "ref_count" components */
    pthread_mutex_t mutex2;
/* Guards access to the "locker" and
				   "ref_count" components */
    struct AstGlobals *globals;

/*  Pointer to thread-specific global data */
} AstObject;


/*  Class identifier structure */
typedef struct AstClassIdentifier {
    int *check;
    struct AstClassIdentifier *parent;
} AstClassIdentifier;


/*  Virtual function table. */


/*  ----------------------- */

/* The virtual function table makes a forward reference to the
   AstChannel structure which is not defined until "channel.h" is
   included (below). Hence make a preliminary definition available
   now. */
struct AstChannel;

/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */


/*  More include files. */


/*  =================== */

/* The interface to the Channel class must be included here (after the
   type definitions for the Object class) because "channel.h" itself
   includes this file ("object.h"), although "object.h" refers to the
   AstChannel structure above. This seems a little strange at first,
   but is simply analogous to making a forward reference to a
   structure type when recursively defining a normal C structure
   (except that here the definitions happen to be in separate include
   files). */

/*
*+
*  Name:
*     channel.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Channel class.

*  Invocation:
*     #include "channel.h"

*  Description:
*     This include file defines the interface to the Channel class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.
*
*     A Channel is the basic form of AST I/O channel, through which
*     Objects may be written and later read back. It causes I/O to
*     take place using a textual format via standard input and
*     standard output.
*
*     Writing to a Channel will result in a textual representation of
*     an Object being produced on standard output. Reading from a
*     Channel will causes a textual description of an Object to be
*     read from standard input, and that Object to be
*     re-created. Channel I/O is stream based, and multiple objects
*     may be written or read in succession through the same Channel. A
*     null Object pointer is returned if there is no more input to
*     read.

*  Inheritance:
*     The Channel class inherits from the Object class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Comment (integer)
*        A boolean value (0 or 1) which controls whether comments are
*        to be included in textual output generated by a Channel. If
*        this value is non-zero (the default), then comments will be
*        included. If it is zero, comments will be omitted.
*     Full (integer)
*        A three-state flag (taking values -1, 0 or +1) which controls
*        the amount of information included in textual output
*        generated by a Channel. If this value is zero (the default),
*        then a modest amount of non-essential but useful information
*        will be included along with the output. If Full is negative,
*        all non-essential information will be suppressed, while if it
*        is positive, the output will include the maximum amount of
*        information about the Object being written.
*     Skip (integer)
*        A boolean value which indicates whether the Objects being
*        read through a Channel are inter-mixed with other external
*        data. If this value is zero (the default), then the source of
*        input data is expected to contain descriptions of AST Objects
*        and comments and nothing else (if anything else is read, an
*        error will result). If Skip is non-zero, then any non-Object
*        data encountered between Objects will simply be skipped over
*        in order to reach the next Object.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Mapping.
*        astGetAttrib
*           Get an attribute value for a Mapping.
*        astSetAttrib
*           Set an attribute value for a Mapping.
*        astTestAttrib
*           Test if an attribute value has been set for a Mapping.

*  New Methods Defined:
*     Public:
*        astRead
*           Read an Object from a Channel.
*        astWrite
*           Write an Object to a Channel.
*
*     Protected:
*        astClearComment
*           Clear the Comment attribute for a Channel.
*        astClearFull
*           Clear the Full attribute for a Channel.
*        astClearSkip
*           Clear the Skip attribute for a Channel.
*        astGetComment
*           Get the value of the Comment attribute for a Channel.
*        astGetFull
*           Get the value of the Full attribute for a Channel.
*        astGetNextData
*           Read the next item of data from a data source.
*        astGetNextText
*           Read the next line of input text from a data source.
*        astGetSkip
*           Get the value of the Skip attribute for a Channel.
*        astPutNextText
*           Write a line of output text to a data sink.
*        astReadClassData
*           Read values from a data source for a class loader.
*        astReadDouble
*           Read a double value as part of loading a class.
*        astReadInt
*           Read an int value as part of loading a class.
*        astReadObject
*           Read a (sub)Object as part of loading a class.
*        astReadString
*           Read a string value as part of loading a class.
*        astSetComment
*           Set the value of the Comment attribute for a Channel.
*        astSetFull
*           Set the value of the Full attribute for a Channel.
*        astSetSkip
*           Set the value of the Skip attribute for a Channel.
*        astTestComment
*           Test whether a value has been set for the Comment attribute of a
*           Channel.
*        astTestFull
*           Test whether a value has been set for the Full attribute of a
*           Channel.
*        astTestSkip
*           Test whether a value has been set for the Skip attribute of a
*           Channel.
*        astWriteBegin
*           Write a "Begin" data item to a data sink.
*        astWriteDouble
*           Write a double value to a data sink.
*        astWriteEnd
*           Write an "End" data item to a data sink.
*        astWriteInt
*           Write an integer value to a data sink.
*        astWriteIsA
*           Write an "IsA" data item to a data sink.
*        astWriteObject
*           Write an Object as a value to a data sink.
*        astWriteString
*           Write a string value to a data sink.

*  Other Class Functions:
*     Public:
*        astChannel
*           Create a Channel.
*        astChannelFor
*           Create a Channel from a foreign language interface.
*        astIsAChannel
*           Test class membership.
*
*     Protected:
*        astCheckChannel
*           Validate class membership.
*        astInitChannel
*           Initialise a Channel.
*        astInitChannelVtab
*           Initialise the virtual function table for the Channel class.
*        astLoadChannel
*           Load a Channel.

*  Type Definitions:
*     Public:
*        AstChannel
*           Channel object type.
*
*     Protected:
*        AstChannelVtab
*           Channel virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     12-AUG-1996 (RFWS):
*        Original version.
*     12-DEC-1996 (RFWS):
*        Added the astChannelFor function.
*     11-NOV-2002 (DSB):
*        Added astWriteInvocations.
*     8-JAN-2003 (DSB):
*        Added protected astInitAxisVtab method.
*-
*/


/*  Include files. */


/*  ============== */


/*  Interface definitions. */


/*  ---------------------- */

/* Note that the usual setting of the CHANNEL_INCLUDED flag, which
   prevents this file being included more than once, must be deferred
   until after including the "object.h" file. This is because
   "object.h" needs to include the present interface definition (as a
   form of "forward reference") in order to have access to I/O
   Channels itself. */


/*  C header files. */


/*  --------------- */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */


/*  Macros */


/*  ====== */


/*  Define constants used to size global arrays in this module. */


/*  Define a dummy __attribute__ macro for use on non-GNU compilers. */


/*  Type Definitions. */


/*  ================= */

/* The astWarnings function returns a KeyMap pointer, but we cannot
   include keymap.h here to define the AstKeyMap type since keymap.h
   itself include sthis file. So we make a temporary declaration which
   will be replaced by the full one when the keymap.h file is included. */
struct AstKeyMap;


/*  Channel structure. */


/*  ------------------ */

/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstChannel {


/*  Attributes inherited from the parent class. */
    AstObject object;

/*  Parent class structure */


/*  Attributes specific to objects in this class. */
    const char *(*source) (void);

/*  Pointer to source function */
    char *(*source_wrap) (const char *(*)(void), int *);


/*  Source wrapper function pointer */
    void (*sink) (const char *);

/*  Pointer to sink function */
    void (*sink_wrap) (void (*)(const char *), const char *, int *);


/*  Sink wrapper function pointer */
    int comment;

/*  Output comments? */
    int full;

/*  Set max/normal/min information level */
    int skip;

/*  Skip data between Objects? */
    int indent;

/*  Indentation increment in astWrite output */
    int report_level;

/*  Skip data between Objects? */
    int strict;

/*  Report unexpected data items? */
    void *data;

/*  Data to pass to source/sink functions */
    char **warnings;

/*  Array of warning strings */
    int nwarn;

/*  Size of warnings array */
    FILE *fd_in;

/*  Descriptor for source text file */
    char *fn_in;

/*  Full path for source text file */
    FILE *fd_out;

/*  Descriptor for sink text file */
    char *fn_out;

/*  Full path for sink text file */
} AstChannel;


/*  Virtual function table. */


/*  ----------------------- */

/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */


/*  Function prototypes. */


/*  ==================== */


/*  Prototypes for standard class functions. */


/*  ---------------------------------------- */
AstChannel *astCheckChannel_(AstChannel *, int *);

/*  Check class membership */
int astIsAChannel_(const AstChannel *, int *);

/*  Test class membership */


/*  Constructor. */
AstChannel *astChannelId_(const char *(*)(void), void (*)(const char *),
			  const char *, ...)
    __attribute__ ((format(printf, 3, 4)));
AstChannel *astChannelForId_(const char *(*)(void),
			     char *(*)(const char *(*)(void), int *),
			     void (*)(const char *),
			     void (*)(void (*)(const char *), const char *,
				      int *), const char *, ...);


/*  Prototypes for member functions. */


/*  -------------------------------- */
AstObject *astRead_(AstChannel *, int *);
int astWrite_(AstChannel *, AstObject *, int *);
void astPutChannelData_(AstChannel *, void *, int *);
void *astChannelData_(void);
struct AstKeyMap *astWarnings_(AstChannel *, int *);
char *astSourceWrap_(const char *(*)(void), int *);
void astSinkWrap_(void (*)(const char *), const char *, int *);


/*  Function interfaces. */


/*  ==================== */

/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */


/*  Interfaces to standard class functions. */


/*  --------------------------------------- */

/* Some of these functions provide validation, so we cannot use them to
   validate their own arguments. We must use a cast when passing object
   pointers (so that they can accept objects from derived classes). */


/*  Check class membership. */


/*  Test class membership. */


/*  Constructor. */


/*  Interfaces to member functions. */


/*  ------------------------------- */

/* Here we make use of astCheckChannel to validate Channel pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */


/*  Function prototypes. */


/*  ==================== */


/*  Prototypes for standard class functions. */


/*  ---------------------------------------- */
AstObject *astCheckObject_(AstObject *, int *);

/*  Validate class membership */
int astIsAObject_(const AstObject *, int *);

/*  Test class membership */


/*  NB. There is no constructor function for this class. */


/*  Prototypes for other class functions. */


/*  ------------------------------------- */
void astBegin_(void);
void astEnd_(int *);
AstObject *astI2P_(int, int *);
AstObject *astMakeId_(AstObject *, int *);
AstObject *astMakePointer_(AstObject *, int *);
AstObject *astMakePointer_NoLockCheck_(AstObject *, int *);
int astP2I_(AstObject *, int *);
int astVersion_(int *);
int astEscapes_(int, int *);
int astTune_(const char *, int, int *);
void astTuneC_(const char *, const char *, char *, int, int *);


/*  Prototypes for member functions. */


/*  -------------------------------- */
AstObject *astDeleteId_(AstObject *, int *);
int astThreadId_(AstObject *, int, int *);
void astExportId_(AstObject *, int *);
void astImportId_(AstObject *, int *);
void astSetId_(void *, const char *, ...)
    __attribute__ ((format(printf, 2, 3)));
AstObject *astAnnulId_(AstObject *, int *);
AstObject *astCheckLock_(AstObject *, int *);
AstObject *astClone_(AstObject *, int *);
AstObject *astCopy_(const AstObject *, int *);
AstObject *astFromString_(const char *, int *);
char *astToString_(AstObject *, int *);
const char *astGetC_(AstObject *, const char *, int *);
double astGetD_(AstObject *, const char *, int *);
float astGetF_(AstObject *, const char *, int *);
int astEqual_(AstObject *, AstObject *, int *);
int astGetI_(AstObject *, const char *, int *);
int astHasAttribute_(AstObject *, const char *, int *);
int astSame_(AstObject *, AstObject *, int *);
int astTest_(AstObject *, const char *, int *);
long astGetL_(AstObject *, const char *, int *);
void *astGetProxy_(AstObject *, int *);
void astClear_(AstObject *, const char *, int *);
void astExemptId_(AstObject *, int *);
void astLockId_(AstObject *, int, int *);
void astSetC_(AstObject *, const char *, const char *, int *);
void astSetD_(AstObject *, const char *, double, int *);
void astSetF_(AstObject *, const char *, float, int *);
void astSetI_(AstObject *, const char *, int, int *);
void astSetL_(AstObject *, const char *, long, int *);
void astSetProxy_(AstObject *, void *, int *);
void astShow_(AstObject *, int *);
void astUnlockId_(AstObject *, int, int *);


/*  Function interfaces. */


/*  ==================== */

/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */


/*  Interfaces to standard class functions. */


/*  --------------------------------------- */


/*  Check class membership. */


/*  Test class membership. */


/*  NB. There is no constructor function for this class. */


/*  Interfaces to other class functions. */


/*  ------------------------------------ */


/*  Interfaces to member functions. */


/*  ------------------------------- */

/* Here we make use of astCheckObject (et al.) to validate Object
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. In the case of an
   external caller, it also performs the required conversion from an
   Object identifier to a true C pointer. */

/* These functions require special treatment for external use because
   they handle Object identifiers and their resources explicitly, and
   must therefore be passed identifier values without conversion to C
   pointers. */


/*  Both.... */


/*  Extra stuff for debuging probnlems with object handles and memory
    usage */

/*
*+
*  Name:
*     pointset.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the PointSet class.

*  Invocation:
*     #include "pointset.h"

*  Description:
*     This include file defines the interface to the PointSet class
*     and provides the type definitions, function prototypes and
*     macros, etc.  needed to use this class.
*
*     The PointSet class encapsulates sets of coordinate values
*     representing points in an N-dimensional space, to which
*     coordinate transformations may be applied. It also provides
*     memory allocation facilities for coordinate values.

*  Inheritance:
*     The PointSet class inherits from the Object class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Ncoord (integer)
*        A read-only attribute that gives the number of coordinates
*        for each point in a PointSet (i.e. the number of dimensions
*        of the space in which the points reside). This value is
*        determined when the PointSet is created.
*     Npoint (integer)
*        A read-only attribute that gives the number of points that
*        can be stored in the PointSet. This value is determined when
*        the PointSet is created.
*     PointAccuracy (floating point)
*        This stores the absolute accuracies for each axis in the PointSet.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        ClearAttrib
*           Clear an attribute value for a PointSet.
*        GetAttrib
*           Get an attribute value for a PointSet.
*        SetAttrib
*           Set an attribute value for a PointSet.
*        TestAttrib
*           Test if an attribute value has been set for a PointSet.

*  New Methods Defined:
*     Public:
*        astAppendPoints
*           Append one PointSet to another.
*        astBndPoints
*           Find the axis bounds of the points in a PointSet.
*        astGetPoints
*           Get a pointer to the coordinate values associated with a PointSet.
*        astPermPoints
*           Permute coordinates within a PointSet.
*        astSetPoints
*           Associate coordinate values with a PointSet.
*        astSetNpoint
*           Reduce the size of a PointSet.
*        astSetSubPoints
*           Associate one PointSet with a subset of another.
*
*     Protected:
*        astGetNpoint
*           Get the number of points in a PointSet.
*        astGetNcoord
*           Get the number of coordinate values per point from a PointSet.
*        astGetPointAccuracy
*           Get the curent value of the PointAcuracy attribute for an axis.
*        astSetPointAccuracy
*           Set a new value for the PointAcuracy attribute for an axis.
*        astTestPointAccuracy
*           Test the value of the PointAcuracy attribute for an axis.
*        astClearPointAccuracy
*           Clear the value of the PointAcuracy attribute for an axis.

*  Other Class Functions:
*     Public:
*        astIsAPointSet
*           Test class membership.
*        astPointSet
*           Create a PointSet.
*
*     Protected:
*        astCheckPointSet
*           Validate class membership.
*        astInitPointSet
*           Initialise a PointSet.
*        astInitPointSetVtab
*           Initialise the virtual function table for the PointSet class.
*        astLoadPointSet
*           Load a PointSet.

*  Macros:
*     Public:
*        AST__BAD
*           Bad value flag for coordinate data.
*
*     Protected:
*        astISBAD
*           Check if a value is AST__BAD or NaN.
*        astISGOOD
*           Check if a value is not AST__BAD or NaN.
*        astISNAN
*           Check if a value is NaN.

*  Type Definitions:
*     Public:
*        AstPointSet
*           PointSet object type.
*
*     Protected:
*        AstPointSetVtab
*           PointSet virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     30-JAN-1996 (RFWS):
*        Original version.
*     27-SEP-1996 (RFWS):
*        Added external interface and I/O facilities.
*     8-JAN-2003 (DSB):
*        Added protected astInitPointSetVtab method.
*     2-NOV-2004 (DSB):
*        Added PointAccuracy attribute.
*-
*/


/*  Include files. */


/*  ============== */


/*  Configuration results. */


/*  ---------------------- */


/*  config.h. Generated from config.h.in by configure. */


/*  config.h.in. Generated from configure.ac by autoheader. */


/*  use external PAL and ERFA libraries */


/*  #undef EXTERNAL_PAL */


/*  Define to 1 if you have the `backtrace' function. */

/* Define to 1 if you have the declaration of `isfinite', and to 0 if you
   don't. */

/* Define to 1 if you have the declaration of `isnan', and to 0 if you don't.
   */


/*  Define to 1 if you have the <dlfcn.h> header file. */


/*  Define to 1 if you have the <execinfo.h> header file. */


/*  Define to 1 if the system has the type `int64_t'. */


/*  Define to 1 if you have the <inttypes.h> header file. */


/*  Define to 1 if you have the `isfinite' function. */


/*  #undef HAVE_ISFINITE */


/*  Define to 1 if you have the `isnan' function. */


/*  Define to 1 if you have the `pthread' library (-lpthread). */


/*  Define to 1 if the system has the type `long double'. */


/*  Define to 1 if you have the <memory.h> header file. */


/*  The sscanf shows the non-ANSI behaviour reported by Bill Joye */


/*  #undef HAVE_NONANSI_SSCANF */


/*  Define to 1 if the Fortran compiler supports the VAX %LOC extension */


/*  #undef HAVE_PERCENTLOC */


/*  Use the starmem library for memory management */


/*  #undef HAVE_STAR_MEM_H */


/*  Define to 1 if you have the <stdarg.h> header file. */


/*  Define to 1 if you have the <stdint.h> header file. */


/*  Define to 1 if you have the <stdlib.h> header file. */


/*  Define to 1 if you have the `strerror_r' function. */


/*  Define to 1 if you have the <strings.h> header file. */


/*  Define to 1 if you have the <string.h> header file. */


/*  Define to 1 if you have the `strtok_r' function. */


/*  Define to 1 if you have the <sys/stat.h> header file. */


/*  Define to 1 if you have the <sys/types.h> header file. */


/*  Define to 1 if the system has the type `uint64_t'. */


/*  Define to 1 if you have the <unistd.h> header file. */


/*  Define to 1 if you have the <varargs.h> header file. */


/*  #undef HAVE_VARARGS_H */


/*  Define to 1 if you have the `vsnprintf' function. */

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */


/*  enable AST memory leak debugging functions in memory.c */


/*  #undef MEM_DEBUG */


/*  Name of package */


/*  Define to the address where bug reports for this package should be
    sent. */


/*  Define to the full name of this package. */


/*  Define to the full name and version of this package. */


/*  Define to the one symbol short name of this package. */


/*  Define to the home page for this package. */


/*  Define to the version of this package. */


/*  The size of `long', as computed by sizeof. */


/*  The size of `long long', as computed by sizeof. */


/*  The size of `void*', as computed by sizeof. */


/*  Define to 1 if you have the ANSI C header files. */


/*  Type of Fortran CNF TRAIL argument */


/*  Version number of package */


/*  Interface definitions. */


/*  ---------------------- */


/*  C header files. */


/*  --------------- */

/* Copyright (C) 2002-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  5.2.4.2.2  Characteristics of floating types <float.h>
 */


/*  Macros. */


/*  ======= */

/*
*+
*  Name:
*     AST__BAD

*  Type:
*     Public macro.

*  Purpose:
*     Bad value flag for coordinate data.

*  Synopsis:
*     #include "pointset.h"
*     const double AST__BAD

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to a const double value that is used to flag
*     coordinate values that are "bad" (i.e. undefined or
*     meaningless). Classes that implement coordinate transformations
*     should test coordinate values against this value, and
*     appropriately propagate bad values to their output.
*-
*/

/* Define AST__BAD to be the most negative (normalised) double
   value. */

/*
*+
*  Name:
*     AST__NAN

*  Type:
*     Public macro.

*  Purpose:
*     A value representing the double precision IEEE NaN value.

*  Synopsis:
*     #include "pointset.h"
*     const double AST__NAN

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to a const double value that is used to indicate
*     that a IEEE NaN value should be used. Note, AST__NAN itself is a finite
*     double precision floating point value a little below the maximum
*     allowed value for a double. This value can be used as flag to
*     indicate that the corresponding IEEE NaN value should be used in its
*     place.

*-
*/

/*
*+
*  Name:
*     AST__NANF

*  Type:
*     Public macro.

*  Purpose:
*     A value representing the single precision IEEE NaN value.

*  Synopsis:
*     #include "pointset.h"
*     const double AST__NANF

*  Class Membership:
*     Defined by the PointSet class.

*  Description:
*     This macro expands to a const float value that is used to indicate
*     that a IEEE NaN value should be used. Note, AST__NANF itself is a finite
*     single precision floating point value a little below the maximum
*     allowed value for a float. This value can be used as flag to
*     indicate that the corresponding IEEE NaN value should be used in its
*     place.

*-
*/


/*  Define a dummy __attribute__ macro for use on non-GNU compilers. */


/*  Type Definitions. */


/*  ================= */


/*  PointSet structure. */


/*  ------------------- */

/* This structure contains all information that is unique to each object in
   the class (e.g. its instance variables). */
typedef struct AstPointSet {


/*  Attributes inherited from the parent class. */
    AstObject object;

/*  Parent class structure */


/*  Attributes specific to objects in this class. */
    double **ptr;

/*  Pointer to array of pointers to values */
    double *values;

/*  Pointer to array of coordinate values */
    int ncoord;

/*  Number of coordinate values per point */
    int npoint;

/*  Number of points */
    double *acc;

/*  Axis accuracies */
} AstPointSet;


/*  Virtual function table. */


/*  ----------------------- */

/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */


/*  Function prototypes. */


/*  ==================== */


/*  Prototypes for standard class functions. */


/*  ---------------------------------------- */
AstPointSet *astCheckPointSet_(AstPointSet *, int *);

/*  Check class membership */
int astIsAPointSet_(const AstPointSet *, int *);

/*  Test class membership */


/*  Constructor. */
AstPointSet *astPointSetId_(int, int, const char *, ...)
    __attribute__ ((format(printf, 3, 4)));


/*  Prototypes for member functions. */


/*  -------------------------------- */
double **astGetPoints_(AstPointSet *, int *);
void astPermPoints_(AstPointSet *, int, const int[], int *);
void astSetPoints_(AstPointSet *, double **, int *);
void astSetNpoint_(AstPointSet *, int, int *);
void astSetSubPoints_(AstPointSet *, int, int, AstPointSet *, int *);
AstPointSet *astAppendPoints_(AstPointSet *, AstPointSet *, int *);
void astBndPoints_(AstPointSet *, double *, double *, int *);
int astReplaceNaN_(AstPointSet *, int *);


/*  Function interfaces. */


/*  ==================== */

/* These macros are wrap-ups for the functions defined by this class
   to make them easier to invoke (e.g. to avoid type mis-matches when
   passing pointers to objects from derived classes). */


/*  Interfaces to standard class functions. */


/*  --------------------------------------- */

/* Some of these functions provide validation, so we cannot use them
   to validate their own arguments. We must use a cast when passing
   object pointers (so that they can accept objects from derived
   classes). */


/*  Check class membership. */


/*  Test class membership. */


/*  Constructor. */


/*  Interfaces to public member functions. */


/*  -------------------------------------- */

/* Here we make use of astCheckPointSet to validate PointSet pointers
   before use.  This provides a contextual error report if a pointer
   to the wrong sort of Object is supplied. */

/*
*+
*  Name:
*     channel.h

*  Type:
*     C include file.

*  Purpose:
*     Define the interface to the Channel class.

*  Invocation:
*     #include "channel.h"

*  Description:
*     This include file defines the interface to the Channel class and
*     provides the type definitions, function prototypes and macros,
*     etc. needed to use this class.
*
*     A Channel is the basic form of AST I/O channel, through which
*     Objects may be written and later read back. It causes I/O to
*     take place using a textual format via standard input and
*     standard output.
*
*     Writing to a Channel will result in a textual representation of
*     an Object being produced on standard output. Reading from a
*     Channel will causes a textual description of an Object to be
*     read from standard input, and that Object to be
*     re-created. Channel I/O is stream based, and multiple objects
*     may be written or read in succession through the same Channel. A
*     null Object pointer is returned if there is no more input to
*     read.

*  Inheritance:
*     The Channel class inherits from the Object class.

*  Attributes Over-Ridden:
*     None.

*  New Attributes Defined:
*     Comment (integer)
*        A boolean value (0 or 1) which controls whether comments are
*        to be included in textual output generated by a Channel. If
*        this value is non-zero (the default), then comments will be
*        included. If it is zero, comments will be omitted.
*     Full (integer)
*        A three-state flag (taking values -1, 0 or +1) which controls
*        the amount of information included in textual output
*        generated by a Channel. If this value is zero (the default),
*        then a modest amount of non-essential but useful information
*        will be included along with the output. If Full is negative,
*        all non-essential information will be suppressed, while if it
*        is positive, the output will include the maximum amount of
*        information about the Object being written.
*     Skip (integer)
*        A boolean value which indicates whether the Objects being
*        read through a Channel are inter-mixed with other external
*        data. If this value is zero (the default), then the source of
*        input data is expected to contain descriptions of AST Objects
*        and comments and nothing else (if anything else is read, an
*        error will result). If Skip is non-zero, then any non-Object
*        data encountered between Objects will simply be skipped over
*        in order to reach the next Object.

*  Methods Over-Ridden:
*     Public:
*        None.
*
*     Protected:
*        astClearAttrib
*           Clear an attribute value for a Mapping.
*        astGetAttrib
*           Get an attribute value for a Mapping.
*        astSetAttrib
*           Set an attribute value for a Mapping.
*        astTestAttrib
*           Test if an attribute value has been set for a Mapping.

*  New Methods Defined:
*     Public:
*        astRead
*           Read an Object from a Channel.
*        astWrite
*           Write an Object to a Channel.
*
*     Protected:
*        astClearComment
*           Clear the Comment attribute for a Channel.
*        astClearFull
*           Clear the Full attribute for a Channel.
*        astClearSkip
*           Clear the Skip attribute for a Channel.
*        astGetComment
*           Get the value of the Comment attribute for a Channel.
*        astGetFull
*           Get the value of the Full attribute for a Channel.
*        astGetNextData
*           Read the next item of data from a data source.
*        astGetNextText
*           Read the next line of input text from a data source.
*        astGetSkip
*           Get the value of the Skip attribute for a Channel.
*        astPutNextText
*           Write a line of output text to a data sink.
*        astReadClassData
*           Read values from a data source for a class loader.
*        astReadDouble
*           Read a double value as part of loading a class.
*        astReadInt
*           Read an int value as part of loading a class.
*        astReadObject
*           Read a (sub)Object as part of loading a class.
*        astReadString
*           Read a string value as part of loading a class.
*        astSetComment
*           Set the value of the Comment attribute for a Channel.
*        astSetFull
*           Set the value of the Full attribute for a Channel.
*        astSetSkip
*           Set the value of the Skip attribute for a Channel.
*        astTestComment
*           Test whether a value has been set for the Comment attribute of a
*           Channel.
*        astTestFull
*           Test whether a value has been set for the Full attribute of a
*           Channel.
*        astTestSkip
*           Test whether a value has been set for the Skip attribute of a
*           Channel.
*        astWriteBegin
*           Write a "Begin" data item to a data sink.
*        astWriteDouble
*           Write a double value to a data sink.
*        astWriteEnd
*           Write an "End" data item to a data sink.
*        astWriteInt
*           Write an integer value to a data sink.
*        astWriteIsA
*           Write an "IsA" data item to a data sink.
*        astWriteObject
*           Write an Object as a value to a data sink.
*        astWriteString
*           Write a string value to a data sink.

*  Other Class Functions:
*     Public:
*        astChannel
*           Create a Channel.
*        astChannelFor
*           Create a Channel from a foreign language interface.
*        astIsAChannel
*           Test class membership.
*
*     Protected:
*        astCheckChannel
*           Validate class membership.
*        astInitChannel
*           Initialise a Channel.
*        astInitChannelVtab
*           Initialise the virtual function table for the Channel class.
*        astLoadChannel
*           Load a Channel.

*  Type Definitions:
*     Public:
*        AstChannel
*           Channel object type.
*
*     Protected:
*        AstChannelVtab
*           Channel virtual function table type.

*  Feature Test Macros:
*     astCLASS
*        If the astCLASS macro is undefined, only public symbols are
*        made available, otherwise protected symbols (for use in other
*        class implementations) are defined. This macro also affects
*        the reporting of error context information, which is only
*        provided for external calls to the AST library.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     12-AUG-1996 (RFWS):
*        Original version.
*     12-DEC-1996 (RFWS):
*        Added the astChannelFor function.
*     11-NOV-2002 (DSB):
*        Added astWriteInvocations.
*     8-JAN-2003 (DSB):
*        Added protected astInitAxisVtab method.
*-
*/


/*  Include files. */


/*  ============== */


/*  Interface definitions. */


/*  ---------------------- */

/* Note that the usual setting of the CHANNEL_INCLUDED flag, which
   prevents this file being included more than once, must be deferred
   until after including the "object.h" file. This is because
   "object.h" needs to include the present interface definition (as a
   form of "forward reference") in order to have access to I/O
   Channels itself. */


/*  C header files. */


/*  --------------- */

/* Copyright (C) 1989-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */

/* Copyright (C) 1997-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99: 7.18 Integer types <stdint.h>
 */

/* Copyright (C) 1991-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* wchar_t type related definitions.
   Copyright (C) 2000-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* The fallback definitions, for when __WCHAR_MAX__ or __WCHAR_MIN__
   are not defined, give the right value and type as long as both int
   and wchar_t are 32-bit types.  Adding L'\0' to a constant value
   ensures that the type is correct; it is necessary to use (L'\0' +
   0) rather than just L'\0' so that the type in C++ is the promoted
   version of wchar_t rather than the distinct wchar_t type itself.
   Because wchar_t in preprocessor #if expressions is treated as
   intmax_t or uintmax_t, the expression (L'\0' - 1) would have the
   wrong value for WCHAR_MAX in such expressions and so cannot be used
   to define __WCHAR_MAX in the unsigned case.  */


/*  Determine the wordsize from the preprocessor defines. */


/*  Both x86-64 and x32 use the 64-bit system call interface. */


/*  Exact integral types. */


/*  Signed. */


/*  There is some amount of overlap with <sys/types.h> as known by inet
    code */


/*  Unsigned. */
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;


/*  Small types. */


/*  Signed. */
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;
typedef long int int_least64_t;


/*  Unsigned. */
typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;
typedef unsigned long int uint_least64_t;


/*  Fast types. */


/*  Signed. */
typedef signed char int_fast8_t;
typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;


/*  Unsigned. */
typedef unsigned char uint_fast8_t;
typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;


/*  Types for `void *' pointers. */
typedef long int intptr_t;
typedef unsigned long int uintptr_t;


/*  Largest integral types. */
typedef long int intmax_t;
typedef unsigned long int uintmax_t;


/*  Limits of integral types. */


/*  Minimum of signed integral types. */


/*  Maximum of signed integral types. */


/*  Maximum of unsigned integral types. */


/*  Minimum of signed integral types having a minimum size. */


/*  Maximum of signed integral types having a minimum size. */


/*  Maximum of unsigned integral types having a minimum size. */


/*  Minimum of fast signed integral types having a minimum size. */


/*  Maximum of fast signed integral types having a minimum size. */


/*  Maximum of fast unsigned integral types having a minimum size. */


/*  Values to test for integral types holding `void *' pointer. */


/*  Minimum for largest signed integral type. */


/*  Maximum for largest signed integral type. */


/*  Maximum for largest unsigned integral type. */


/*  Limits of other integer types. */


/*  Limits of `ptrdiff_t' type. */


/*  Limits of `sig_atomic_t'. */


/*  Limit of `size_t' type. */


/*  Limits of `wchar_t'. */


/*  These constants might also be defined in <wchar.h>. */


/*  Limits of `wint_t'. */


/*  Signed. */


/*  Unsigned. */


/*  Maximal type. */


/*  Macros. */


/*  ======= */


/*  Sizes of global arrays */


/*  Resampling flags. */


/*  ----------------- */

/* These macros define flag values which may be passed to
   astResample<X> (via the "flags" argument) to provide control over
   resampling operations. */

/* These macros identify standard sub-pixel interpolation algorithms
   for use by astResample<X>. They are used by giving the macro's
   value for the "interp" argument. */


/*  64 bit types */
typedef int64_t INT_BIG;
typedef uint64_t UINT_BIG;

/* Flags defining the meaning of each bit in the "flags" field of the
   Mapping structure. */


/*  Type Definitions. */


/*  ================= */


/*  Mapping structure. */


/*  ------------------ */

/* This structure contains all information that is unique to each
   object in the class (e.g. its instance variables). */
typedef struct AstMapping {


/*  Attributes inherited from the parent class. */
    AstObject object;

/*  Parent class structure */


/*  Attributes specific to objects in this class. */
    char invert;

/*  Mapping inverted? */
    char flags;

/*  Bit-wise flags describing the Mapping */
    int nin;

/*  Number of input coordinates */
    int nout;

/*  Number of output coordinates */
    char report;

/*  Report when converting coordinates? */
    char tran_forward;

/*  Forward transformation defined? */
    char tran_inverse;

/*  Inverse transformation defined? */
} AstMapping;


/*  Virtual function table. */


/*  ----------------------- */

/* This table contains all information that is the same for all
   objects in the class (e.g. pointers to its virtual functions). */


/*  Function prototypes. */


/*  ==================== */


/*  Prototypes for standard class functions. */


/*  ---------------------------------------- */
AstMapping *astCheckMapping_(AstMapping *, int *);

/*  Check class membership */
int astIsAMapping_(const AstMapping *, int *);

/*  Test class membership */


/*  NB. There is no constructor function for this class. */


/*  Prototypes for member functions. */


/*  -------------------------------- */
int astResampleB_(AstMapping *, int, const int[], const int[],
		  const signed char[], const signed char[], int,
		  void (*)(void), const double[], int, double, int,
		  signed char, int, const int[], const int[], const int[],
		  const int[], signed char[], signed char[], int *);
int astResampleD_(AstMapping *, int, const int[], const int[],
		  const double[], const double[], int, void (*)(void),
		  const double[], int, double, int, double, int,
		  const int[], const int[], const int[], const int[],
		  double[], double[], int *);
int astResampleF_(AstMapping *, int, const int[], const int[],
		  const float[], const float[], int, void (*)(void),
		  const double[], int, double, int, float, int,
		  const int[], const int[], const int[], const int[],
		  float[], float[], int *);
int astResampleI_(AstMapping *, int, const int[], const int[], const int[],
		  const int[], int, void (*)(void), const double[], int,
		  double, int, int, int, const int[], const int[],
		  const int[], const int[], int[], int[], int *);
int astResampleK_(AstMapping *, int, const int[], const int[],
		  const INT_BIG[], const INT_BIG[], int, void (*)(void),
		  const double[], int, double, int, INT_BIG, int,
		  const int[], const int[], const int[], const int[],
		  INT_BIG[], INT_BIG[], int *);
int astResampleL_(AstMapping *, int, const int[], const int[],
		  const long int[], const long int[], int, void (*)(void),
		  const double[], int, double, int, long int, int,
		  const int[], const int[], const int[], const int[],
		  long int[], long int[], int *);
int astResampleS_(AstMapping *, int, const int[], const int[],
		  const short int[], const short int[], int,
		  void (*)(void), const double[], int, double, int,
		  short int, int, const int[], const int[], const int[],
		  const int[], short int[], short int[], int *);
int astResampleUB_(AstMapping *, int, const int[], const int[],
		   const unsigned char[], const unsigned char[], int,
		   void (*)(void), const double[], int, double, int,
		   unsigned char, int, const int[], const int[],
		   const int[], const int[], unsigned char[],
		   unsigned char[], int *);
int astResampleUI_(AstMapping *, int, const int[], const int[],
		   const unsigned int[], const unsigned int[], int,
		   void (*)(void), const double[], int, double, int,
		   unsigned int, int, const int[], const int[],
		   const int[], const int[], unsigned int[],
		   unsigned int[], int *);
int astResampleUK_(AstMapping *, int, const int[], const int[],
		   const UINT_BIG[], const UINT_BIG[], int, void (*)(void),
		   const double[], int, double, int, UINT_BIG, int,
		   const int[], const int[], const int[], const int[],
		   UINT_BIG[], UINT_BIG[], int *);
int astResampleUL_(AstMapping *, int, const int[], const int[],
		   const unsigned long int[], const unsigned long int[],
		   int, void (*)(void), const double[], int, double, int,
		   unsigned long int, int, const int[], const int[],
		   const int[], const int[], unsigned long int[],
		   unsigned long int[], int *);
int astResampleUS_(AstMapping *, int, const int[], const int[],
		   const unsigned short int[], const unsigned short int[],
		   int, void (*)(void), const double[], int, double, int,
		   unsigned short int, int, const int[], const int[],
		   const int[], const int[], unsigned short int[],
		   unsigned short int[], int *);
int astResampleLD_(AstMapping *, int, const int[], const int[],
		   const long double[], const long double[], int,
		   void (*)(void), const double[], int, double, int,
		   long double, int, const int[], const int[], const int[],
		   const int[], long double[], long double[], int *);
void astRebinD_(AstMapping *, double, int, const int[], const int[],
		const double[], const double[], int, const double[], int,
		double, int, double, int, const int[], const int[],
		const int[], const int[], double[], double[], int *);
void astRebinSeqD_(AstMapping *, double, int, const int[], const int[],
		   const double[], const double[], int, const double[],
		   int, double, int, double, int, const int[], const int[],
		   const int[], const int[], double[], double[], double[],
		   int64_t *, int *);
void astRebinF_(AstMapping *, double, int, const int[], const int[],
		const float[], const float[], int, const double[], int,
		double, int, float, int, const int[], const int[],
		const int[], const int[], float[], float[], int *);
void astRebinSeqF_(AstMapping *, double, int, const int[], const int[],
		   const float[], const float[], int, const double[], int,
		   double, int, float, int, const int[], const int[],
		   const int[], const int[], float[], float[], double[],
		   int64_t *, int *);
void astRebinI_(AstMapping *, double, int, const int[], const int[],
		const int[], const int[], int, const double[], int, double,
		int, int, int, const int[], const int[], const int[],
		const int[], int[], int[], int *);
void astRebinSeqI_(AstMapping *, double, int, const int[], const int[],
		   const int[], const int[], int, const double[], int,
		   double, int, int, int, const int[], const int[],
		   const int[], const int[], int[], int[], double[],
		   int64_t *, int *);
void astRebinB_(AstMapping *, double, int, const int[], const int[],
		const signed char[], const signed char[], int,
		const double[], int, double, int, signed char, int,
		const int[], const int[], const int[], const int[],
		signed char[], signed char[], int *);
void astRebinSeqB_(AstMapping *, double, int, const int[], const int[],
		   const signed char[], const signed char[], int,
		   const double[], int, double, int, signed char, int,
		   const int[], const int[], const int[], const int[],
		   signed char[], signed char[], double[], int64_t *,
		   int *);
void astRebinUB_(AstMapping *, double, int, const int[], const int[],
		 const unsigned char[], const unsigned char[], int,
		 const double[], int, double, int, unsigned char, int,
		 const int[], const int[], const int[], const int[],
		 unsigned char[], unsigned char[], int *);
void astRebinSeqUB_(AstMapping *, double, int, const int[], const int[],
		    const unsigned char[], const unsigned char[], int,
		    const double[], int, double, int, unsigned char, int,
		    const int[], const int[], const int[], const int[],
		    unsigned char[], unsigned char[], double[], int64_t *,
		    int *);
void astRebinLD_(AstMapping *, double, int, const int[], const int[],
		 const long double[], const long double[], int,
		 const double[], int, double, int, long double, int,
		 const int[], const int[], const int[], const int[],
		 long double[], long double[], int *);
void astRebinSeqLD_(AstMapping *, double, int, const int[], const int[],
		    const long double[], const long double[], int,
		    const double[], int, double, int, long double, int,
		    const int[], const int[], const int[], const int[],
		    long double[], long double[], double[], int64_t *,
		    int *);
AstMapping *astRemoveRegions_(AstMapping *, int *);
AstMapping *astSimplify_(AstMapping *, int *);
void astInvert_(AstMapping *, int *);
int astLinearApprox_(AstMapping *, const double *, const double *, double,
		     double *, int *);
int astQuadApprox_(AstMapping *, const double[2], const double[2], int,
		   int, double *, double *, int *);
void astTran1_(AstMapping *, int, const double[], int, double[], int *);
void astTran2_(AstMapping *, int, const double[], const double[], int,
	       double[], double[], int *);
void astTranGrid_(AstMapping *, int, const int[], const int[], double, int,
		  int, int, int, double *, int *);
void astTranN_(AstMapping *, int, int, int, const double *, int, int, int,
	       double *, int *);
void astTranP_(AstMapping *, int, int, const double *[], int, int,
	       double *[], int *);
void astDecomposeId_(AstMapping *, AstMapping **, AstMapping **, int *,
		     int *, int *, int *);
void astMapBoxId_(AstMapping *, const double[], const double[], int, int,
		  double *, double *, double[], double[], int *);
double astRateId_(AstMapping *, double *, int, int, int *);
void astMapSplitId_(AstMapping *, int, const int *, int *, AstMapping **,
		    int *);


/*  Function interfaces. */


/*  ==================== */

/* These macros are wrap-ups for the functions defined by this class to make
   them easier to invoke (e.g. to avoid type mis-matches when passing pointers
   to objects from derived classes). */


/*  Interfaces to standard class functions. */


/*  --------------------------------------- */

/* Some of these functions provide validation, so we cannot use them to
   validate their own arguments. We must use a cast when passing object
   pointers (so that they can accept objects from derived classes). */


/*  Check class membership. */


/*  Test class membership. */


/*  NB. There is no constructor function for this class. */


/*  Interfaces to member functions. */


/*  ------------------------------- */

/* Here we make use of astCheckMapping (et al.) to validate Mapping
   pointers before use. This provides a contextual error report if a
   pointer to the wrong sort of object is supplied. */


/*  Module Variables. */


/*  ================= */

/* Pointer to user-supplied (FORTRAN 77) interpolation function for
   use by AST_RESAMPLE<X>. */
static void (*ast_resample_FINTERP) ();


/*  Interpolation function interface. */


/*  ================================= */

/* These functions are associated with allowing FORTRAN 77
   implementations of interpolation functions to be passed to
   AST_RESAMPLE<X> via the FORTRAN 77 interface and then to be invoked
   when necessary by the C code in the main implementation of
   astResample<X>. */

/* Define a macro which defines an interface function called
   ast_resample_uinterp<X> for a specific data type.

   The resulting function has a suitable interface to allow it to be
   passed as an interpolation function to the C interface of
   astResample<X> in the case where the "interp" parameter is set to
   AST__UINTERP. In turn, it invokes the equivalent user-supplied
   FORTRAN 77 interpolation function, a pointer to which should
   previously have been stored in the static variable
   "ast_resample_FINTERP". */

/* Invoke the above macro to define an interface function for each
   required data type. */
static void ast_resample_uinterpD(int ndim, const int lbnd[],
				  const int ubnd[], const double in[],
				  const double in_var[], int npoint,
				  const int offset[],
				  const double *const coords[],
				  const double params[], int flags,
				  double badval, double *out,
				  double *out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (double *) in, (double *) in_var, &npoint,
			     (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (double *) out, (double *) out_var, nbad,
			     &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpF(int ndim, const int lbnd[],
				  const int ubnd[], const float in[],
				  const float in_var[], int npoint,
				  const int offset[],
				  const double *const coords[],
				  const double params[], int flags,
				  float badval, float *out, float *out_var,
				  int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (float *) in, (float *) in_var, &npoint,
			     (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (float *) out, (float *) out_var, nbad,
			     &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpI(int ndim, const int lbnd[],
				  const int ubnd[], const int in[],
				  const int in_var[], int npoint,
				  const int offset[],
				  const double *const coords[],
				  const double params[], int flags,
				  int badval, int *out, int *out_var,
				  int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd, (int *) in,
			     (int *) in_var, &npoint, (int *) offset,
			     (double *) coords[0], (double *) params,
			     &flags, &badval, (int *) out, (int *) out_var,
			     nbad, &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpUI(int ndim, const int lbnd[],
				   const int ubnd[],
				   const unsigned int in[],
				   const unsigned int in_var[], int npoint,
				   const int offset[],
				   const double *const coords[],
				   const double params[], int flags,
				   unsigned int badval, unsigned int *out,
				   unsigned int *out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd, (int *) in,
			     (int *) in_var, &npoint, (int *) offset,
			     (double *) coords[0], (double *) params,
			     &flags, &badval, (int *) out, (int *) out_var,
			     nbad, &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpK(int ndim, const int lbnd[],
				  const int ubnd[], const INT_BIG in[],
				  const INT_BIG in_var[], int npoint,
				  const int offset[],
				  const double *const coords[],
				  const double params[], int flags,
				  INT_BIG badval, INT_BIG * out,
				  INT_BIG * out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (int64_t *) in, (int64_t *) in_var, &npoint,
			     (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (int64_t *) out, (int64_t *) out_var, nbad,
			     &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpUK(int ndim, const int lbnd[],
				   const int ubnd[], const UINT_BIG in[],
				   const UINT_BIG in_var[], int npoint,
				   const int offset[],
				   const double *const coords[],
				   const double params[], int flags,
				   UINT_BIG badval, UINT_BIG * out,
				   UINT_BIG * out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (int64_t *) in, (int64_t *) in_var, &npoint,
			     (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (int64_t *) out, (int64_t *) out_var, nbad,
			     &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpS(int ndim, const int lbnd[],
				  const int ubnd[], const short int in[],
				  const short int in_var[], int npoint,
				  const int offset[],
				  const double *const coords[],
				  const double params[], int flags,
				  short int badval, short int *out,
				  short int *out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (short int *) in, (short int *) in_var,
			     &npoint, (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (short int *) out, (short int *) out_var,
			     nbad, &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpUS(int ndim, const int lbnd[],
				   const int ubnd[],
				   const unsigned short int in[],
				   const unsigned short int in_var[],
				   int npoint, const int offset[],
				   const double *const coords[],
				   const double params[], int flags,
				   unsigned short int badval,
				   unsigned short int *out,
				   unsigned short int *out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (unsigned short int *) in,
			     (unsigned short int *) in_var, &npoint,
			     (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (unsigned short int *) out,
			     (unsigned short int *) out_var, nbad,
			     &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpB(int ndim, const int lbnd[],
				  const int ubnd[], const signed char in[],
				  const signed char in_var[], int npoint,
				  const int offset[],
				  const double *const coords[],
				  const double params[], int flags,
				  signed char badval, signed char *out,
				  signed char *out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (signed char *) in, (signed char *) in_var,
			     &npoint, (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (signed char *) out, (signed char *) out_var,
			     nbad, &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}

static void ast_resample_uinterpUB(int ndim, const int lbnd[],
				   const int ubnd[],
				   const unsigned char in[],
				   const unsigned char in_var[],
				   int npoint, const int offset[],
				   const double *const coords[],
				   const double params[], int flags,
				   unsigned char badval,
				   unsigned char *out,
				   unsigned char *out_var, int *nbad)
{
    int STATUS;
    int *status;

/*  Get a pointer to the inherited staus value. */
    status = astGetStatusPtr_();

/*  Obtain the C status and then invoke the FORTRAN 77 interpolation
    function via the stored pointer. Note that the "coords" array we pass
    to FORTRAN has to be a contiguous 2-d array, so we must de-reference
    one level of pointer compared to the C case. */
    STATUS = (*status);
    (*ast_resample_FINTERP) (&ndim, (int *) lbnd, (int *) ubnd,
			     (unsigned char *) in,
			     (unsigned char *) in_var, &npoint,
			     (int *) offset, (double *) coords[0],
			     (double *) params, &flags, &badval,
			     (unsigned char *) out,
			     (unsigned char *) out_var, nbad, &STATUS);

/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}



/*  Undefine the macro. */

/* Define a function called ast_resample_ukern1 which has a suitable
   interface to allow it to be passed as an interpolation function to
   the C interface of astResample<X> in the case where the "interp"
   parameter is set to AST__UKERN1. In turn, it invokes the equivalent
   user-supplied FORTRAN 77 interpolation function, a pointer to which
   should previously have been stored in the static variable
   "ast_resample_FINTERP". */
static void ast_resample_ukern1(double offset, const double params[],
				int flags, double *value)
{
    int STATUS;
    int *status;

/* Obtain the C status and then invoke the FORTRAN 77 interpolation
   function via the stored pointer. */
    status = astGetStatusPtr_();
    STATUS = (*status);
    (*ast_resample_FINTERP) (&offset,
			     (double *) params, &flags, value, &STATUS);


/*  Set the C status to the returned FORTRAN 77 status. */
    ((*status) = (STATUS));
}



/*  FORTRAN interface functions. */


/*  ============================ */


/*  These functions implement the remainder of the FORTRAN interface. */
void ast_decompose_(int *const THIS,
		    int *const MAP1,
		    int *const MAP2,
		    int *const SERIES,
		    int *const INVERT1,
		    int *const INVERT2, int *const STATUS)
{






    AstMapping *map1;
    AstMapping *map2;
    int series;
    astAt_("AST_DECOMPOSE", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astDecomposeId_(astCheckMapping_((AstMapping *) ((void *)
							  astCheckLock_
							  (astMakePointer_
							   ((AstObject
							     *) (((void *)
								  astI2P_
								  (*THIS,
								   status))),
							    status),
							   status)),
					  astGetStatusPtr_()),
			 (AstMapping **) (&map1), (AstMapping **) (&map2),
			 &series, INVERT1, INVERT2, status));
	*MAP1 = astP2I_((AstObject *) (map1), status);
	*MAP2 = astP2I_((AstObject *) (map2), status);
	*SERIES = (series) ? 1 : 0;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}

void ast_invert_(int *const THIS, int *const STATUS)
{

    astAt_("AST_INVERT", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astInvert_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}

int ast_isamapping_(int *const THIS, int *const STATUS)
{

    int (RESULT);
    astAt_("AST_ISAMAPPING", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	RESULT =
	    (astIsAMapping_
	     ((const AstMapping *) ((void *)
				    astCheckLock_(astMakePointer_
						  ((AstObject
						    *) (((void *)
							 astI2P_(*THIS,
								 status))),
						   status), status)),
	      astGetStatusPtr_()))? 1 : 0;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
    return RESULT;
}

int ast_linearapprox_(int *const THIS,
		      double *const LBND,
		      double *const UBND,
		      double *const TOL,
		      double *const FIT, int *const STATUS)
{





    int (RESULT);
    astAt_("AST_LINEARAPPROX", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	RESULT =
	    (astLinearApprox_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), LBND, UBND, *TOL, FIT, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
    return RESULT;
}

int ast_quadapprox_(int *const THIS,
		    double *const LBND,
		    double *const UBND,
		    int *const NX,
		    int *const NY,
		    double *const FIT,
		    double *const RMS, int *const STATUS)
{







    int (RESULT);
    astAt_("AST_QUADAPPROX", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	RESULT =
	    (astQuadApprox_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), LBND, UBND, *NX, *NY, FIT, RMS,
	      status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
    return RESULT;
}

void ast_mapbox_(int *const THIS,
		 double *const LBND_IN,
		 double *const UBND_IN,
		 int *const FORWARD,
		 int *const COORD_OUT,
		 double *const LBND_OUT,
		 double *const UBND_OUT,
		 double *const XL, double *const XU, int *const STATUS)
{









    double lbnd_out;
    double ubnd_out;
    astAt_("AST_MAPBOX", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astMapBoxId_(astCheckMapping_((AstMapping *) ((void *)
						       astCheckLock_
						       (astMakePointer_
							((AstObject
							  *) (((void *)
							       astI2P_
							       (*THIS,
								status))),
							 status), status)),
				       astGetStatusPtr_()), LBND_IN,
		      UBND_IN, (*FORWARD), *COORD_OUT, &lbnd_out,
		      &ubnd_out, XL, XU, status));
	*LBND_OUT = lbnd_out;
	*UBND_OUT = ubnd_out;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}


/* AST_RESAMPLE<X> requires a function for each possible data type, so
   define it via a macro. */

/* Invoke the above macro to define a function for each data
   type. Include synonyms for some functions. */
int ast_resampled_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, double *const IN,
		   double *const IN_VAR, int *const INTERP,
		   void (*FINTERP) (), double *const PARAMS,
		   int *const FLAGS, double *const TOL, int *const MAXPIX,
		   double *const BADVAL, int *const NDIM_OUT,
		   int *const LBND_OUT, int *const UBND_OUT,
		   int *const LBND, int *const UBND, double *const OUT,
		   double *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    double *out_var;
    const double *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "D", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpD;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const double *) IN_VAR;
	    out_var = (double *) OUT_VAR;
	}
	RESULT =
	    (astResampleD_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const double *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (double *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampler_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, float *const IN,
		   float *const IN_VAR, int *const INTERP,
		   void (*FINTERP) (), double *const PARAMS,
		   int *const FLAGS, double *const TOL, int *const MAXPIX,
		   float *const BADVAL, int *const NDIM_OUT,
		   int *const LBND_OUT, int *const UBND_OUT,
		   int *const LBND, int *const UBND, float *const OUT,
		   float *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    float *out_var;
    const float *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "R", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpF;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const float *) IN_VAR;
	    out_var = (float *) OUT_VAR;
	}
	RESULT =
	    (astResampleF_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const float *) IN, in_var, *INTERP, finterp, PARAMS, *FLAGS,
	      *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND,
	      UBND, (float *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resamplei_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, int *const IN, int *const IN_VAR,
		   int *const INTERP, void (*FINTERP) (),
		   double *const PARAMS, int *const FLAGS,
		   double *const TOL, int *const MAXPIX, int *const BADVAL,
		   int *const NDIM_OUT, int *const LBND_OUT,
		   int *const UBND_OUT, int *const LBND, int *const UBND,
		   int *const OUT, int *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    int *out_var;
    const int *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "I", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpI;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const int *) IN_VAR;
	    out_var = (int *) OUT_VAR;
	}
	RESULT =
	    (astResampleI_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const int *) IN, in_var, *INTERP, finterp, PARAMS, *FLAGS,
	      *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND,
	      UBND, (int *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampleui_(int *const THIS, int *const NDIM_IN,
		    int *const LBND_IN, int *const UBND_IN, int *const IN,
		    int *const IN_VAR, int *const INTERP,
		    void (*FINTERP) (), double *const PARAMS,
		    int *const FLAGS, double *const TOL, int *const MAXPIX,
		    int *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND, int *const OUT,
		    int *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    unsigned int *out_var;
    const unsigned int *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "UI", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpUI;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const unsigned int *) IN_VAR;
	    out_var = (unsigned int *) OUT_VAR;
	}
	RESULT =
	    (astResampleUI_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const unsigned int *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (unsigned int *) OUT, out_var,
	      status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resamplek_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, int64_t * const IN,
		   int64_t * const IN_VAR, int *const INTERP,
		   void (*FINTERP) (), double *const PARAMS,
		   int *const FLAGS, double *const TOL, int *const MAXPIX,
		   int64_t * const BADVAL, int *const NDIM_OUT,
		   int *const LBND_OUT, int *const UBND_OUT,
		   int *const LBND, int *const UBND, int64_t * const OUT,
		   int64_t * const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    INT_BIG *out_var;
    const INT_BIG *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "K", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpK;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const INT_BIG *) IN_VAR;
	    out_var = (INT_BIG *) OUT_VAR;
	}
	RESULT =
	    (astResampleK_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const INT_BIG *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (INT_BIG *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampleuk_(int *const THIS, int *const NDIM_IN,
		    int *const LBND_IN, int *const UBND_IN,
		    int64_t * const IN, int64_t * const IN_VAR,
		    int *const INTERP, void (*FINTERP) (),
		    double *const PARAMS, int *const FLAGS,
		    double *const TOL, int *const MAXPIX,
		    int64_t * const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND, int64_t * const OUT,
		    int64_t * const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    UINT_BIG *out_var;
    const UINT_BIG *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "UK", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpUK;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const UINT_BIG *) IN_VAR;
	    out_var = (UINT_BIG *) OUT_VAR;
	}
	RESULT =
	    (astResampleUK_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const UINT_BIG *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (UINT_BIG *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resamples_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, short int *const IN,
		   short int *const IN_VAR, int *const INTERP,
		   void (*FINTERP) (), double *const PARAMS,
		   int *const FLAGS, double *const TOL, int *const MAXPIX,
		   short int *const BADVAL, int *const NDIM_OUT,
		   int *const LBND_OUT, int *const UBND_OUT,
		   int *const LBND, int *const UBND, short int *const OUT,
		   short int *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    short int *out_var;
    const short int *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "S", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpS;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const short int *) IN_VAR;
	    out_var = (short int *) OUT_VAR;
	}
	RESULT =
	    (astResampleS_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const short int *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (short int *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampleus_(int *const THIS, int *const NDIM_IN,
		    int *const LBND_IN, int *const UBND_IN,
		    unsigned short int *const IN,
		    unsigned short int *const IN_VAR, int *const INTERP,
		    void (*FINTERP) (), double *const PARAMS,
		    int *const FLAGS, double *const TOL, int *const MAXPIX,
		    unsigned short int *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND,
		    unsigned short int *const OUT,
		    unsigned short int *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    unsigned short int *out_var;
    const unsigned short int *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "US", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpUS;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const unsigned short int *) IN_VAR;
	    out_var = (unsigned short int *) OUT_VAR;
	}
	RESULT =
	    (astResampleUS_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const unsigned short int *) IN, in_var, *INTERP, finterp,
	      PARAMS, *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (unsigned short int *) OUT, out_var,
	      status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resamplew_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, short int *const IN,
		   short int *const IN_VAR, int *const INTERP,
		   void (*FINTERP) (), double *const PARAMS,
		   int *const FLAGS, double *const TOL, int *const MAXPIX,
		   short int *const BADVAL, int *const NDIM_OUT,
		   int *const LBND_OUT, int *const UBND_OUT,
		   int *const LBND, int *const UBND, short int *const OUT,
		   short int *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    short int *out_var;
    const short int *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "W", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpS;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const short int *) IN_VAR;
	    out_var = (short int *) OUT_VAR;
	}
	RESULT =
	    (astResampleS_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const short int *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (short int *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampleuw_(int *const THIS, int *const NDIM_IN,
		    int *const LBND_IN, int *const UBND_IN,
		    unsigned short int *const IN,
		    unsigned short int *const IN_VAR, int *const INTERP,
		    void (*FINTERP) (), double *const PARAMS,
		    int *const FLAGS, double *const TOL, int *const MAXPIX,
		    unsigned short int *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND,
		    unsigned short int *const OUT,
		    unsigned short int *const OUT_VAR, int *const STATUS)
{
    void (*finterp) ();
    unsigned short int *out_var;
    const unsigned short int *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "UW", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpUS;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const unsigned short int *) IN_VAR;
	    out_var = (unsigned short int *) OUT_VAR;
	}
	RESULT =
	    (astResampleUS_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const unsigned short int *) IN, in_var, *INTERP, finterp,
	      PARAMS, *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (unsigned short int *) OUT, out_var,
	      status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampleb_(int *const THIS, int *const NDIM_IN, int *const LBND_IN,
		   int *const UBND_IN, signed char *const IN,
		   signed char *const IN_VAR, int *const INTERP,
		   void (*FINTERP) (), double *const PARAMS,
		   int *const FLAGS, double *const TOL, int *const MAXPIX,
		   signed char *const BADVAL, int *const NDIM_OUT,
		   int *const LBND_OUT, int *const UBND_OUT,
		   int *const LBND, int *const UBND,
		   signed char *const OUT, signed char *const OUT_VAR,
		   int *const STATUS)
{
    void (*finterp) ();
    signed char *out_var;
    const signed char *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "B", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpB;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const signed char *) IN_VAR;
	    out_var = (signed char *) OUT_VAR;
	}
	RESULT =
	    (astResampleB_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const signed char *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (signed char *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}

int ast_resampleub_(int *const THIS, int *const NDIM_IN,
		    int *const LBND_IN, int *const UBND_IN,
		    unsigned char *const IN, unsigned char *const IN_VAR,
		    int *const INTERP, void (*FINTERP) (),
		    double *const PARAMS, int *const FLAGS,
		    double *const TOL, int *const MAXPIX,
		    unsigned char *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND,
		    unsigned char *const OUT, unsigned char *const OUT_VAR,
		    int *const STATUS)
{
    void (*finterp) ();
    unsigned char *out_var;
    const unsigned char *in_var;
    int RESULT;
    astAt_("AST_RESAMPLE" "UB", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If *INTERP is set to a value that requires a user-supplied
    interpolation function, then store a pointer to the supplied FORTRAN
    77 version of this function and use the appropriate C wrapper
    function (defined above) to invoke it. */
	if (*INTERP == (3)
/* User-supplied interpolation function */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_uinterpUB;
	} else if (*INTERP == (1)
/* User-supplied 1-d interpolation kernel */
	    ) {
	    ast_resample_FINTERP = FINTERP;
	    finterp = (void (*)()) ast_resample_ukern1;
	} else {
	    ast_resample_FINTERP = ((void *) 0);
	    finterp = ((void *) 0);
	}
/* If the AST__USEVAR flag is set, use the input and output variance    arrays, otherwise pass NULL pointers. */
	    in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const unsigned char *) IN_VAR;
	    out_var = (unsigned char *) OUT_VAR;
	}
	RESULT =
	    (astResampleUB_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), *NDIM_IN, LBND_IN, UBND_IN,
	      (const unsigned char *) IN, in_var, *INTERP, finterp, PARAMS,
	      *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
	      UBND_OUT, LBND, UBND, (unsigned char *) OUT, out_var,
	      status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    } return RESULT;
}


/* AST_REBIN<X> requires a function for each possible data type, so
   define it via a macro. */

/* Invoke the above macro to define a function for each data
   type. Include synonyms for some functions. */
void ast_rebind_(int *const THIS, double *const WLIM, int *const NDIM_IN,
		 int *const LBND_IN, int *const UBND_IN, double *const IN,
		 double *const IN_VAR, int *const INTERP,
		 double *const PARAMS, int *const FLAGS, double *const TOL,
		 int *const MAXPIX, double *const BADVAL,
		 int *const NDIM_OUT, int *const LBND_OUT,
		 int *const UBND_OUT, int *const LBND, int *const UBND,
		 double *const OUT, double *const OUT_VAR,
		 int *const STATUS)
{
    double *out_var;
    const double *in_var;
    astAt_("AST_REBIN" "D", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If the AST__USEVAR flag is set, use the input and output variance
    arrays, otherwise pass NULL pointers. */
	in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const double *) IN_VAR;
	    out_var = (double *) OUT_VAR;
	}
	(astRebinD_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *WLIM, *NDIM_IN, LBND_IN, UBND_IN,
	  (const double *) IN, in_var, *INTERP, PARAMS, *FLAGS, *TOL,
	  *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND,
	  (double *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinr_(int *const THIS, double *const WLIM, int *const NDIM_IN,
		 int *const LBND_IN, int *const UBND_IN, float *const IN,
		 float *const IN_VAR, int *const INTERP,
		 double *const PARAMS, int *const FLAGS, double *const TOL,
		 int *const MAXPIX, float *const BADVAL,
		 int *const NDIM_OUT, int *const LBND_OUT,
		 int *const UBND_OUT, int *const LBND, int *const UBND,
		 float *const OUT, float *const OUT_VAR, int *const STATUS)
{
    float *out_var;
    const float *in_var;
    astAt_("AST_REBIN" "R", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If the AST__USEVAR flag is set, use the input and output variance
    arrays, otherwise pass NULL pointers. */
	in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const float *) IN_VAR;
	    out_var = (float *) OUT_VAR;
	}
	(astRebinF_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *WLIM, *NDIM_IN, LBND_IN, UBND_IN,
	  (const float *) IN, in_var, *INTERP, PARAMS, *FLAGS, *TOL,
	  *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND,
	  (float *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebini_(int *const THIS, double *const WLIM, int *const NDIM_IN,
		 int *const LBND_IN, int *const UBND_IN, int *const IN,
		 int *const IN_VAR, int *const INTERP,
		 double *const PARAMS, int *const FLAGS, double *const TOL,
		 int *const MAXPIX, int *const BADVAL, int *const NDIM_OUT,
		 int *const LBND_OUT, int *const UBND_OUT, int *const LBND,
		 int *const UBND, int *const OUT, int *const OUT_VAR,
		 int *const STATUS)
{
    int *out_var;
    const int *in_var;
    astAt_("AST_REBIN" "I", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If the AST__USEVAR flag is set, use the input and output variance
    arrays, otherwise pass NULL pointers. */
	in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const int *) IN_VAR;
	    out_var = (int *) OUT_VAR;
	}
	(astRebinI_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *WLIM, *NDIM_IN, LBND_IN, UBND_IN,
	  (const int *) IN, in_var, *INTERP, PARAMS, *FLAGS, *TOL, *MAXPIX,
	  *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND, (int *) OUT,
	  out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinb_(int *const THIS, double *const WLIM, int *const NDIM_IN,
		 int *const LBND_IN, int *const UBND_IN,
		 signed char *const IN, signed char *const IN_VAR,
		 int *const INTERP, double *const PARAMS, int *const FLAGS,
		 double *const TOL, int *const MAXPIX,
		 signed char *const BADVAL, int *const NDIM_OUT,
		 int *const LBND_OUT, int *const UBND_OUT, int *const LBND,
		 int *const UBND, signed char *const OUT,
		 signed char *const OUT_VAR, int *const STATUS)
{
    signed char *out_var;
    const signed char *in_var;
    astAt_("AST_REBIN" "B", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If the AST__USEVAR flag is set, use the input and output variance
    arrays, otherwise pass NULL pointers. */
	in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const signed char *) IN_VAR;
	    out_var = (signed char *) OUT_VAR;
	}
	(astRebinB_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *WLIM, *NDIM_IN, LBND_IN, UBND_IN,
	  (const signed char *) IN, in_var, *INTERP, PARAMS, *FLAGS, *TOL,
	  *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND,
	  (signed char *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinub_(int *const THIS, double *const WLIM, int *const NDIM_IN,
		  int *const LBND_IN, int *const UBND_IN,
		  unsigned char *const IN, unsigned char *const IN_VAR,
		  int *const INTERP, double *const PARAMS,
		  int *const FLAGS, double *const TOL, int *const MAXPIX,
		  unsigned char *const BADVAL, int *const NDIM_OUT,
		  int *const LBND_OUT, int *const UBND_OUT,
		  int *const LBND, int *const UBND,
		  unsigned char *const OUT, unsigned char *const OUT_VAR,
		  int *const STATUS)
{
    unsigned char *out_var;
    const unsigned char *in_var;
    astAt_("AST_REBIN" "UB", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  If the AST__USEVAR flag is set, use the input and output variance
    arrays, otherwise pass NULL pointers. */
	in_var = out_var = ((void *) 0);
	if ((16)
/* Use variance arrays? */
	    & *FLAGS) {
	    in_var = (const unsigned char *) IN_VAR;
	    out_var = (unsigned char *) OUT_VAR;
	}
	(astRebinUB_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *WLIM, *NDIM_IN, LBND_IN, UBND_IN,
	  (const unsigned char *) IN, in_var, *INTERP, PARAMS, *FLAGS,
	  *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND,
	  UBND, (unsigned char *) OUT, out_var, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}


/* AST_REBINSEQ<X> requires a function for each possible data type, so
   define it via a macro. */

/* Invoke the above macro to define a function for each data
   type. Include synonyms for some functions. */
void ast_rebinseqd_(int *const THIS, double *const WLIM,
		    int *const NDIM_IN, int *const LBND_IN,
		    int *const UBND_IN, double *const IN,
		    double *const IN_VAR, int *const INTERP,
		    double *const PARAMS, int *const FLAGS,
		    double *const TOL, int *const MAXPIX,
		    double *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND, double *const OUT,
		    double *const OUT_VAR, double *const WEIGHTS,
		    int64_t * const NUSED, int *const STATUS)
{
    double *out_var;
    const double *in_var;
    int64_t nused;
    astAt_("AST_REBINSEQ" "D", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  We need the input variances if the AST__USEVAR or AST__VARWGT flag
    is set. Otherwise use a NULL pointer for the input variances. */
	if ((16)
/* Use variance arrays? */
	    & *FLAGS || (1024)
/* Use input variances as weights? */
	    & *FLAGS) {
	    in_var = (const double *) IN_VAR;
	} else {
	    in_var = ((void *) 0);
	}
/* We need the output variances if the AST__USEVAR or AST__GENVAR flag is    set. Otherwise use a NULL pointer for the output variances. */
	    if ((16)
/* Use variance arrays? */
		& *FLAGS || (512)
/* Generate output variances when rebinning? */
		& *FLAGS) {
	    out_var = (double *) OUT_VAR;
	} else {
	    out_var = ((void *) 0);
	} nused = *NUSED;
	(astRebinSeqD_(astCheckMapping_((AstMapping *) ((void *)
							astCheckLock_
							(astMakePointer_
							 ((AstObject
							   *) (((void *)
								astI2P_
								(*THIS,
								 status))),
							  status),
							 status)),
					astGetStatusPtr_()), *WLIM,
		       *NDIM_IN, LBND_IN, UBND_IN, (const double *) IN,
		       in_var, *INTERP, PARAMS, *FLAGS, *TOL, *MAXPIX,
		       *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND,
		       (double *) OUT, out_var, WEIGHTS, &nused, status));
	*NUSED = nused;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinseqr_(int *const THIS, double *const WLIM,
		    int *const NDIM_IN, int *const LBND_IN,
		    int *const UBND_IN, float *const IN,
		    float *const IN_VAR, int *const INTERP,
		    double *const PARAMS, int *const FLAGS,
		    double *const TOL, int *const MAXPIX,
		    float *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND, float *const OUT,
		    float *const OUT_VAR, double *const WEIGHTS,
		    int64_t * const NUSED, int *const STATUS)
{
    float *out_var;
    const float *in_var;
    int64_t nused;
    astAt_("AST_REBINSEQ" "R", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  We need the input variances if the AST__USEVAR or AST__VARWGT flag
    is set. Otherwise use a NULL pointer for the input variances. */
	if ((16)
/* Use variance arrays? */
	    & *FLAGS || (1024)
/* Use input variances as weights? */
	    & *FLAGS) {
	    in_var = (const float *) IN_VAR;
	} else {
	    in_var = ((void *) 0);
	}
/* We need the output variances if the AST__USEVAR or AST__GENVAR flag is    set. Otherwise use a NULL pointer for the output variances. */
	    if ((16)
/* Use variance arrays? */
		& *FLAGS || (512)
/* Generate output variances when rebinning? */
		& *FLAGS) {
	    out_var = (float *) OUT_VAR;
	} else {
	    out_var = ((void *) 0);
	} nused = *NUSED;
	(astRebinSeqF_(astCheckMapping_((AstMapping *) ((void *)
							astCheckLock_
							(astMakePointer_
							 ((AstObject
							   *) (((void *)
								astI2P_
								(*THIS,
								 status))),
							  status),
							 status)),
					astGetStatusPtr_()), *WLIM,
		       *NDIM_IN, LBND_IN, UBND_IN, (const float *) IN,
		       in_var, *INTERP, PARAMS, *FLAGS, *TOL, *MAXPIX,
		       *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND,
		       (float *) OUT, out_var, WEIGHTS, &nused, status));
	*NUSED = nused;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinseqi_(int *const THIS, double *const WLIM,
		    int *const NDIM_IN, int *const LBND_IN,
		    int *const UBND_IN, int *const IN, int *const IN_VAR,
		    int *const INTERP, double *const PARAMS,
		    int *const FLAGS, double *const TOL, int *const MAXPIX,
		    int *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND, int *const OUT,
		    int *const OUT_VAR, double *const WEIGHTS,
		    int64_t * const NUSED, int *const STATUS)
{
    int *out_var;
    const int *in_var;
    int64_t nused;
    astAt_("AST_REBINSEQ" "I", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  We need the input variances if the AST__USEVAR or AST__VARWGT flag
    is set. Otherwise use a NULL pointer for the input variances. */
	if ((16)
/* Use variance arrays? */
	    & *FLAGS || (1024)
/* Use input variances as weights? */
	    & *FLAGS) {
	    in_var = (const int *) IN_VAR;
	} else {
	    in_var = ((void *) 0);
	}
/* We need the output variances if the AST__USEVAR or AST__GENVAR flag is    set. Otherwise use a NULL pointer for the output variances. */
	    if ((16)
/* Use variance arrays? */
		& *FLAGS || (512)
/* Generate output variances when rebinning? */
		& *FLAGS) {
	    out_var = (int *) OUT_VAR;
	} else {
	    out_var = ((void *) 0);
	} nused = *NUSED;
	(astRebinSeqI_(astCheckMapping_((AstMapping *) ((void *)
							astCheckLock_
							(astMakePointer_
							 ((AstObject
							   *) (((void *)
								astI2P_
								(*THIS,
								 status))),
							  status),
							 status)),
					astGetStatusPtr_()), *WLIM,
		       *NDIM_IN, LBND_IN, UBND_IN, (const int *) IN,
		       in_var, *INTERP, PARAMS, *FLAGS, *TOL, *MAXPIX,
		       *BADVAL, *NDIM_OUT, LBND_OUT, UBND_OUT, LBND, UBND,
		       (int *) OUT, out_var, WEIGHTS, &nused, status));
	*NUSED = nused;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinseqb_(int *const THIS, double *const WLIM,
		    int *const NDIM_IN, int *const LBND_IN,
		    int *const UBND_IN, signed char *const IN,
		    signed char *const IN_VAR, int *const INTERP,
		    double *const PARAMS, int *const FLAGS,
		    double *const TOL, int *const MAXPIX,
		    signed char *const BADVAL, int *const NDIM_OUT,
		    int *const LBND_OUT, int *const UBND_OUT,
		    int *const LBND, int *const UBND,
		    signed char *const OUT, signed char *const OUT_VAR,
		    double *const WEIGHTS, int64_t * const NUSED,
		    int *const STATUS)
{
    signed char *out_var;
    const signed char *in_var;
    int64_t nused;
    astAt_("AST_REBINSEQ" "B", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  We need the input variances if the AST__USEVAR or AST__VARWGT flag
    is set. Otherwise use a NULL pointer for the input variances. */
	if ((16)
/* Use variance arrays? */
	    & *FLAGS || (1024)
/* Use input variances as weights? */
	    & *FLAGS) {
	    in_var = (const signed char *) IN_VAR;
	} else {
	    in_var = ((void *) 0);
	}
/* We need the output variances if the AST__USEVAR or AST__GENVAR flag is    set. Otherwise use a NULL pointer for the output variances. */
	    if ((16)
/* Use variance arrays? */
		& *FLAGS || (512)
/* Generate output variances when rebinning? */
		& *FLAGS) {
	    out_var = (signed char *) OUT_VAR;
	} else {
	    out_var = ((void *) 0);
	} nused = *NUSED;
	(astRebinSeqB_(astCheckMapping_((AstMapping *) ((void *)
							astCheckLock_
							(astMakePointer_
							 ((AstObject
							   *) (((void *)
								astI2P_
								(*THIS,
								 status))),
							  status),
							 status)),
					astGetStatusPtr_()), *WLIM,
		       *NDIM_IN, LBND_IN, UBND_IN,
		       (const signed char *) IN, in_var, *INTERP, PARAMS,
		       *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT, LBND_OUT,
		       UBND_OUT, LBND, UBND, (signed char *) OUT, out_var,
		       WEIGHTS, &nused, status));
	*NUSED = nused;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

void ast_rebinsequb_(int *const THIS, double *const WLIM,
		     int *const NDIM_IN, int *const LBND_IN,
		     int *const UBND_IN, unsigned char *const IN,
		     unsigned char *const IN_VAR, int *const INTERP,
		     double *const PARAMS, int *const FLAGS,
		     double *const TOL, int *const MAXPIX,
		     unsigned char *const BADVAL, int *const NDIM_OUT,
		     int *const LBND_OUT, int *const UBND_OUT,
		     int *const LBND, int *const UBND,
		     unsigned char *const OUT,
		     unsigned char *const OUT_VAR, double *const WEIGHTS,
		     int64_t * const NUSED, int *const STATUS)
{
    unsigned char *out_var;
    const unsigned char *in_var;
    int64_t nused;
    astAt_("AST_REBINSEQ" "UB", ((void *) 0), 0, 1, STATUS);
/* Begin a new C scope. */  {

/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */

/*  We need the input variances if the AST__USEVAR or AST__VARWGT flag
    is set. Otherwise use a NULL pointer for the input variances. */
	if ((16)
/* Use variance arrays? */
	    & *FLAGS || (1024)
/* Use input variances as weights? */
	    & *FLAGS) {
	    in_var = (const unsigned char *) IN_VAR;
	} else {
	    in_var = ((void *) 0);
	}
/* We need the output variances if the AST__USEVAR or AST__GENVAR flag is    set. Otherwise use a NULL pointer for the output variances. */
	    if ((16)
/* Use variance arrays? */
		& *FLAGS || (512)
/* Generate output variances when rebinning? */
		& *FLAGS) {
	    out_var = (unsigned char *) OUT_VAR;
	} else {
	    out_var = ((void *) 0);
	} nused = *NUSED;
	(astRebinSeqUB_(astCheckMapping_((AstMapping *) ((void *)
							 astCheckLock_
							 (astMakePointer_
							  ((AstObject
							    *) (((void *)
								 astI2P_
								 (*THIS,
								  status))),
							   status),
							  status)),
					 astGetStatusPtr_()), *WLIM,
			*NDIM_IN, LBND_IN, UBND_IN,
			(const unsigned char *) IN, in_var, *INTERP,
			PARAMS, *FLAGS, *TOL, *MAXPIX, *BADVAL, *NDIM_OUT,
			LBND_OUT, UBND_OUT, LBND, UBND,
			(unsigned char *) OUT, out_var, WEIGHTS, &nused,
			status));
	*NUSED = nused;

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
}}

int ast_removeregions_(int *const THIS, int *const STATUS)
{

    int (RESULT);
    astAt_("AST_REMOVEREGIONS", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	RESULT =
	    astP2I_((AstObject
		     *) (((void *)
			  astMakeId_((AstObject
				      *) (astRemoveRegions_
					  (astCheckMapping_
					   ((AstMapping *) ((void *)
							    astCheckLock_
							    (astMakePointer_
							     ((AstObject
							       *) (((void
								     *)
								    astI2P_
								    (*THIS,
								     status))),
							      status),
							     status)),
					    astGetStatusPtr_()), status)),
				     status))), status);

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
    return RESULT;
}

int ast_simplify_(int *const THIS, int *const STATUS)
{

    int (RESULT);
    astAt_("AST_SIMPLIFY", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	RESULT =
	    astP2I_((AstObject
		     *) (((void *)
			  astMakeId_((AstObject
				      *) (astSimplify_(astCheckMapping_
						       ((AstMapping
							 *) ((void *)
							     astCheckLock_
							     (astMakePointer_
							      ((AstObject
								*) (((void
								      *)
								     astI2P_
								     (*THIS,
								      status))),
							       status),
							      status)),
							astGetStatusPtr_
							()), status)),
				     status))), status);

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
    return RESULT;
}

void ast_tran1_(int *const THIS,
		int *const NPOINT,
		double *const XIN,
		int *const FORWARD, double *const XOUT, int *const STATUS)
{





    astAt_("AST_TRAN1", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astTran1_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *NPOINT, XIN, (*FORWARD), XOUT, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}

void ast_tran2_(int *const THIS,
		int *const NPOINT,
		double *const XIN,
		double *const YIN,
		int *const FORWARD,
		double *const XOUT, double *const YOUT, int *const STATUS)
{







    astAt_("AST_TRAN2", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astTran2_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *NPOINT, XIN, YIN, (*FORWARD), XOUT, YOUT,
	  status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}

void ast_trangrid_(int *const THIS,
		   int *const NCOORD_IN,
		   int *const LBND,
		   int *const UBND,
		   double *const TOL,
		   int *const MAXPIX,
		   int *const FORWARD,
		   int *const NCOORD_OUT,
		   int *const OUTDIM, double *const OUT, int *const STATUS)
{










    astAt_("AST_TRANGRID", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astTranGrid_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *NCOORD_IN, LBND, UBND, *TOL, *MAXPIX,
	  (*FORWARD), *NCOORD_OUT, *OUTDIM, OUT, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}

void ast_trann_(int *const THIS,
		int *const NPOINT,
		int *const NCOORD_IN,
		int *const INDIM,
		double *const IN,
		int *const FORWARD,
		int *const NCOORD_OUT,
		int *const OUTDIM, double *const OUT, int *const STATUS)
{









    astAt_("AST_TRANN", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astTranN_
	 (astCheckMapping_
	  ((AstMapping *) ((void *)
			   astCheckLock_(astMakePointer_
					 ((AstObject
					   *) (((void *)
						astI2P_(*THIS, status))),
					  status), status)),
	   astGetStatusPtr_()), *NPOINT, *NCOORD_IN, *INDIM,
	  (const double *) IN, (*FORWARD), *NCOORD_OUT, *OUTDIM, OUT,
	  status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}

double ast_rate_(int *const THIS,
		 double *const AT,
		 int *const AX1, int *const AX2, int *const STATUS)
{




    double (RESULT);
    astAt_("AST_RATE", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	RESULT =
	    (astRateId_
	     (astCheckMapping_
	      ((AstMapping *) ((void *)
			       astCheckLock_(astMakePointer_
					     ((AstObject
					       *) (((void *)
						    astI2P_(*THIS,
							    status))),
					      status), status)),
	       astGetStatusPtr_()), AT, *AX1, *AX2, status));

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
    return RESULT;
}

void ast_mapsplit_(int *const THIS,
		   int *const NIN,
		   int *const IN,
		   int *const OUT, int *const MAP, int *const STATUS)
{





    AstMapping *map;
    astAt_("AST_MAPSPLIT", ((void *) 0), 0, 1, STATUS);

/* Begin a new C scope. */  {


/*  Ensure that a pointer to the STATUS argument exists. */

/*  Store the STATUS value in a local int. */
	int ast_local_status = *STATUS;
	int *status = &ast_local_status;

/*  Make this int the AST error status variable, saving the address of
    the previous variable. */
	int *ast_previous_status = astWatch_(&ast_local_status);

/*  Execute the code supplied using the new error status variable. */
	(astMapSplitId_(astCheckMapping_((AstMapping *) ((void *)
							 astCheckLock_
							 (astMakePointer_
							  ((AstObject
							    *) (((void *)
								 astI2P_
								 (*THIS,
								  status))),
							   status),
							  status)),
					 astGetStatusPtr_()), *NIN, IN,
			OUT, &map, status));
	*MAP = astP2I_((AstObject *) (map), status);

/*  Restore the original error status variable. */
	(void) astWatch_(ast_previous_status);

/*  Return the final error status to STATUS. */
	*STATUS = ast_local_status;
    }
}
