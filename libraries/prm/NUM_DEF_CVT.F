#include "config.h"
#if HAVE_INTRINSIC_NINT
#  define xNINT NINT
#elif HAVE_INTRINSIC_ANINT
#  define xNINT ANINT
#else
 error Neither NINT nor ANINT is available
#endif

*+
*  Name:
*     NUM_DEF_CVT

*  Type of Module:
*     Fortran include file, to be preprocessed.

*  Purpose:
*     Define NUM_ functions for type conversion.

*  Description:
*     This file defines the NUM_ statement functions which perform type
*     conversion between all the supported PRIMDAT data types.

*  Notes:
*     This file should be preceded by the file NUM_DEC_CVT which
*     declares the function data types and their arguments.
*
*     This file is to be preprocessed by running it through a
*     preprocessor which can expand #include and #define statements.
*     The file config.h should define HAVE_INTRINSIC_ANINT, ...NINT,
*     ...IZEXT, ...JZEXT to be 1 if the Fortran compiler to be used does
*     have the corresponding intrinsics.
*
*     This file is an adaptation of the _set_ of files num_def_cvt_X, for
*     X in {alpha_OSF1, ix86_Linux, mips, sun4 sun4_Solaris}, which were
*     in the original pre-autoconf distribution.  These files had some
*     puzzling features, probably related to their history.  The
*     floating-to-integer functions below are documented to round, and
*     to produce integers rather than reals, so they should be
*     implemented using the NINT intrinsic.  The sun4 and sun4_Solaris
*     originals used ANINT for some reason: I presume this was either an
*     error, or the result of some (long ago?) limitation on the part of
*     the Sun compiler.  At any rate, these now use NINT if it's
*     available, and ANINT only if necessary.
*
*     This file also uses the VMS intrinsics JZEXT and IZEXT if they are
*     available (configure.ac tests for IZEXT, and if it's present
*     presumes that the other VMS intrinsics are also).  If not, they
*     must be defined in the module num1_cvt.c instead.


*  Machine-specific features used:
*     None.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1991 (RFWS):
*        Original version, derived from the SUN4 file.
*     05-OCT-1995 (BKM):
*        Linux version with some NUM1 routines implemented in C.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Preliminary definitions:
*  =======================
*  These are unlikely to vary between machines and are defined first to
*  ensure legal statement ordering below.
      NUM1_WTOI( NUM_ARGW ) = NUM_ARGW
      NUM1_ITOW( NUM_ARGI ) = NUM_ARGI

*  Machine dependent definitions:
*  =============================
*  The following conversions between the unsigned integer types use VMS
*  intrinsic functions which are not directly supported by normal
*  Fortran implementations, so we must define them only if the intrinsic
*  IZEXT (which we take as proxy for the others) is available.  If it is
*  not available, then the following six functions must be defined in
*  the module num1_cvt.c

#if HAVE_INTRINSIC_IZEXT
      NUM1_UBTOI( NUM_ARGUB ) = JZEXT( NUM_ARGUB )
      NUM1_UBTOUW( NUM_ARGUB ) = IZEXT( NUM_ARGUB )
      NUM1_UBTOW( NUM_ARGUB ) = IZEXT( NUM_ARGUB )
      NUM1_UWTOI( NUM_ARGUW ) = JZEXT( NUM_ARGUW )
      NUM1_WTOUB( NUM_ARGW )   =
     :   IIEOR( NUM_ARGW,
     :      IIAND( - IIAND( NUM_ARGW, NUM1_ITOW( 128 ) ),
     :             NUM1_ITOW( -256 ) ) )
      NUM1_ITOUW( NUM_ARGI )   =
     :  JIAND( 65535, NUM1_WTOI( NUM1_ITOW( NUM_ARGI - 32768 ) )
     :                           + 65536 ) - 32768
#else
*  VMS intrinsics IZEXT and co are not supported by this Fortran
*  compiler.  Functions NUM1_UBTOI, NUM1_UBTOUW, NUM1_UBTOW, NUM1_UWTOI,
*  NUM1_WTOUB, and NUM1_ITOUW must therefore be supplied by module num1_cvt.c.
#endif

*  Functions resulting in BYTE.
*  ===========================

      NUM_BTOB( NUM_ARGB )    = NUM_ARGB

      NUM_UBTOB( NUM_ARGUB )  = NUM1_UBTOW( NUM_ARGUB )

      NUM_DTOB( NUM_ARGD )    = xNINT( NUM_ARGD )

      NUM_ITOB( NUM_ARGI )    = NUM_ARGI

      NUM_RTOB( NUM_ARGR )    = xNINT( NUM_ARGR )

      NUM_WTOB( NUM_ARGW )    = NUM_ARGW

      NUM_UWTOB( NUM_ARGUW )  = NUM1_UWTOI( NUM_ARGUW )


*  Functions resulting in DOUBLE PRECISION.
*  =======================================

      NUM_BTOD( NUM_ARGB )    = DBLE( NUM_ARGB )

      NUM_UBTOD( NUM_ARGUB )  = DBLE( NUM1_UBTOI( NUM_ARGUB ) )

      NUM_DTOD( NUM_ARGD )    = NUM_ARGD

      NUM_ITOD( NUM_ARGI )    = DBLE( NUM_ARGI )

      NUM_RTOD( NUM_ARGR )    = DBLE( NUM_ARGR )

      NUM_WTOD( NUM_ARGW )    = DBLE( NUM_ARGW )

      NUM_UWTOD( NUM_ARGUW )  = DBLE( NUM1_UWTOI( NUM_ARGUW ) )


*  Functions resulting in INTEGER.
*  ==============================

      NUM_BTOI( NUM_ARGB )    = NUM_ARGB

      NUM_UBTOI( NUM_ARGUB )  = NUM1_UBTOI( NUM_ARGUB )

      NUM_DTOI( NUM_ARGD )    = xNINT( NUM_ARGD )

      NUM_ITOI( NUM_ARGI )    = NUM_ARGI

      NUM_RTOI( NUM_ARGR )    = xNINT( NUM_ARGR )

      NUM_WTOI( NUM_ARGW )    = NUM_ARGW

      NUM_UWTOI( NUM_ARGUW )  = NUM1_UWTOI( NUM_ARGUW )


*  Functions resulting in REAL.
*  ===========================

      NUM_BTOR( NUM_ARGB )    = REAL( NUM_ARGB )

      NUM_UBTOR( NUM_ARGUB )  = REAL( NUM1_UBTOI( NUM_ARGUB ) )

      NUM_DTOR( NUM_ARGD )    = SNGL( NUM_ARGD )

      NUM_ITOR( NUM_ARGI )    = REAL( NUM_ARGI )

      NUM_RTOR( NUM_ARGR )    = NUM_ARGR

      NUM_WTOR( NUM_ARGW )    = REAL( NUM_ARGW )

      NUM_UWTOR( NUM_ARGUW )  = REAL( NUM1_UWTOI( NUM_ARGUW ) )


*  Functions resulting in WORD.
*  ===========================

      NUM_BTOW( NUM_ARGB )    = NUM_ARGB

      NUM_UBTOW( NUM_ARGUB )  = NUM1_UBTOW( NUM_ARGUB )

      NUM_DTOW( NUM_ARGD )    = xNINT( NUM_ARGD )

      NUM_ITOW( NUM_ARGI )    = NUM_ARGI

      NUM_RTOW( NUM_ARGR )    = xNINT( NUM_ARGR )

      NUM_WTOW( NUM_ARGW )    = NUM_ARGW

      NUM_UWTOW( NUM_ARGUW )  = NUM1_UWTOI( NUM_ARGUW )


*  Functions resulting in UNSIGNED BYTE.
*  ====================================

      NUM_WTOUB( NUM_ARGW )   = NUM1_WTOUB( NUM_ARGW )

      NUM_BTOUB( NUM_ARGB )   =
     :                         NUM_WTOUB( NUM_BTOW( NUM_ARGB ) )

      NUM_UBTOUB( NUM_ARGUB ) = NUM_ARGUB

      NUM_DTOUB( NUM_ARGD )   =
     :                         NUM_WTOUB( NUM_DTOW( NUM_ARGD ) )

      NUM_ITOUB( NUM_ARGI )   =
     :                         NUM_WTOUB( NUM_ITOW( NUM_ARGI ) )

      NUM_RTOUB( NUM_ARGR )   =
     :                         NUM_WTOUB( NUM_RTOW( NUM_ARGR ) )

      NUM_UWTOUB( NUM_ARGUW ) =
     :                         NUM_WTOUB( NUM_UWTOW( NUM_ARGUW ) )


*  Functions resulting in UNSIGNED WORD.
*  ====================================

      NUM_ITOUW( NUM_ARGI )   = NUM1_ITOUW( NUM_ARGI )

      NUM_BTOUW( NUM_ARGB )   =
     :                         NUM_ITOUW( NUM_BTOI( NUM_ARGB ) )

      NUM_UBTOUW( NUM_ARGUB ) = NUM1_UBTOUW( NUM_ARGUB )

      NUM_DTOUW( NUM_ARGD )   =
     :                         NUM_ITOUW( NUM_DTOI( NUM_ARGD ) )

      NUM_RTOUW( NUM_ARGR )   =
     :                         NUM_ITOUW( NUM_RTOI( NUM_ARGR ) )

      NUM_WTOUW( NUM_ARGW )   =
     :                         NUM_ITOUW( NUM_WTOI( NUM_ARGW ) )

      NUM_UWTOUW( NUM_ARGUW ) = NUM_ARGUW

*.
