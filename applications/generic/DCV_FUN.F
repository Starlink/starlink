#if HAVE_CONFIG_H
#  include "config.h"
#endif
*+
*  Name:
*     DCV_FUN

*  Purpose:
*     Statement function definitions for data type inter-conversion

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INCLUDE 'DCV_FUN'

*  Description:
*     This INCLUDE file contains definitions of in-line
*     functions of type DCV_<T1>TO<T2>, which may be used for
*     converting from one data type into another.
*
*     The following conversions are fully supported. (They are either
*     trivial, or are mentioned in the Fortran manual as legal
*     conversions) :-
*
*     Function name          Conversion
*     -------------          ----------
*
*     DCV_BTOB               BYTE to BYTE
*     DCV_UBTOUB             UNSIGNED BYTE to UNSIGNED BYTE
*     DCV_UBTOD              UNSIGNED BYTE to DOUBLE PRECISION
*     DCV_DTOD               DOUBLE PRECISION to DOUBLE PRECISION
*     DCV_ITOD               INTEGER to DOUBLE PRECISION
*     DCV_RTOD               REAL to DOUBLE PRECISION
*     DCV_WTOD               WORD to DOUBLE PRECISION
*     DCV_UWTOD              UNSIGNED WORD to DOUBLE PRECISION
*     DCV_UBTOI              UNSIGNED BYTE to INTEGER
*     DCV_DTOI               DOUBLE PRECISION to INTEGER
*     DCV_ITOI               INTEGER to INTEGER
*     DCV_RTOI               REAL to INTEGER
*     DCV_UWTOI              UNSIGNED WORD to INTEGER
*     DCV_UBTOR              UNSIGNED BYTE to REAL
*     DCV_DTOR               DOUBLE PRECISION to REAL
*     DCV_ITOR               INTEGER to REAL
*     DCV_RTOR               REAL to REAL
*     DCV_WTOR               WORD to REAL
*     DCV_UWTOR              UNSIGNED WORD to REAL
*     DCV_UBTOW              UNSIGNED BYTE to WORD
*     DCV_DTOW               DOUBLE PRECISION to WORD
*     DCV_RTOW               REAL to WORD
*     DCV_WTOW               WORD to WORD
*     DCV_UBTOUW             UNSIGNED BYTE to UNSIGNED WORD
*     DCV_UWTOUW             UNSIGNED WORD to UNSIGNED WORD
*
*     The following conversions are partially supported only, and should
*     be used with caution. (Trial and error has shown that VAX Fortran
*     will make the conversions successfully, but they are not mentioned
*     in the Fortran manual) :-
*
*     Function name          Conversion
*     -------------          ----------
*
*     DCV_UBTOB              UNSIGNED BYTE to BYTE
*     DCV_DTOB               DOUBLE PRECISION to BYTE
*     DCV_ITOB               INTEGER to BYTE
*     DCV_RTOB               REAL to BYTE
*     DCV_WTOB               WORD to BYTE
*     DCV_UWTOB              UNSIGNED WORD to BYTE
*     DCV_BTOD               BYTE to DOUBLE PRECISION
*     DCV_BTOI               BYTE to INTEGER
*     DCV_WTOI               WORD to INTEGER
*     DCV_BTOR               BYTE to REAL
*     DCV_BTOW               BYTE to WORD
*     DCV_ITOW               INTEGER to WORD
*
*     The following conversions are NOT defined here as statement
*     functions. (They are defined as external functions and may be
*     obtained by linking with the DCV library) :-
*
*     Function name          Conversion
*     -------------          ----------
*     DCV_BTOUB              BYTE to UNSIGNED BYTE
*     DCV_DTOUB              DOUBLE PRECISION to UNSIGNED BYTE
*     DCV_ITOUB              INTEGER to UNSIGNED BYTE
*     DCV_RTOUB              REAL to UNSIGNED BYTE
*     DCV_WTOUB              WORD to UNSIGNED BYTE
*     DCV_UWTOUB             UNSIGNED WORD to UNSIGNED BYTE
*     DCV_BTOUW              BYTE to UNSIGNED WORD
*     DCV_UBTOUW             UNSIGNED BYTE to UNSIGNED WORD
*     DCV_DTOUW              DOUBLE PRECISION to UNSIGNED WORD
*     DCV_ITOUW              INTEGER to UNSIGNED WORD
*     DCV_RTOUW              REAL to UNSIGNED WORD
*     DCV_WTOUW              WORD to UNSIGNED WORD

*  Notes:
*     -  DCV_ can only be used for NUMERIC conversions.
*     The CHR_ library contains routines for conversion to and from
*     CHARACTER and numeric data types.

*  Implementation Deficiencies:
*     - Fortran does not provide all the intrinsic functions
*     necessary for converting between the various data types.
*     This file declares the functions which are supported
*     in the VAX/VMS Fortran manual. Some conversions are
*     declared for which there is no conversion function, but
*     for which VAX Fortran is found to behave sensibly.
*     These latter conversions are labelled "???", and
*     care should be taken with these. Dangerous or unpredictable
*     conversions are mentioned, but are commented out and not
*     defined as statement functions. These latter conversions
*     are defined as external functions, and are found in the
*     DCV object library.
*     -  REAL*16 and COMPLEX types are not included here, but may be
*     added if required.

*  Machine-specific features used:
*     Many of the conversions defined in this file (especially
*     those which use the ZEXT function) are specific to VAX
*     Fortran.) This file should therefore be replaced if software
*     is developed on a non-VAX machine. Defining a logical name
*     'DCV_FUN' to point to this file should make this task easier.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     SMB: Steven Beard (ROE)
*     PMA: Peter Allan (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-MAR-1987 (SMB):
*        Original version.
*     16-MAR-1987 (SMB):
*        Renamed to DCV.FUN. Facility name DCV included. Also some bug
*        fixes.
*     17-MAR-1987 (SMB):
*        Unreliable functions removed.
*     03-APR-1987 (SMB):
*        Documentation brought up to date.
*     09-APR-1987 (SMB):
*        External references transferred to DCV.EXT
*     12-JAN-1993 (PMA):
*        Converted prologue to ADAM style.
*     21-AUG-2005 (TIMJ):
*        Autoconf for g95. IAND can not take typeless BOZ constants
*     25-FEB-2007 (PWD):
*        Add non-BOZ parameters.
*     {enter_further_changes_here}

*-

*  Type Definitions:

*  Declare the types of the dummy arguments

      BYTE DUMMY_B               ! BYTE argument
      BYTE DUMMY_UB              ! UNSIGNED BYTE argument
      DOUBLE PRECISION DUMMY_D   ! DOUBLE PRECISION (REAL*8) argument
      INTEGER DUMMY_I            ! INTEGER*4 argument
      REAL DUMMY_R               ! REAL*4 argument
      INTEGER*2 DUMMY_W          ! WORD (INTEGER*2) argument
      INTEGER*2 DUMMY_UW         ! UNSIGNED WORD argument

#if !HAVE_INTRINSIC_ZEXT
*  Declare the variables needed to enforce type compatibility for systems
*  that lack ZEXT. Some compilers insist that IAND arguments are the same type
      BYTE      DCV_UBMASK
      INTEGER*2 DCV_UWMASK

# if HAVE_TYPELESS_BOZ
*  Can not used typed BOZ here because some compilers do not like
*  it when FF is used in a signed type
      PARAMETER (DCV_UBMASK = X'000000FF' )
      PARAMETER (DCV_UWMASK = X'0000FFFF' )
# else
#   if HAVE_OLD_TYPELESS_BOZ
      PARAMETER (DCV_UBMASK = '000000FF'X )
      PARAMETER (DCV_UWMASK = '0000FFFF'X )
#   else
*  No BOZ support so use simple assignments. These may require compiler
*  flag support to allow overflow (-fno-range-check for gfortran).
      PARAMETER (DCV_UBMASK = 255)
      PARAMETER (DCV_UWMASK = 65535)
#   endif
# endif

/* !HAVE_INTRINSIC_ZEXT */
#endif

*   Declare the types of the functions

      BYTE DCV_BTOB
      BYTE DCV_UBTOB
      BYTE DCV_DTOB
      BYTE DCV_ITOB
      BYTE DCV_RTOB
      BYTE DCV_UWTOB
      BYTE DCV_WTOB

      BYTE DCV_UBTOUB

      DOUBLE PRECISION DCV_BTOD
      DOUBLE PRECISION DCV_UBTOD
      DOUBLE PRECISION DCV_DTOD
      DOUBLE PRECISION DCV_ITOD
      DOUBLE PRECISION DCV_RTOD
      DOUBLE PRECISION DCV_WTOD
      DOUBLE PRECISION DCV_UWTOD

      INTEGER DCV_BTOI
      INTEGER DCV_UBTOI
      INTEGER DCV_DTOI
      INTEGER DCV_ITOI
      INTEGER DCV_RTOI
      INTEGER DCV_WTOI
      INTEGER DCV_UWTOI

      REAL DCV_BTOR
      REAL DCV_UBTOR
      REAL DCV_DTOR
      REAL DCV_ITOR
      REAL DCV_RTOR
      REAL DCV_WTOR
      REAL DCV_UWTOR

      INTEGER*2 DCV_BTOW
      INTEGER*2 DCV_UBTOW
      INTEGER*2 DCV_DTOW
      INTEGER*2 DCV_ITOW
      INTEGER*2 DCV_RTOW
      INTEGER*2 DCV_WTOW
      INTEGER*2 DCV_UWTOW

      INTEGER*2 DCV_UWTOUW

*   Declare the DCV_<T1>TO<T2> functions :-

*   (1) Routines resulting in BYTE

      DCV_BTOB( DUMMY_B )    = DUMMY_B
#if HAVE_INTRINSIC_ZEXT
      DCV_UBTOB( DUMMY_UB )  = ZEXT( DUMMY_UB )              ! ???
#else
      DCV_UBTOB( DUMMY_UB )  = DUMMY_UB
#endif
      DCV_DTOB( DUMMY_D )    = NINT( DUMMY_D )               ! ???
      DCV_ITOB( DUMMY_I )    = DUMMY_I                       ! ???
      DCV_RTOB( DUMMY_R )    = NINT( DUMMY_R )               ! ???
      DCV_WTOB( DUMMY_W )    = DUMMY_W                       ! ???
#if HAVE_INTRINSIC_ZEXT
      DCV_UWTOB( DUMMY_UW )  = ZEXT( DUMMY_UW )              ! ???
#else
      DCV_UWTOB( DUMMY_UW )  = DUMMY_UW                      ! ???
#endif

*   (2) Routines resulting in UNSIGNED BYTE

*     DCV_BTOUB( DUMMY_B )   external function
      DCV_UBTOUB( DUMMY_UB ) = DUMMY_UB
*     DCV_DTOUB( DUMMY_D )   external function
*     DCV_ITOUB( DUMMY_I )   external function
*     DCV_RTOUB( DUMMY_R )   external function
*     DCV_WTOUB( DUMMY_W )   external function
*     DCV_UWTOUB( DUMMY_UW ) external function

*   (3) Routines resulting in DOUBLE PRECISION

      DCV_BTOD( DUMMY_B )    = DBLE( DUMMY_B )               ! ???
#if HAVE_INTRINSIC_ZEXT
      DCV_UBTOD( DUMMY_UB )  = DBLE( ZEXT( DUMMY_UB ) )
#else
      DCV_UBTOD( DUMMY_UB )  = DBLE( IAND( DUMMY_UB , DCV_UBMASK ) )
#endif
      DCV_DTOD( DUMMY_D )    = DUMMY_D
      DCV_ITOD( DUMMY_I )    = DBLE( DUMMY_I )
      DCV_RTOD( DUMMY_R )    = DBLE( DUMMY_R )
      DCV_WTOD( DUMMY_W )    = DBLE( DUMMY_W )
#if HAVE_INTRINSIC_ZEXT
      DCV_UWTOD( DUMMY_UW )  = DBLE( ZEXT( DUMMY_UW ) )
#else
      DCV_UWTOD( DUMMY_UW )  = DBLE( IAND( DUMMY_UW , DCV_UWMASK ) )
#endif

*   (4) Routines resulting in INTEGER

      DCV_BTOI( DUMMY_B )    = DUMMY_B                       ! ???
#if HAVE_INTRINSIC_ZEXT
      DCV_UBTOI( DUMMY_UB )  = ZEXT( DUMMY_UB )
#else
      DCV_UBTOI( DUMMY_UB )  = IAND( DUMMY_UB , DCV_UBMASK )
#endif
      DCV_DTOI( DUMMY_D )    = NINT( DUMMY_D )
      DCV_ITOI( DUMMY_I )    = DUMMY_I
      DCV_RTOI( DUMMY_R )    = NINT( DUMMY_R )
      DCV_WTOI( DUMMY_W )    = DUMMY_W                       ! ???
#if HAVE_INTRINSIC_ZEXT
      DCV_UWTOI( DUMMY_UW )  = ZEXT( DUMMY_UW )
#else
      DCV_UWTOI( DUMMY_UW )  = IAND( DUMMY_UW , DCV_UWMASK )
#endif

*   (5) Routines resulting in REAL

      DCV_BTOR( DUMMY_B )    = REAL( DUMMY_B )               ! ???
#if HAVE_INTRINSIC_ZEXT
      DCV_UBTOR( DUMMY_UB )  = REAL( ZEXT( DUMMY_UB ) )
#else
      DCV_UBTOR( DUMMY_UB )  = REAL( IAND( DUMMY_UB , DCV_UBMASK ) )
#endif
      DCV_DTOR( DUMMY_D )    = SNGL( DUMMY_D )
      DCV_ITOR( DUMMY_I )    = REAL( DUMMY_I )
      DCV_RTOR( DUMMY_R )    = DUMMY_R
      DCV_WTOR( DUMMY_W )    = REAL( DUMMY_W )
#if HAVE_INTRINSIC_ZEXT
      DCV_UWTOR( DUMMY_UW )  = REAL( ZEXT( DUMMY_UW ) )
#else
      DCV_UWTOR( DUMMY_UW )  = REAL( IAND( DUMMY_UW , DCV_UWMASK) )
#endif

*   (6) Routines resulting in WORD

      DCV_BTOW( DUMMY_B )    = DUMMY_B                       ! ???
#if HAVE_INTRINSIC_ZEXT
      DCV_UBTOW( DUMMY_UB )  = ZEXT( DUMMY_UB )
#else
      DCV_UBTOW( DUMMY_UB )  = IAND( DUMMY_UB , DCV_UBMASK )
#endif
      DCV_DTOW( DUMMY_D )    = NINT( DUMMY_D )
      DCV_ITOW( DUMMY_I )    = DUMMY_I                       ! ???
      DCV_RTOW( DUMMY_R )    = NINT( DUMMY_R )
      DCV_WTOW( DUMMY_W )    = DUMMY_W
#if HAVE_INTRINSIC_ZEXT
      DCV_UWTOW( DUMMY_UW )  = ZEXT( DUMMY_UW )
#else
      DCV_UWTOW( DUMMY_UW )  = IAND( DUMMY_UW , DCV_UWMASK )
#endif

*   (7) Routines resulting in UNSIGNED WORD

*     DCV_BTOUW( DUMMY_B )   external function
*     DCV_UBTOUW( DUMMY_UB ) external function
*     DCV_DTOUW( DUMMY_D )   external function
*     DCV_ITOUW( DUMMY_I )   external function
*     DCV_RTOUW( DUMMY_R )   external function
*     DCV_WTOUW( DUMMY_W )   external function
      DCV_UWTOUW( DUMMY_UW ) = DUMMY_UW

*-
