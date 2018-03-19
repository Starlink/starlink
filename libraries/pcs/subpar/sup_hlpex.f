    	SUBROUTINE SUBPAR_HLPEX( LIBRARY, EXNAME, ELEN, STATUS )
*+
*  Name:
*     SUBPAR_HLPEX

*  Purpose:
*     Expands the help library name in accordance with the ADAM rules

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HLPEX( LIBRARY, EXNAME, ELEN, STATUS )

*  Description:
*     The routine does any required expansion of logical names in the LIBRARY
*     specification.
*
*     The LIBRARY name must be in a form acceptable to to platform on which
*     the program is runnung except that, in the interests of allowing common
*     code, LIBRARY may take the form $ENV/name or ENV:name, where ENV is a
*     logical-name/environment-variable. Either the ENV component or the name
*     component may be omitted.
*
*     Environment variables in the VMS form (ENV:) will be forced to upper
*     case for translation and any 'name' component to lower case; the Unix
*     form will be translated as given.
*
*     The routine will do nothing if STATUS is given as not SAI__OK.
*
*  Deficiencies:
*     1.Assumptions about syntax of filenames are made - VMS and Unix forms
*       are catered for.
*     2.Lower case logical names may not be used on VMS - the given name is
*       converted to upper case for translation
*  Arguments:
*     LIBRARY = CHARACTER*(*) (Given)
*        The help library name
*        It may take the form: NAME,
*                              LOG:NAME,
*                          or  $LOG/NAME
*        where NAME is a filename optionally including a path (directory spec)
*                   and filetype.
*          and LOG  is an environment variable (logical name).
*        Note that the VMS system assumes SYSLIB if no directory is specified
*     EXNAME  = CHARACTER*(*) (Returned)
*        The fully expanded filename
*     ELEN = INTEGER (Returned)
*        The used length of EXNAME
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1992 (AJC):
*        Original version.
*     21-OCT-1992 (AJC):
*        Remove unwanted declarations
*     24-FEB-1994 (AJC):
*        Only force name to lower case if there's a VMS-type logical name
*     19-MAR-2018 (DSB):
*        Cater for long file names.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) LIBRARY

*  Arguments Returned:
      CHARACTER*(*) EXNAME
      INTEGER ELEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Local Variables:
      INTEGER LIBLEN             ! Used length of LIBRARY
      INTEGER SLN                ! Pointer to start of logical name
      INTEGER ELN                ! Pointer to end of logical name
      INTEGER STNM               ! Pointer to start of file name
      CHARACTER*1000 ULIB        ! LIBRARY in upper case
      CHARACTER*8 SYSNM          ! PSX_UNAME returns
      CHARACTER*8 NODENM         !    "      "
      CHARACTER*8 RELEASE        !    "      "
      CHARACTER*8 VERSION        !    "      "
      CHARACTER*8 MACHINE        !    "      "
      LOGICAL VMSLOG             ! Whether there's a VMS-type name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out which system we are on
      CALL PSX_UNAME( SYSNM, NODENM, RELEASE, VERSION, MACHINE, STATUS )

*  Find the length of the library specification
      LIBLEN = CHR_LEN( LIBRARY )
*  and store in a temporary buffer
      ULIB = LIBRARY(1:LIBLEN)

*  Check for VMS-style environment variables
      SLN = 1
      ELN = INDEX( ULIB(1:LIBLEN), ':' ) - 1

*     Set start of name depending on whether or not there was an
*     environment variable
      IF ( ELN .EQ. -1 ) THEN
*        There was no VMS-style logical -
         VMSLOG = .FALSE.
         STNM = 1

*        check for Unix-style environment variable
         IF ( ULIB(1:1) .EQ. '$' ) THEN
            SLN = 2
            ELN = INDEX( ULIB(1:LIBLEN), '/' ) - 1
*        Check for whole name is environment variable
*        and set start of name appropriately
            IF ( ELN .EQ. -1 ) THEN
               ELN = LIBLEN
               STNM = LIBLEN + 1
            ELSE
               STNM = ELN + 2
            ENDIF
*        If we are running on VMS, force the name to UPPER case
            IF ( SYSNM .EQ. 'VMS' ) THEN
               CALL CHR_UCASE( ULIB(SLN:ELN) )
            ENDIF

         ENDIF

      ELSE
*     There was a VMS-style logical name
         VMSLOG = .TRUE.
         STNM = ELN + 2
*     Force all VMS-style logical names to UPPER case
         CALL CHR_UCASE( ULIB(SLN:ELN) )

      ENDIF

*  Translate the logical name if found
      IF ( ELN .GE. SLN ) THEN
         CALL PSX_GETENV( ULIB(SLN:ELN), EXNAME, STATUS )
         IF ( STATUS .NE. SAI__OK) THEN
            CALL EMS_SETC( 'LNAM', ULIB(SLN:ELN) )
            CALL EMS_REP( 'SUP_HLPEX1',
     :      'SUBPAR: '//
     :      'Logical name ^LNAM used to specify help library is '//
     :      'undefined', STATUS )
            ELEN = 0

         ELSE
*        Find the length of the translation
            ELEN = CHR_LEN( EXNAME )
*        If it translated to another VMS logical, restore the first
*        covers disk units such as $1$DUA2: which won't translate again
            IF ( EXNAME(ELEN:ELEN) .EQ. ':' ) THEN
               EXNAME = ULIB(SLN:ELN)//':'
               ELEN = ELN - SLN + 2
            ENDIF

         ENDIF

      ELSE
         EXNAME = ' '
         ELEN = 0
      ENDIF

*  Extract the remaining filename portion, inserting / if not VMS
*  and environment variable present.
      IF ( STNM .LE. LIBLEN ) THEN
         IF ( ( SYSNM .NE. 'VMS' )
     :   .AND.( ELEN .NE. 0 ) ) THEN
            EXNAME(ELEN+1:ELEN+1) = '/'
            ELEN = ELEN + 1
         ENDIF
*     Furthermore, if the name included a VMS-style LOG: part, force the
*     'name' component to lower case.
*     Note that case is not significant when running on Unix.
         IF ( VMSLOG ) CALL CHR_LCASE ( ULIB(STNM:LIBLEN) )
         EXNAME(ELEN+1:) = ULIB(STNM:LIBLEN)
         ELEN = ELEN + LIBLEN - STNM + 1
      ENDIF

      END
