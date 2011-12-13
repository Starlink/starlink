      SUBROUTINE SUBPAR_CURVAL ( NAMECODE, STRING, STATUS )
*+
*  Name:
*     SUBPAR_CURVAL

*  Purpose:
*     Get the current value of a parameter as a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CURVAL ( NAMECODE, STRING, STATUS )

*  Description:
*     Given the index of a program parameter, try to get a locator to
*     its associated 'private' HDS storage. If this is
*     successful, get the value as a character string.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     STRING=CHARACTER*(*) (returned)
*        value of parameter as a string
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     The top-level locator to 'private' storage is obtained from
*     COMMON. A locator is requested for the named component.
*     If this is successful, if the value is a structure name return it,
*     otherwise get the value and convert it to a string, taking account
*     of array values.

*  Copyright:
*     Copyright (C) 1987, 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 1997 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08-MAY-1987 (BDK):
*        Original
*     20-JUL-1987 (BDK):
*        new syntax, prefix HDSnames by @
*     12-AUG-1987 (BDK):
*        surround strings with quotes
*     01-DEC-1988 (AJC):
*        improve to character conversion
*        Temporary mod till DAT_GET0C is better
*     11-JAN-1988 (AJC):
*        Avoid annulling locator if not obtained -
*        it corrupts HDS
*     25-JUL-1991 (AJC):
*        check existence of component first
*     27-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*     10-NOV-1992 (AJC):
*        Use SUBPAR__ERROR not PAR__
*        and report error
*     10-FEB-1993 (AJC):
*        Correct handling array of _DOUBLE
*        Prevent array overflow if > 20 elements
*        Add deficiency comments
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     12-JUL-1993 (AJC):
*        Size CARRAY*20 to overcome CHR/Sun-Fortran rounding bug
*      8-AUG-1994 (AJC):
*        Correct messages - CURLOC to CURVAL
*     27-NOV-1996 (AJC):
*        Use SUBPAR_ENQUOTE to quote strings
*     31-OCT-1997 (AJC):
*        Allow 100 array elements (was 20)
*      3-DEC-1997 (AJC):
*        CARRAY element size 20 -> 132
*        CHR_CLEAN strings
*     {enter_further_changes_here}

*  Deficiencies:
*     Cannot handle objects of > 100 elements or >132 character elements
*     Also consider using HDS data conversion throughout

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      INTEGER NAMECODE             ! Number of program parameter

*  Arguments Returned:
      CHARACTER*(*) STRING         ! value of parameter as a string

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  External routines :
      INTEGER CHR_LEN               ! used length of string

*  Local Variables:
      CHARACTER*(DAT__SZLOC) BOTLOC ! HDS locator (temporary)
      CHARACTER*(DAT__SZLOC) LOC    ! Locator to data structure
      CHARACTER*15 HDSTYPE          ! type of named object
      LOGICAL PRIM                  ! .TRUE. => primitive object
      LOGICAL LVAL(100)             ! logical values
      LOGICAL THERE                 ! existence of component
      INTEGER ACTDIM                ! number of dimensions
      INTEGER DIMS(7)               ! sizes of dimensions
      CHARACTER*132 CARRAY(100)     ! array for character values
      INTEGER LENGTH                ! length of character values
      INTEGER SIZE                  ! number of array elements
      INTEGER J                     ! counter for array elements
      INTEGER NCHAR                 ! used length of string
      INTEGER IVALUE                ! temp holder for conversion
      INTEGER IARRAY(100)           ! temp holder for conversion
      REAL RVALUE                   ! temp holder for conversion
      REAL RARRAY(100)              ! temp holder for conversion
      DOUBLE PRECISION DVALUE       ! temp holder for conversion
      DOUBLE PRECISION DARRAY(100)  ! temp holder for conversion
      CHARACTER*(SUBPAR__STRLEN) CVALUE  ! temp holder for conversion
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check for a component in 'private' storage with the same name as the
*   parameter.
      CALL DAT_THERE ( EXTLOC, PARNAMES(NAMECODE), THERE, STATUS )

*   If present, get a locator to it
      IF ( ( STATUS .EQ. SAI__OK ) .AND.  THERE ) THEN

         CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )

*      Find what kind of an object has been located. If it is a primitive,
*      then it is the one required. If it is a structure, then it is
*      the structure name required.
         CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
         CALL DAT_PRIM ( BOTLOC, PRIM, STATUS )

         IF ( .NOT. PRIM ) THEN

*         The value is a structure. If it is a pointer to
*         another structure, find the real structure name required.
            IF ( HDSTYPE .EQ. 'ADAM_PARNAME' ) THEN

               STRING(1:1) = '@'
               CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
               CALL DAT_GETC ( LOC, 0, 0, STRING(2:), STATUS )
               CALL DAT_ANNUL ( LOC, STATUS )

            ELSE

*           Impossible type - force continuation
               STATUS = SUBPAR__ERROR
               CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
               CALL EMS_REP ( 'SUP_CURVAL1',
     :         'SUBPAR_CURVAL: Parameter ^PARAM - ' //
     :         'Illegal parameter file object', STATUS )


            ENDIF

         ELSE

*         The actual value is here. Get its dimensions.
            CALL DAT_SHAPE ( BOTLOC, 7, DIMS, ACTDIM, STATUS )
            IF ( ACTDIM .EQ. 0 ) THEN

*            A scalar. Get its value.
               IF ( HDSTYPE .EQ. '_LOGICAL' ) THEN
                  CALL DAT_GETL ( BOTLOC, 0, 0, LVAL(1), STATUS )
                  IF ( LVAL(1) ) THEN
                     STRING = 'TRUE'
                  ELSE
                     STRING = 'FALSE'
                  ENDIF
               ELSE IF ( HDSTYPE(1:5) .EQ. '_CHAR' ) THEN
                  CALL DAT_GETC ( BOTLOC, 0, 0, CVALUE, STATUS )
                  LENGTH = CHR_LEN( CVALUE )
                  CALL SUBPAR_ENQUOTE(
     :               CVALUE(1:LENGTH), STRING, LENGTH, STATUS )
                  IF( STATUS .NE. SAI__OK ) THEN
                     STATUS = SUBPAR__ERROR
                     CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
                     CALL EMS_REP ( 'SUP_CURVAL1',
     :               'SUBPAR_CURVAL: Parameter ^PARAM - ' //
     :               'object exceeds buffer size',
     :                STATUS )
                  ENDIF
               ELSE IF ( HDSTYPE(1:8) .EQ. '_INTEGER') THEN
                  CALL DAT_GETI ( BOTLOC, 0, 0, IVALUE, STATUS )
                  CALL CHR_ITOC ( IVALUE, STRING, NCHAR )
               ELSE IF ( HDSTYPE(1:5) .EQ. '_REAL') THEN
                  CALL DAT_GETR ( BOTLOC, 0, 0, RVALUE, STATUS )
                  CALL CHR_RTOC ( RVALUE, STRING, NCHAR )
               ELSE
                  CALL DAT_GETD ( BOTLOC, 0, 0, DVALUE, STATUS )
                  CALL CHR_DTOC ( DVALUE, STRING, NCHAR)
               ENDIF
            ELSE

*            Get an array of strings and concatenate them.
*            Note, the dimensionality of CARRAY is being lied about.
               CALL DAT_SIZE ( BOTLOC, SIZE, STATUS )
               IF ( SIZE .LE. 100 ) THEN
                  IF ( HDSTYPE .EQ. '_LOGICAL' ) THEN
                     CALL DAT_GETL ( BOTLOC, ACTDIM, DIMS, LVAL,
     :                               STATUS )
                     DO J = 1, SIZE
                        IF ( LVAL(J) ) THEN
                           CARRAY(J) = 'TRUE'
                        ELSE
                           CARRAY(J) = 'FALSE'
                        ENDIF
                     ENDDO
                  ELSE IF ( HDSTYPE(1:5) .EQ. '_CHAR' ) THEN
                     CALL DAT_GETC ( BOTLOC, ACTDIM, DIMS, CARRAY,
     :                               STATUS )
                     DO J = 1, SIZE
                        CVALUE = CARRAY(J)
                        CALL CHR_CLEAN( CVALUE )
                        LENGTH = CHR_LEN( CVALUE )
                        CALL SUBPAR_ENQUOTE(
     :                   CVALUE(1:LENGTH), CARRAY(J), LENGTH, STATUS )
                        IF( STATUS .NE. SAI__OK ) THEN
                           STATUS = SUBPAR__ERROR
                           CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
                           CALL EMS_REP ( 'SUP_CURVAL1',
     :                     'SUBPAR_CURVAL: Parameter ^PARAM - ' //
     :                     'object exceeds buffer size',
     :                     STATUS )
                        ENDIF
                     ENDDO
                  ELSE IF ( HDSTYPE(1:8) .EQ. '_INTEGER') THEN
                     CALL DAT_GETI ( BOTLOC, ACTDIM, DIMS, IARRAY,
     :                               STATUS )
                     DO J = 1, SIZE
                        CALL CHR_ITOC ( IARRAY(J), CARRAY(J), NCHAR )
                     ENDDO
                  ELSE IF ( HDSTYPE(1:5) .EQ. '_REAL') THEN
                     CALL DAT_GETR ( BOTLOC, ACTDIM, DIMS, RARRAY,
     :                               STATUS )
                     DO J = 1, SIZE
                        CALL CHR_RTOC ( RARRAY(J), CARRAY(J), NCHAR )
                     ENDDO
                  ELSE
                     CALL DAT_GETD ( BOTLOC, ACTDIM, DIMS, DARRAY,
     :                               STATUS )
                     DO J = 1, SIZE
                        CALL CHR_DTOC ( DARRAY(J), CARRAY(J), NCHAR)
                     ENDDO
                  ENDIF

*               Concatenate
                  CALL STRING_BUILDARR ( ACTDIM, DIMS, CARRAY, STRING,
     :              STATUS )

               ELSE
*               More than 100 elements - cannot be handled
                  STATUS = SUBPAR__ERROR
                  CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
                  CALL EMS_REP ( 'SUP_CURVAL1',
     :            'SUBPAR_CURVAL: Parameter ^PARAM - ' //
     :            'object > 100 elements, exceeds buffer size',
     :             STATUS )
               ENDIF

            ENDIF

         ENDIF

      CALL DAT_ANNUL ( BOTLOC, STATUS )

      ELSEIF ( STATUS .EQ. SAI__OK ) THEN
*  Component wasn't there - flag it to calling routine
         STATUS = SUBPAR__ERROR
         CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
         CALL EMS_REP ( 'SUP_CURVAL2',
     :   'SUBPAR: No "current" value for ^PARAM', STATUS )

      ENDIF

      END
