      SUBROUTINE SUBPAR_VALASS ( NAMECODE, STRING, STATUS )
*+
*  Name:
*     SUBPAR_VALASS

*  Purpose:
*     Get the associated value of a parameter as a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_VALASS ( NAMECODE, STRING, STATUS )

*  Description:
*     Given the index of a program parameter, try to get a locator to
*     its associated global variable. If this is
*     successful, get the value as a character string.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     STRING=CHARACTER*(*) (returned)
*        value of global parameter as a string
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     A locator to the global parameter storage associated with this
*     parameter is obtained.
*     If this is successful, if the value is a structure name return it,
*     otherwise get the value and convert it to a string, taking account
*     of array values.

*  Copyright:
*     Copyright (C) 1988, 1990, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     23-MAY-1988 (AJC):
*        Original
*     05-DEC-1988 (AJC):
*        Improve to-character conversion
*        Temporary mod till DAT_GET0C is better
*     01-FEB-1990 (AJC):
*        Guard against hanging locator problem
*     06-APR-1992 (AJC):
*        translate ADAM_USER in GLOBAL name
*     11-NOV-1992 (AJC):
*        Use SUBPAR__ERROR not PAR__
*        and report error
*     10-FEB-1993 (AJC):
*        Correct handling array of _DOUBLE
*        Prevent array overflow if > 20 elements
*        Add deficiency comments
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      9-AUG-1993 (AJC):
*        Size CARRAY*20 to overcome CHR/Sun-Fortran rounding bug
*     27-NOV-1996 (AJC):
*        Use SUBPAR_ENQUOTE to quote strings
*     {enter_further_changes_here}

*  Deficiencies:
*     Cannot handle objects of > 20 elements - just ignores them
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

*    External routines :
      INTEGER CHR_LEN              ! used length of string

*  Local Variables:
      CHARACTER*200 GLONAM                 ! expanded GLOBAL association name
      CHARACTER*(DAT__SZLOC) ASSLOC        ! HDS locator (temporary)
      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)
      CHARACTER*(DAT__SZLOC) LOC   ! Locator to data structure
      CHARACTER*15 HDSTYPE         ! type of named object
      INTEGER AULEN                  ! length of ADAM_USER translation
      LOGICAL PRIM                 ! .TRUE. => primitive object
      LOGICAL LVAL(20)             ! logical values
      INTEGER ACTDIM               ! number of dimensions
      INTEGER DIMS(7)              ! sizes of dimensions
      CHARACTER*20 CARRAY(20)      ! array for character values
      INTEGER LENGTH               ! length of character values
      INTEGER SIZE                 ! number of array elements
      INTEGER J                    ! counter for array elements
      INTEGER ISTAT                ! temporary status
      INTEGER NCHAR                ! used length of string
      INTEGER IVALUE               ! temp holder for conversion
      INTEGER IARRAY(20)           ! temp holder for conversion
      REAL RVALUE                  ! temp holder for conversion
      REAL RARRAY(20)              ! temp holder for conversion
      DOUBLE PRECISION DVALUE      ! temp holder for conversion
      DOUBLE PRECISION DARRAY(20)  ! temp holder for conversion
      CHARACTER*(SUBPAR__STRLEN) CVALUE  ! temp holder for conversion
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get locator to stored association for this parameter.
*   _HDSLOCS will nullify locators if it fails
*
*   First expand ADAM_USER.
      IF ( CHARLIST(PARASSOC(1,NAMECODE))(1:10) .EQ. 'ADAM_USER:' )
     : THEN
         CALL SUBPAR_ADMUS( GLONAM, AULEN, STATUS )
         GLONAM(AULEN+1:) = CHARLIST(PARASSOC(1,NAMECODE))(11:)
      ELSE
         GLONAM = CHARLIST(PARASSOC(1,NAMECODE))
      ENDIF

      CALL SUBPAR_HDSLOCS ( GLONAM, 'READ', ASSLOC, BOTLOC, STATUS )

*   Find what kind of an object has been located. If it is a primitive,
*   then it is the one required. If it is a structure, then it is
*   the structure name required.
      HDSTYPE = ' '
      CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
      PRIM = .FALSE.
      CALL DAT_PRIM ( BOTLOC, PRIM, STATUS )

      IF ( .NOT. PRIM ) THEN

*      The value is a structure. If it is a pointer to
*      another structure, find the real structure name required.
         IF ( HDSTYPE .EQ. 'ADAM_PARNAME' ) THEN

            STRING(1:1) = '@'
            LOC = ' '
            CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
            CALL DAT_GETC ( LOC, 0, 0, STRING(2:), STATUS )
            CALL DAT_ANNUL ( LOC, STATUS )

         ELSE

            STATUS = SUBPAR__ERROR
            CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
            CALL EMS_REP ( 'SUP_VALASS1',
     :      'SUBPAR_VALASS: Parameter ^PARAM - ' //
     :      'Illegal associated global file object', STATUS )

         ENDIF

      ELSE

*      The actual value is here. Get its dimensions.
         CALL DAT_SHAPE ( BOTLOC, 7, DIMS, ACTDIM, STATUS )

         IF ( ACTDIM .EQ. 0 ) THEN

*         A scalar. Get its value.
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
     :            CVALUE(1:LENGTH), STRING, LENGTH, STATUS )
               IF( STATUS .NE. SAI__OK ) THEN
                  STATUS = SUBPAR__ERROR
                  CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
                  CALL EMS_REP ( 'SUP_VALASS1',
     :            'SUBPAR_VALASS: Parameter ^PARAM - ' //
     :            'object exceeds buffer size',
     :             STATUS )
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

*         Get an array of strings and concatenate them.
*         Note, the dimensionality of CARRAY is being lied about.
            CALL DAT_SIZE ( BOTLOC, SIZE, STATUS )
            IF ( SIZE .LE. 20 ) THEN
               IF ( HDSTYPE .EQ. '_LOGICAL' ) THEN
                  CALL DAT_GETL ( BOTLOC, ACTDIM, DIMS, LVAL,
     :                            STATUS )
                  DO J = 1, SIZE
                     IF ( LVAL(J) ) THEN
                        CARRAY(J) = 'TRUE'
                     ELSE
                        CARRAY(J) = 'FALSE'
                     ENDIF
                  ENDDO
               ELSE IF ( HDSTYPE(1:5) .EQ. '_CHAR' ) THEN
                  CALL DAT_GETC ( BOTLOC, ACTDIM, DIMS, CARRAY,
     :                            STATUS )
                  DO J = 1, SIZE
                     CVALUE = CARRAY(J)
                     LENGTH = CHR_LEN( CVALUE )
                     CALL SUBPAR_ENQUOTE(
     :                CVALUE(1:LENGTH), CARRAY(J), LENGTH, STATUS )
                     IF( STATUS .NE. SAI__OK ) THEN
                        STATUS = SUBPAR__ERROR
                        CALL EMS_SETC ( 'PARAM', PARNAMES(NAMECODE) )
                        CALL EMS_REP ( 'SUP_VALASS1',
     :                  'SUBPAR_VALASS: Parameter ^PARAM - ' //
     :                  'object exceeds buffer size',
     :                  STATUS )
                     ENDIF
                  ENDDO
               ELSE IF ( HDSTYPE(1:8) .EQ. '_INTEGER') THEN
                  CALL DAT_GETI ( BOTLOC, ACTDIM, DIMS, IARRAY,
     :                            STATUS )
                  DO J = 1, SIZE
                     CALL CHR_ITOC ( IARRAY(J), CARRAY(J), NCHAR )
                  ENDDO
               ELSE IF ( HDSTYPE(1:5) .EQ. '_REAL') THEN
                  CALL DAT_GETR ( BOTLOC, ACTDIM, DIMS, RARRAY,
     :                            STATUS )
                  DO J = 1, SIZE
                     CALL CHR_RTOC ( RARRAY(J), CARRAY(J), NCHAR )
                  ENDDO
               ELSE
                  CALL DAT_GETD ( BOTLOC, ACTDIM, DIMS, DARRAY,
     :                            STATUS )
                  DO J = 1, SIZE
                     CALL CHR_DTOC ( DARRAY(J), CARRAY(J), NCHAR)
                  ENDDO
               ENDIF

*            Concatenate
               CALL STRING_BUILDARR ( ACTDIM, DIMS, CARRAY, STRING,
     :           STATUS )

            ELSE
*            More than 20 elements - cannot be handled
               STATUS = SUBPAR__ERROR
               CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
               CALL EMS_REP( 'SUP_VALASS2',
     :         'SUBPAR_VALASS: Parameter ^PARAM - ' //
     :         'global file object > 20 elements', STATUS )

            ENDIF

         ENDIF

      ENDIF

      CALL DAT_ANNUL ( BOTLOC, STATUS )

*    Close the container file containing the associated value
      ISTAT = SAI__OK
      CALL HDS_CLOSE ( ASSLOC, ISTAT )

      END
