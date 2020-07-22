*        Add DAT_PAR for SUBPAR_CMN
      SUBROUTINE SUBPAR_MNMX( NAMECODE, MINMAX, STATUS )
*+
*  Name:
*     SUBPAR_MNMX

*  Purpose:
*     To set a parameter to its minimum or maximum value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_MNMX( NAMECODE, MINMAX, STATUS )

*  Description:
*     This routine attempts to obtain a minimum or maximum
*     (depending on MINMAX) value for the parameter from the
*     the SUBPAR common blocks - trying first values set by
*     SUBPAR_MIN or SUBPAR_MAX and failing that, values
*     defined by the RANGE declaration in the interface file.
*     If no appropriate value is found, error SUBPAR__NOMNMX
*     is reported; otherwise, storage for the parameter is
*     created if necessary and the value stored.

*  Arguments:
*     NAMECODE = INTEGER (Given)
*        The parameter index
*     MINMAX = CHARACTER * ( * ) (Given)
*        MIN or MAX as required
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     3-OCT-1990 (AJC):
*        Original version.
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     16-DEC-1998 (AJC):
*        Create _CHAR storage of sufficient size
*     22-JUL-2020 (DSB):
*        Correct assignment of _INT64 internal value to PARINT64 rather
*        than PARINT.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PAR'

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  External routines :
      CHARACTER*15 SUBPAR_CTYPE  ! Character form of TYPE
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL CHR_LEN
*  Arguments Given:
      INTEGER NAMECODE
      CHARACTER*(*) MINMAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

      CHARACTER*(DAT__SZLOC) LOC ! locator to the stored data
      INTEGER PTR
      INTEGER TYPE               ! code for data type
      INTEGER TMPLEN             ! temporary string length
      INTEGER FIELD              ! output from CHR_ITOC

      CHARACTER*15 HDSTYPE       ! data type
      CHARACTER*15 POSTYPES(8)   ! possible primitive data types

*  Local Data:
      DATA POSTYPES / '_CHAR*', '_REAL', '_DOUBLE', '_INTEGER',
     :  '_LOGICAL', ' ',  ' ',  '_INT64' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( MINMAX .EQ. 'MIN' ) THEN
*     Minimum value is required
         IF ( PARMIN(2,NAMECODE) .GT. 0 ) THEN
*        There is a MIN value
            PTR = PARMIN( 1, NAMECODE )

         ELSE
*        No MIN value exists - try a lower RANGE value.

            IF (( PARLIMS( 1, NAMECODE ) .GT. 0 ) .AND.
     :            PARCONT( NAMECODE ) ) THEN
*           There is a RANGE
               PTR = PARLIMS( 1, NAMECODE )
            ELSE
*           Neither MIN nor RANGE
               STATUS = SUBPAR__NOMNMX
               CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_REP( 'SUP_MNMX1',
     :         'SUBPAR_MNMX: Parameter ^NAME - ' //
     :         'no lower limit set', STATUS )
            ENDIF

         ENDIF

      ELSEIF ( MINMAX .EQ. 'MAX' ) THEN
*     Maximum value is required
         IF ( PARMAX(2,NAMECODE) .GT. 0 ) THEN
*        There is a MAX value
            PTR = PARMAX( 1, NAMECODE )

         ELSE
*        No MAX value exists - try an upper RANGE value.

            IF (( PARLIMS( 2, NAMECODE ) .GT. 0 ) .AND.
     :            PARCONT( NAMECODE ) ) THEN
*           There is a RANGE
               PTR = PARLIMS( 2, NAMECODE )
            ELSE
*           Neither MAX nor RANGE
               STATUS = SUBPAR__NOMNMX
               CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_REP( 'SUP_MNMX2',
     :         'SUBPAR_MNMX: Parameter ^NAME - ' //
     :         'no upper limit set', STATUS )
            ENDIF

         ENDIF

      ELSE
*     Illegal MINMAX argument
         STATUS = SUBPAR__ERROR
         CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
         CALL EMS_SETC( 'ARG', MINMAX )
         CALL EMS_REP( 'SUP_MNMX3',
     :   'SUBPAR_MNMX: Parameter ^NAME - Illegal argument ''^ARG''',
     :    STATUS )
      ENDIF

*  Now we should have a pointer to the required value in the
*  appropriate typeLIST array.
      IF ( STATUS .EQ. SAI__OK ) THEN

*     Get the parameter type
         TYPE = MOD( PARTYPE(NAMECODE), 10 )

         IF( ( TYPE .EQ. SUBPAR__CHAR ) .OR.
     :       ( TYPE .EQ. SUBPAR__DOUBLE ) .OR.
     :       ( TYPE .EQ. SUBPAR__INTEGER ) .OR.
     :       ( TYPE .EQ. SUBPAR__REAL ) ) THEN

*        If parameter is internal cancel any previous HDS association
*        and store the value in memory
            IF (PARVPATH(1,NAMECODE) .EQ. SUBPAR__INTERNAL) THEN
               CALL SUBPAR_CANCL( NAMECODE, STATUS)
               IF ( TYPE .EQ. SUBPAR__REAL ) THEN
                  PARREAL( NAMECODE ) = REALLIST(PTR)
               ELSE IF ( TYPE .EQ. SUBPAR__INTEGER ) THEN
                  PARINT( NAMECODE ) = INTLIST(PTR)
               ELSE IF ( TYPE .EQ. SUBPAR__INT64 ) THEN
                  PARINT64( NAMECODE ) = INT64LIST(PTR)
               ELSE IF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN
                  PARDOUBLE( NAMECODE ) = DOUBLELIST(PTR)
               ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
                  PARVALS( NAMECODE) = CHARLIST(PTR)
               ELSE
*              Illegal type
                  STATUS = SUBPAR__MNMXTY
                  CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_SETC( 'TYPE', SUBPAR_CTYPE(TYPE) )
                  CALL EMS_REP( 'SUP_MNMX4',
     :            'SUBPAR: Parameter ^NAME - '//
     :            'MIN/MAX is illegal for type ''^TYPE''', STATUS )
               ENDIF
               IF ( STATUS .EQ. SAI__OK ) THEN
                  PARSTATE( NAMECODE ) = SUBPAR__ACTIVE
               ENDIF

            ELSE
*           otherwise not internal - create storage to hold the data and
*           return a locator to it
*           also sets the parameter ACTIVE.
*
*           Invent the character-form of the data-type
               HDSTYPE = POSTYPES(TYPE)
               IF ( HDSTYPE .EQ. '_CHAR*' ) THEN
                  TMPLEN = MAX( 132, CHR_LEN(CHARLIST(PTR)) )
                  CALL CHR_ITOC( TMPLEN, HDSTYPE(7:), FIELD )
               ENDIF
*           Create the parameter file component
               CALL SUBPAR_CRINT ( NAMECODE, HDSTYPE, 0, 0, LOC,
     :         STATUS )

*           and, IF OK,  save data in it.
               IF( STATUS .EQ.SAI__OK ) THEN

                  IF ( TYPE .EQ. SUBPAR__REAL ) THEN
                     CALL DAT_PUT0R( LOC, REALLIST(PTR), STATUS )
                  ELSE IF ( TYPE .EQ. SUBPAR__INTEGER ) THEN
                     CALL DAT_PUT0I( LOC, INTLIST(PTR), STATUS )
                  ELSE IF ( TYPE .EQ. SUBPAR__INT64 ) THEN
                     CALL DAT_PUT0K( LOC, INT64LIST(PTR), STATUS )
                  ELSE IF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN
                     CALL DAT_PUT0D( LOC, DOUBLELIST(PTR), STATUS )
                  ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
                     CALL DAT_PUT0C( LOC, CHARLIST(PTR), STATUS )
                  ENDIF
*              If store failed, cancel the parameter
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL SUBPAR_CANCL( NAMECODE, STATUS )
                  ENDIF
*               Annul the locator.
                  CALL DAT_ANNUL( LOC, STATUS )

               ENDIF
            ENDIF
         ELSE
*        Illegal type
            STATUS = SUBPAR__MNMXTY
            CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
            CALL EMS_SETC( 'TYPE', SUBPAR_CTYPE(TYPE) )
            CALL EMS_REP( 'SUP_MNMX5', 'SUBPAR: Parameter ^NAME - '//
     :      'MIN/MAX is illegal for type ''^TYPE''', STATUS )
         ENDIF
      ENDIF

      END

