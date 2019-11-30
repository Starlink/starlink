*        Add DAT_PAR for SUBPAR_CMN
      SUBROUTINE SUBPAR_FETCHI ( NAMECODE, VALUE, STATUS )
*+
*  Name:
*     SUBPAR_FETCHI

*  Purpose:
*     Get a scalar value for an internal parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FETCHI ( NAMECODE, VALUE, STATUS )

*  Description:
*     The value of a scalar parameter declared to have a VPATH of INTERNAL
*     is obtained from internal storage. If the parameter is not ACTIVE,
*     then an attempt is made to give it a value from the lists of
*     dynamic and static defaults.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to parameter
*     VALUE=INTEGER (returned)
*        parameter value
*     STATUS=INTEGER

*  Algorithm:
*     If the parameter is active, return its value. Otherwise, try to
*     get a value from the dynamic or static defaults. If this succeeds
*     copy it into the parameter value storage and mark the parameter as
*     active, and return the value.

*  Copyright:
*     Copyright (C) 1984, 1992, 1993 Science & Engineering Research Council.
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
*     28-SEP-1984 (BDK):
*        Original
*     22-JUL-1992 (AJC):
*        Allow for dynamic stored in other type
*     17-NOV-1992 (AJC):
*        Allow MIN/MAX
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE


*  Arguments Returned:
      INTEGER VALUE


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACTIVE ) THEN
*
*   Get the value from internal storage
*
         VALUE = PARINT(NAMECODE)

      ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__MAX ) THEN
*   The parameter was given the value MAX
*   First set the maximum value
         CALL SUBPAR_MNMX( NAMECODE, 'MAX', STATUS )
*   then get it
         IF ( STATUS .EQ. SAI__OK ) THEN
            VALUE = PARINT(NAMECODE)
         ENDIF

      ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__MIN ) THEN
*   The parameter was given the value MIN
*   First set the minimum value
         CALL SUBPAR_MNMX( NAMECODE, 'MIN', STATUS )
*   then get it
         IF ( STATUS .EQ. SAI__OK ) THEN
            VALUE = PARINT(NAMECODE)
         ENDIF

      ELSE IF ( ( PARDYN(1,NAMECODE) .GT. 0 )
     :     .AND.( PARDYN(3,NAMECODE) .GT. 0 ) ) THEN
*
*     There is a dynamic default - convert from the stored type
*
         IF ( PARDYN(3,NAMECODE) .EQ. SUBPAR__INTEGER ) THEN
            PARINT(NAMECODE) = INTLIST( PARDYN(1,NAMECODE) )

         ELSE IF ( PARDYN(3,NAMECODE) .EQ. SUBPAR__INT64 ) THEN
            PARINT(NAMECODE) = INT64LIST( PARDYN(1,NAMECODE) )

         ELSE IF ( PARDYN(3,NAMECODE) .EQ. SUBPAR__CHAR ) THEN
            CALL CHR_CTOI( CHARLIST(PARDYN(1,NAMECODE)),
     :        PARINT(NAMECODE), STATUS )

         ELSE IF ( PARDYN(3,NAMECODE) .EQ. SUBPAR__DOUBLE ) THEN
            PARINT(NAMECODE) = INT( DOUBLELIST(PARDYN(1,NAMECODE)) )

         ELSE IF ( PARDYN(3,NAMECODE) .EQ. SUBPAR__LOGICAL ) THEN
            IF ( LOGLIST( PARDYN(1,NAMECODE) ) )  THEN
               PARINT(NAMECODE) = 1
            ELSE
               PARINT(NAMECODE) = 0
            ENDIF

         ELSE IF ( PARDYN(3,NAMECODE) .EQ. SUBPAR__REAL ) THEN
            PARINT(NAMECODE) = INT( REALLIST(PARDYN(1,NAMECODE)) )

         END IF

         VALUE = PARINT(NAMECODE)
         PARSTATE(NAMECODE) = SUBPAR__ACTIVE

      ELSE IF ( PARDEF(3,NAMECODE) .EQ. SUBPAR__INTEGER ) THEN
*
*   There is a static default
*
         PARINT(NAMECODE) = INTLIST(PARDEF(1,NAMECODE))
         VALUE = PARINT(NAMECODE)
         PARSTATE(NAMECODE) = SUBPAR__ACTIVE

      ELSE

         STATUS = PAR__NULL

      ENDIF

      END
