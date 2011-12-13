      SUBROUTINE SUBPAR_GETVI ( NAMECODE, MAXVAL, IVALUES, ACTVAL,
     :  STATUS )

*+
*  Name:
*     SUBPAR_GETVI

*  Purpose:
*     Read parameter values as if object were a vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETVI ( NAMECODE, MAXVAL, IVALUES, ACTVAL, STATUS )

*  Description:
*     Read the values from a primitive object associated with a Parameter
*     as if it were vectorised (ie regardless of dimensionality)
*     There is a routine for each access type, INTEGER:

*        SUBPAR_GETVD    DOUBLE PRECISION
*        SUBPAR_GETVR    REAL
*        SUBPAR_GETVI    INTEGER
*        SUBPAR_GETVL    LOGICAL
*        SUBPAR_GETVC    CHARACTER[*n]

*     If the object data type differs from the access type, INTEGER, then
*     conversion is performed (if allowed).

*  Arguments:
*     NAMECODE=INTEGER ( given)
*        pointer to the parameter
*     MAXVAL=INTEGER
*        the maximum number of values to be obtained.
*     IVALUES(MAXVAL)=INTEGER
*        Array containing the values to be read from the object.
*     ACTVAL=INTEGER
*        number of values returned
*     STATUS=INTEGER

*  Algorithm:
*     The HDS locator associated with the parameter is obtained.
*     If this is successful, the size of the object is obtained.
*     If it is too big, error SUBPAR__ARRDIM is reported, the value cancelled
*     and another value obtained if possible by prompting.
*     If all is OK, the data are extracted into the required vector.
*     If after ASSOC is OK, an error is detected, the value cancelled and
*     another value obtained if possible by prompting.
*     If the locator was obtained it is annulled.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
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
*     19.11.1984 (BDK):
*        Original
*     12.02.1985 (BDK):
*        Set STATUS=DAT__BOUND only if status ok.
*     05.06.1985 (BDK):
*        Call DAT_ASSOC with UPDATE in case of subsequent
*        PAR_PUT calls
*     18.08.1986 (BDK):
*        Check status before using pointer
*     23.06.1987 (BDK):
*        Avoid passing invalid descriptor to OUTCOPY
*     15.08.1988 (AJC):
*        Don't annul locator if not obtained
*     11.07.1991 (AJC):
*        Use HDS conversion
*      5.08.1992 (AJC):
*        Report to cover HDS not reporting on error
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      2-JUL-1993 (AJC):
*        Re-prompt on all errors up to five times
*     29-SEP-1994 (AJC):
*        Use EMS_FACER not DAT_ERMSG to report errors
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! SAI Constants
      INCLUDE 'DAT_PAR'                 ! DAT Constants
      INCLUDE 'SUBPAR_PAR'              ! SUBPAR Constants
      INCLUDE 'SUBPAR_ERR'              ! SUBPAR errors
      INCLUDE 'SUBPAR_PARERR'           ! SUBPAR PAR errors

*  Local Constants:
      INTEGER MAXTRY                    ! Maximum attempts to get good value
      PARAMETER ( MAXTRY = 5 )

*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter number
      INTEGER MAXVAL                    ! maximum number of values

*  Arguments Returned:
      INTEGER IVALUES(*)               ! Array for values
      INTEGER ACTVAL                    ! number of values

*  Status return :
      INTEGER STATUS                    ! Status Return

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER SIZE                      ! Actual size of object
      INTEGER TRIES                     ! Number of tries
      CHARACTER*(DAT__SZLOC) BOTLOC     ! HDS locator
      LOGICAL ACCEPTED                  ! If no re-prompt required

*.

      IF (STATUS .NE. SAI__OK) RETURN

*   Protect higher level tokens
      CALL EMS_MARK

*   Loop until ACCEPTED
      ACCEPTED = .FALSE.
      TRIES = 0

      DOWHILE ( .NOT. ACCEPTED )

*      Get an HDS locator to the data for the parameter, determine its
*      type and ask for a vectorised locator to it.
*
         IF ( PARWRITE(NAMECODE) ) THEN
            CALL SUBPAR_ASSOC ( NAMECODE, 'UPDATE', BOTLOC, STATUS )
         ELSE
            CALL SUBPAR_ASSOC ( NAMECODE, 'READ', BOTLOC, STATUS )
         ENDIF

         IF (STATUS .EQ. SAI__OK) THEN

*        Check the size
            CALL DAT_SIZE( BOTLOC, SIZE, STATUS )
            IF ( ( STATUS .EQ. SAI__OK )
     :      .AND. ( SIZE .GT. MAXVAL ) ) THEN
               STATUS = SUBPAR__ARRDIM
               CALL EMS_SETI( 'MAXVAL', MAXVAL )
               CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_REP( 'SUP_GETV1', 'SUBPAR: '//
     :         'No more than ^MAXVAL elements are allowed '//
     :         'for parameter ^NAME', STATUS )

            ELSE
*           Obtain the 'vectorised' data
               CALL DAT_GETVI ( BOTLOC, MAXVAL, IVALUES, ACTVAL,
     :         STATUS )

            ENDIF

*        Annul the locator
            CALL DAT_ANNUL ( BOTLOC, STATUS )

*        If OK, break the loop
            IF ( STATUS .EQ. SAI__OK ) THEN
               ACCEPTED = .TRUE.

*        Otherwise cancel the parameter - forcing reprompt,
*        flush error messages and loop
            ELSE
               IF ( STATUS .NE. SUBPAR__ARRDIM ) THEN
                  CALL EMS_FACER( 'MESS', STATUS )
                  CALL EMS_REP( 'SUP_GETV2', '^MESS', STATUS )
               ENDIF
               CALL SUBPAR_CANCL ( NAMECODE, STATUS )
*           Flush error and reset status
               CALL SUBPAR_EFLSH( STATUS )

*           Check for try limit
               TRIES = TRIES + 1
               IF ( TRIES .EQ. MAXTRY ) THEN
                  ACCEPTED = .FALSE.
                  STATUS = PAR__NULL
                  PARSTATE(NAMECODE) = SUBPAR__NULL
                  CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_SETI( 'TRIES', TRIES )
                  CALL EMS_REP( 'SUP_GETV3', 'SUBPAR: '//
     :            '^TRIES prompts failed to get a good value for '//
     :            'parameter ^NAME - NULL assumed', STATUS )
               ENDIF

            ENDIF


*     Else if ASSOC failed - exit
         ELSE
            ACCEPTED = .TRUE.

         ENDIF

      ENDDO

*  Release the error context
      CALL EMS_RLSE

      END
