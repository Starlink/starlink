      LOGICAL FUNCTION SUBPAR_GREF( NAMECODE, REFSTR, REFLEN )
*+
*  Name:
*     SUBPAR_GREF

*  Purpose:
*     Get the reference for the specified parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_GREF( NAMECODE, REFSTR, REFLEN )

*  Description:
*     This routine makes an enquiry of the parameter system to
*     get the absolute object, device or file name (i.e. reference)
*     associated with the specified parameter.

*  Arguments:
*     NAMECODE = INTEGER * ( * ) (Given)
*        The parameter namecode.
*     REFSTR = CHARACTER * ( * ) (Returned)
*        The reference.
*     REFLEN = INTEGER (Returned)
*        The length of the reference.

*  Implementation Notes:
*     -  This function is for use in the ADAM version of MSG_. It is based on
*     the former version of MSG1_GREF, which now calls this routine. This
*     change allows the substitution of the Java parameter system

*  Algorithm:
*     -  Attempt to get a name via a valid locator from the parameter
*     system.
*     -  If this fails and the parameter type indicates a name, get
*     the name from the parameter table.

*  Copyright:
*     Copyright (C) 1982, 1983, 1984, 1988, 1989, 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     17-Aug-1983 (SLW):
*        New parameter system interface.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     3-NOV-1988 (AJC):
*        If no valid locator, try PARVAL.
*     18-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Changed name of function.
*     22-OCT-1991 (PCTR):
*        Added EMS_MARK and EMS_RLSE to annul any error messages from
*        SUBPAR on error.
*      3-DEC-2002 (AJC):
*        Changed to SUBPAR_GREF from MSG1_GREF
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'DAT_PAR'                 ! DAT_ public constants
      INCLUDE 'SUBPAR_PAR'              ! SUBPAR_ public constants

*  Arguments Given:
      INTEGER NAMECODE

*  Arguments Returned:
      CHARACTER * ( * ) REFSTR

      INTEGER REFLEN

*  External References:
      INTEGER CHR_LEN                   ! String length

*  Local Variables:
      LOGICAL VALID                     ! .TRUE. => valid locator obtained

      INTEGER NLEV                      ! Number of levels in refstr
      INTEGER STATE                     ! Parameter state
      INTEGER STATUS                    ! Local status
      INTEGER TYPE                      ! Parameter type code
      CHARACTER * 80 FILNAM             ! Object filename



      CHARACTER * ( DAT__SZLOC ) LOC    ! HDS locator

*.

*  Initialise the returned value of MSG1_GREF.
      SUBPAR_GREF = .FALSE.

*  Initialise the returned string.
      REFSTR =  ' '
      REFLEN = 1

*  Initialise the local status.
      STATUS = SAI__OK

*  Mark a new error reporting context.
      CALL EMS_MARK

*  Get a locator associated with the parameter PARAM, if it has one,
*  and return the HDS name.
      CALL SUBPAR_STATE( NAMECODE, STATE, STATUS )

*  Check the returned status.
      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( STATE .EQ. SUBPAR__ACTIVE ) THEN

*        Get the locator.
            CALL SUBPAR_GETLOC( NAMECODE, VALID, LOC, STATUS )

*        Check the returned status.
            IF ( STATUS .EQ. SAI__OK .AND. VALID ) THEN
*           Mark a new error reporting context.
               CALL EMS_MARK

*           Get the name of the object.
               CALL HDS_TRACE( LOC, NLEV, REFSTR, FILNAM, STATUS )

               IF( STATUS .EQ. SAI__OK ) THEN
*           Set returned arguments for normal successful completion.
                  SUBPAR_GREF = .TRUE.
                  REFLEN = CHR_LEN( REFSTR )

               ELSE
*           On error, annul the error context.
                  CALL EMS_ANNUL( STATUS )

               END IF

*           Release the error reporting context.
               CALL EMS_RLSE


            ELSE

*           Annul the error context.
               CALL EMS_ANNUL( STATUS )

*           There is no valid locator, so see if there is a
*           name associated.
               CALL SUBPAR_PARTYPE( NAMECODE, TYPE, STATUS )

*           Check the returned status.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TYPE .GE. 20 ) THEN

*                 It must be a name other than an HDS object,
*                 so get the PARVALS entry.
                     CALL SUBPAR_FETCHC( NAMECODE, REFSTR, STATUS )

*                 Check the returned status.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        REFLEN = CHR_LEN( REFSTR )
                        SUBPAR_GREF = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Check the returned status and annul the error context on error.
      IF ( STATUS .NE. SAI__OK ) CALL EMS_ANNUL( STATUS )

*  Release the error reporting context.
      CALL EMS_RLSE

      END
