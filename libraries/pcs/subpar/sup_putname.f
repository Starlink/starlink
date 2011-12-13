      SUBROUTINE SUBPAR_PUTNAME ( NAMECODE, STRUCTNAME, STATUS )
*+
*  Name:
*     SUBPAR_PUTNAME

*  Purpose:
*     Associates structure name with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_PUTNAME ( NAMECODE, STRUCTNAME, STATUS )

*  Description:
*     The character string STRUCTNAME is taken to be the full
*     description of a data structure to be associated with the named
*     program parameter. This string is written into internal storage,
*     and is used by the PAR_GETnx, PAR_PUTnx, DAT_ASSOC etc, routines
*     to point to where the actual parameter values are stored.

*  Arguments:
*     NAMECODE=INTEGER (given)
*     STRUCTNAME=CHARACTER*(*) (given)
*     STATUS=INTEGER

*  Algorithm:
*     NAMECODE, which is the address associated with the parameter is
*     used to access internal parameter storage. The parameter is
*     cancelled, and then marked as being active and stored externally, and
*     the structure name is copied to the internal store.

*  Implementation Deficiencies:
*     This routine is part of the implementation of a Starlink
*     look-alike for ADAM. However, it does not correspond to anything
*     in the Starlink applications interfaces, and should not be called
*     by programs (eg data analysis programs) intended to be Starlink
*     compatible.

*  Copyright:
*     Copyright (C) 1984, 1988, 1993 Science & Engineering Research Council.
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
*     24-SEP-1984 (BDK):
*        Original version
*     16-AUG-1988 (AJC):
*        Include SUBPAR_PAR - How did it ever work
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE                    ! index to the program
                                          ! parameter

      CHARACTER*(*) STRUCTNAME            ! the name of the data structure
                                          ! to be associated with the
                                          ! parameter.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Cancel the previous state of the parameter
*
      CALL SUBPAR_CANCL ( NAMECODE, STATUS )
*
*   Store the structure name, and set the type to signal
*   external value storage.
*
      PARVALS(NAMECODE) = STRUCTNAME
      PARTYPE(NAMECODE) = 20 + PARTYPE(NAMECODE)
      PARSTATE(NAMECODE) = SUBPAR__ACTIVE

      END
