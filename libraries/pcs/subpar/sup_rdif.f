      SUBROUTINE SUBPAR_RDIF( IFNAM, IFC, STATUS )
*+
*  Name:
*     SUBPAR_RDIF

*  Purpose:
*     To read the task interface module and use it to set up the
*     parameter system common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_RDIF( IFNAM, IFC, STATUS )

*  Description:
*     If STATUS is not SAI__OK on entry, the routine returns without
*     action.
*      The common blocks are set up
*      using either SUBPAR_LOADIFC (if the filetype is .IFC) or
*      PARSECON_READIFL (if the filetype is .IFL).
*      If the routine is unsuccessful, a message is reported and an
*      appropriate STATUS value is returned.

*  Arguments:
*     IFNAM = CHARACTER*(*) (Given)
*        The file specification of the interface module
*     IFC = LOGICAL (Given)
*        True if IFNAM is a compiled form interface module

*  Implementation Deficiencies:

*  Copyright:
*     Copyright (C) 1991, 1992, 1993, 1994 Science & Engineering Research Council.
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
*     16-JUL-1991 (AJC):
*        Original version.
*     27-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'.
*      5-AUG-1992 (AJC):
*        Save initial list pointers
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     24-FEB-1994 (AJC):
*        Initialize all PARVALIDs
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants
      INCLUDE 'SUBPAR_ERR'       ! SUBPAR Error numbers

*  Arguments Given:
      CHARACTER*(*) IFNAM
      LOGICAL IFC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER ISTAT              ! Local status
      INTEGER LUCON              ! Interface module unit number
      INTEGER NUMERR             ! Number of IFL compilation errors
      INTEGER I                  ! Parameter counter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If it's a compiled form, open and read the compiled interface file
      IF ( IFC ) THEN
         CALL SUBPAR_OPENIFC( IFNAM, LUCON, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SUBPAR_LOADIFC ( LUCON, STATUS )
            CLOSE ( LUCON, IOSTAT = ISTAT )
         ENDIF

*  Otherwise the interface module is text form - try to compile it
      ELSE
         CALL PARSECON_OPENIFL ( IFNAM, LUCON, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL PARSECON_READIFL ( LUCON, NUMERR, STATUS )
            CLOSE ( LUCON, IOSTAT = ISTAT )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL EMS_SYSER( 'SSTAT', STATUS )
               CALL EMS_REP( 'SUP_RDIF1', '^SSTAT', STATUS )
               CALL EMS_SETI( 'NUMERR', NUMERR )
               CALL EMS_REP( 'SUP_RDIF2',
     :          'SUBPAR: IFL parse failed with ^NUMERR errors',
     :          STATUS )

            ELSE IF ( NUMERR .GT. 0 ) THEN
               ISTAT = SAI__WARN
               CALL EMS_SETI( 'NUMERR', NUMERR )
               CALL EMS_REP( 'SUP_RDIF3',
     :          'SUBPAR: IFL parse completed with ^NUMERR errors',
     :          ISTAT )
               IF ( ISTAT .NE. SAI__WARN ) STATUS = ISTAT

            ENDIF

         ENDIF

      ENDIF

*  Initialize other elements
      DO 10 I = 1, PARPTR
         PARVALID(I) = .FALSE.
10    CONTINUE

*  Save the initial list pointer values
      CHARPSV = CHARPTR
      DOUBLEPSV = DOUBLEPTR
      INTPSV = INTPTR
      INT64PSV = INT64PTR
      LOGPSV = LOGPTR
      REALPSV = REALPTR

      END
