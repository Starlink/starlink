
      INTEGER FUNCTION SUBPAR_OPUT( TEXT )
*+
*  Name:
*     SUBPAR_OPUT

*  Purpose:
*     For use by SUBPAR_HELP (and LBR$OUTPUT_HELP) when called by
*     SUBPAR_ASKPARAM, to output parameter help information or
*     associated error messages.
*     It has the calling sequence of LIB$PUT_OUTPUT, as required
*     by LBR$OUTPUT_HELP and Portable Help.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SUBPAR_OPUT( TEXT )

*  Description:
*     The routine calls SUBPAR_WRMSG prompting at the end of pages.
*     The function value is then set appropriately.

*  Arguments:
*     TEXT = CHARACTER*(*) (Given)
*        The text to be output

*  Returned Value:
*     SUBPAR_OPUT = INTEGER
*        1 if the routine is successful
*        An ADAM error status value if not

*  External Routines Used:
*     ICL:
*        ICL_WRITEA

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
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
*     RLVAD::AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1990 (RLVAD::AJC):
*        Original version.
*      1-MAR-1993 (RLVAD::AJC):
*        Use SAI not ADAM__OK
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      2-MAR-1993 (AJC):
*        Use SUBPAR_WRMSG not ICL_WRITEA
*        Also remove INCLUDE REPORT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard ADAM constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_CMN'       ! For help screen size

*  Arguments Given:
      CHARACTER*(*) TEXT

*  External References:
      INTEGER SUBPAR_IPUT
      EXTERNAL SUBPAR_IPUT

*  Local Variables:
      INTEGER ISTAT               ! Local status VMS type
      INTEGER STATUS              ! Local status ADAM type
      CHARACTER*20 REPLY
      INTEGER RETLEN

      ISTAT = 1

*   Check if paging required
      IF (SUBPARLCNT .GT. 0) THEN
*      If so, count down SUBPARLCNT
         SUBPARLCNT = SUBPARLCNT - 1
         IF (SUBPARLCNT .EQ. 1) THEN
*         Room for two more lines -
*         output the prompt. SUBPAR_IPUT will reset SUBPARLCNT to SUBPARPGSZ
*         but here we must allow for the current line being output.
            CALL SUBPAR_WRMSG( ' ', STATUS )
            ISTAT = SUBPAR_IPUT( REPLY, 'Press RETURN to continue ...',
     :                          RETLEN )
            SUBPARLCNT = SUBPARLCNT - 1
         ENDIF
      ENDIF

      IF (ISTAT .EQ. 1) THEN
*      Output the line
         STATUS = SAI__OK
         CALL SUBPAR_WRMSG( TEXT, STATUS )
         IF (STATUS .NE. SAI__OK) ISTAT = STATUS
      ENDIF

*   and set return status
      SUBPAR_OPUT = ISTAT

      END

