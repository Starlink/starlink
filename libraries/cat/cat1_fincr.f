      SUBROUTINE CAT1_FINCR (CI, STATUS)
*+
*  Name:
*     CAT1_FINCR
*  Purpose:
*     Finish the inital creation of a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_FINCR (CI; STATUS)
*  Description:
*     Finish the intial creation of the files associated with a new
*     catalogue.
*
*     The functions performed by this routine are as follows:
*
*      - maybe the initial creation of the files (though this may
*        have been done by TOPEN - individual back-ends will vary),
*      - creation of any mandatory header information,
*      - writing the parameters to the files,
*      - writing the column details to the files.
*
*     However, at least in this first implementation, most of this
*     functionality goes on inside the back-end for the appropriate
*     catalogue.
*  Arguments:
*     CI  INTEGER (Given)
*        Catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the array element corresponding to the catalogue.
*     If ok then
*       Determine the back-end type.
*       Invoke the appropriate back-end.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     5/8/93   (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CIELM,   ! Common block array element for the catalogue.
     :  BCKTYP   ! Back-end type.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN
            BCKTYP = BKTYP__CAT1(CIELM)

*
*          Invoke the appropriate back-end.

            CALL CAT0_FINCR (BCKTYP, CI, STATUS)

         END IF

      END IF

      END
