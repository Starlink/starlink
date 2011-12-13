      SUBROUTINE CAT_TRLSE (CI, STATUS)
*+
*  Name:
*     CAT_TRLSE
*  Purpose:
*     Release a catalogue identifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TRLSE (CI; STATUS)
*  Description:
*     Release a catalogue identifier.
*
*     Internal work space and arrays used by the catalogue are released
*     and made available for re-use.
*
*     Note thatthis routine attempts to execute irrespective of the
*     status on entry.
*  Arguments:
*     CI  =  INTEGER (Given and Returned)
*        On input: the catalogue identifier to be released.
*        On output: the null identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the identifier is for a catalogue then
*       Determine the back-end type.
*       If ok then
*         Update and close the appropriate back-end files.
*       end if
*       Free any work arrays associated with selections or indices.
*       Release the space occupied by the catalogue in the catalogue,
*       identifiers and attributes arrays.
*       Update the return argument to the null identifier.
*     end if
*     If input status is ok then
*       Copy the local status to the return status.
*       Report any local error.
*     end if
*  Implementation Deficiencies:
*     This implmentation only releases a catalogue identifier.  The
*     original idea of CAT_TRLSE was that it would release any type
*     of identifier.  However, in the current version of CAT only
*     catalogue identifiers need to be released.
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
*     6/8/93   (ACD): Original version.
*     11/10/93 (ACD): First stable version.
*     25/1/94  (ACD): Modified error reporting.
*     11/4/95  (ACD): Changed the name of the null identifier.
*     9/20/95  (ACD): Corrected spelling mistake in the prologue.
*     27/5/98  (ACD): Modified to release work and array space.
*        Also converted to be specifically for catalogue identifiers.
*     15/6/98  (ACD): Removed warning message issued when the routine
*        is given a non-catalogue identifier.
*     19/9/99  (ACD): Modified to execute irrespective of the status
*        on entry.
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
*  Arguments Given and Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  RLSTAT,  ! Local CAT status.
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  BCKTYP,  ! Back-end type.
     :  IDTYPE   ! Type of identifier CI.
*.

c     print1999, status
c1999 format(1x, 'Entry status: ', i15)

*
*    Initialise the local status.

      RLSTAT = CAT__OK

*
*    Determine the type of the identifier and proceed if it is a
*    catalogue.

      CALL CAT1_TIDTP (CI, IDTYPE, RLSTAT)
c     print2000, 'TIDTP', rlstat
c2000 format(1x, 'after ', a, 2x, 'rlstat: ', i15)

      IF (IDTYPE .EQ. CAT__CITYP) THEN

*
*       Determine the back-end type and if ok then update and close
*       the appropriate back-end files.

         CALL CAT1_CIELM (CI, CIELM, RLSTAT)
c        print2000, 'CIELM', rlstat

         IF (RLSTAT .EQ. CAT__OK) THEN
            BCKTYP = BKTYP__CAT1(CIELM)
            CALL CAT0_CLOSE (BCKTYP, CI, RLSTAT)
c           print2000, 'CAT0_CLOSE', rlstat
         END IF

*
*       Free any work arrays associated with selections or indices.

         CALL CAT1_FRESP (CI, RLSTAT)
c        print2000, 'CAT1_FRESP', rlstat

*
*       Release the space occupied by the catalogue in the catalogue,
*       identifiers and attributes arrays.

         CALL CAT1_RCLSP (CI, RLSTAT)
c        print2000, 'CAT1_RCLSP', rlstat

*
*       Update the return argument to the null identifier.

         CI = CAT__NOID

      END IF

*
*    If the input status was ok then copy the local status to the return
*    status and report any local error.

      IF (STATUS .EQ. CAT__OK) THEN
         STATUS = RLSTAT

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TRLSE_ERR', 'CAT_TRLSE: error '/
     :        /'releasing an identifier.', STATUS)
         END IF

      END IF

c     print1998, status
c1998 format(1x, 'Exit status: ', i15)

      END
