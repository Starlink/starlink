      SUBROUTINE COF_EXTXT( FSPEC, FLEN, EXTLEN, STATUS )
*+
*  Name:
*     COF_EXTXT

*  Purpose:
*     Find an extension specifier in a FITS filename.

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_EXTXT( FSPEC, FLEN, EXTLEN, STATUS )

*  Description:
*     If the last non-space character in FSPEC is ], returns
*     EXTLEN = length of FSPEC from the preceding [;
*     if not, it returns EXTLEN = 0.
*     The length of the filename part of FSPEC is returned in FLEN

*  Arguments:
*     FSPEC = CHARACTER*(*)
*        A Group Expression specifying a FITS file, possibly terminating
*        with a FITS extension specifier enclosed in [].
*     FLEN = INTEGER (Returned)
*        Length of the filename part of FSPEC
*     EXTLEN = INTEGER (Returned)
*        The length of the FITS extension component within FSPEC.
*        0 if none.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     CALL COF_EXTXT( FSPEC, FLEN, EXTLEN, STATUS )
*        If FSPEC is 'afile.fit', returns FLEN = 9, EXTLEN = 0
*        If FSPEC is 'bfile.fit[2]', returns FLEN = 9, EXTLEN = 3
*        If FSPEC is '[abc]file.fit[10], returns  FLEN = 13, EXTLEN = 4

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  References:
*     -  {reference}
*     [routine_references]...

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  External Routines Used:
*     CHR
*        CHR_LEN
*     MERS
*        MSG_SETC
*        ERR_REP
*
*     {name_of_facility_or_package}:
*        {routine_used}...
*     [facility_or_package]...

*  Keywords:
*     {routine_keywords}...

*  Pitfalls:
*     -  {pitfall}
*     [pitfall_description]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     20-MAR-2000 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) FSPEC

*  Arguments Given and Returned:

*  Arguments Returned:
      INTEGER FLEN
      INTEGER EXTLEN

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      INTEGER SPECSZ            ! Used length of filename part of FSPEC
      INTEGER I                 ! Loop counter

*  Local Data:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the used length of FSPEC
      SPECSZ = CHR_LEN( FSPEC )

*  If the last character of FSPEC is ']', find the preceding '['
      IF ( FSPEC(SPECSZ:SPECSZ) .EQ. ']' ) THEN
         IF ( SPECSZ .GT. 1 ) THEN
            DO I = SPECSZ-1, 1, -1
               IF ( FSPEC(I:I) .EQ. '[' ) THEN
                  EXTLEN = SPECSZ - I + 1
                  GOTO 10
               END IF
            END DO
         END IF

*  Missing open bracket
         EXTLEN = 0
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FSPEC', FSPEC )
         CALL ERR_REP( 'COF_EXTXT_1',
     :     'Missing open bracket in ^FSPEC', STATUS )


      ELSE

         EXTLEN = 0

      END IF

10    CONTINUE
      FLEN = SPECSZ - EXTLEN

      END
