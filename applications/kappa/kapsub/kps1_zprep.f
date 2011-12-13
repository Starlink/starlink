      SUBROUTINE KPS1_ZPREP( REGION, LINE, ZLBND, ZUBND, LBND, UBND,
     :                       STATUS )
*+
*  Name:
*     KPS1_ZPREP

*  Purpose:
*     Reports the pixel indices of the bounds of a region to be zapped.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ZPREP( REGION, LINE, ZLBND, ZUBND, LBND, UBND, STATUS )

*  Description:
*     This routine is a server for ZAPLIN.  It reports the pixel indices
*     of the bounds of the region to be be zapped.  It also validates
*     the bounds for a line or column region by checking that they do
*     not include the whole array.
*
*     If a region has been zapped the format of the report is:
*        Zapping region (lower_column, lower_line) to
*        (upper_column, upper_line).
*
*     If a line has been zapped the format of the report is:
*        Zapping line from lower_line to upper_line
*
*     If lines have been zapped the format of the report is:
*        Zapping lines from lower_line to upper_line
*
*     If a column has been zapped the format of the report is:
*        Zapping column from lower_column to upper_column
*
*     If columns have been zapped the format of the report is:
*        Zapping columns from lower_column to upper_column

*  Arguments:
*     REGION = LOGICAL (Given)
*        If true a region the lower and upper pixel indices of the
*        regin to be zapped is reported.  If false the line or column
*        pixel-index bounds are reported.
*     LINE = LOGICAL (Given)
*        If true a string giving the line bounds in pixel indices are
*        reported.  If false the column bounds are reported.  However,
*        LINE is ignored if REGION is true.
*     ZLBND( 2 ) = INTEGER (Given)
*        The pixel-index lower bounds of the region that has been
*        zapped (column then line).  If REGION is false only one of
*        these will be used depending on the value of LINE.  The bounds
*        do include the origin of the main array.
*     ZUBND( 2 ) = INTEGER (Given)
*        The pixel-index upper bounds of the region that has been
*        zapped (column then line).  If REGION is false only one of
*        these will be used depending on the value of LINE.  The bounds
*        do include the origin of the main array.
*     LBND( 2 ) = INTEGER (Given)
*        The lower bounds of the main array in which zapping is to
*        occur.  Used to check that the whole array has not been
*        selected.
*     ULBND( 2 ) = INTEGER (Given)
*        The upper bounds of the main array in which zapping is to
*        occur.  Used to check that the whole array has not been
*        selected.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 May 29 (MJC):
*        Original version.
*     1995 April 26 (MJC):
*        Corrected the Purpose, and used modern-style commenting and
*        variable declarations.
*     2010 August 25 (MJC):
*        Used KPG_DIMLS instead of old DIMLST.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL REGION
      LOGICAL LINE
      INTEGER ZLBND( 2 )
      INTEGER ZUBND( 2 )
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 20 ) DIMSTR  ! Bounds in (x,y) notation
      INTEGER NC                 ! Character column counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( REGION ) THEN

*  Report what is going on by converting the lower and upper bounds to
*  of the region to strings in (x,y) notation.
         CALL KPG_DIMLS( 2, ZLBND, NC, DIMSTR, STATUS )
         CALL MSG_SETC( 'START', DIMSTR )
         CALL KPG_DIMLS( 2, ZUBND, NC, DIMSTR, STATUS )
         CALL MSG_SETC( 'FINISH', DIMSTR )
         CALL MSG_OUT( 'REGION_LIMITS', 'Zapping region '/
     :     /'^START to ^FINISH.', STATUS )

*  Check that the limits are valid, namely we must check for the case
*  where the whole array has been requested.
      ELSE IF ( LINE ) THEN
         IF ( ZLBND( 2 ) .LE. LBND( 2 ) .AND.
     :        ZUBND( 2 ) .GE. UBND( 2 ) ) THEN

*  Asking to zap whole array.  Report an error.
            CALL MSG_SETI( 'START', ZLBND( 2 ) )
            CALL MSG_SETI( 'FINISH', ZUBND( 2 ) )

            STATUS = SAI__ERROR
            CALL ERR_REP( 'ZAPLIN_WHOL',
     :        'ZAPLIN: Bad lines - given values for Start '/
     :        /'(^START) and Finish (^FINISH) cover whole array '/
     :        /'- cannot zap entire array.', STATUS )

         ELSE

*  Report what is going on.
            CALL MSG_SETI( 'START', ZLBND( 2 ) )
            IF ( ZLBND( 2 ) .EQ. ZUBND( 2 ) ) THEN
               CALL MSG_OUT( 'LINE_LIMITS', 'Zapping line '/
     :           /'^START.', STATUS )
            ELSE
               CALL MSG_SETI( 'FINISH', ZUBND( 2 ) )
               CALL MSG_OUT( 'LINE_LIMITS', 'Zapping lines '/
     :           /'from ^START to ^FINISH.', STATUS )
            END IF

*  End of if-input-lines-cover-whole-array check.
         END IF

*  Check column limits.
      ELSE

*  Check that the limits are valid, namely we must check for the case
*  where the whole array has been requested.
         IF ( ZLBND( 1 ) .LE. LBND( 1 ) .AND.
     :        ZUBND( 1 ) .GE. UBND( 1 ) ) THEN

*  Asking to zap the whole array.  Report an error.
            CALL MSG_SETI( 'START', ZLBND( 1 ) )
            CALL MSG_SETI( 'FINISH', ZUBND( 1 ) )

            STATUS = SAI__ERROR
            CALL ERR_REP( 'ZAPLIN_WHOC',
     :        'ZAPLIN: Bad columns - given values for Start '/
     :        /'(^START) and Finish (^FINISH) cover whole '/
     :        /'array - cannot zap entire array.', STATUS )

            ELSE

*  Report what is going on.
            CALL MSG_SETI( 'START', ZLBND( 1 ) )
            IF ( ZLBND( 1 ) .EQ. ZUBND( 1 ) ) THEN
               CALL MSG_OUT( 'COLUMN_LIMITS', 'Zapping column '/
     :           /'^START.', STATUS )
            ELSE
               CALL MSG_SETI( 'FINISH', ZUBND( 1 ) )
               CALL MSG_OUT( 'COLUMN_LIMITS', 'Zapping columns '/
     :           /'from ^START to ^FINISH.', STATUS )
            END IF

*  End of if-columns-cover-whole-array check.
         END IF

*  End of check for lines or columns.
      END IF

      END
