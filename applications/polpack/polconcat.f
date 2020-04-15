      SUBROUTINE POLCONCAT ( STATUS )
*+
*  Name:
*     POLCONCAT

*  Purpose:
*     Concatenate two or more vector catalogues.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLCONCAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new vector catalogue by concatenating
*     the vectors from a list of two or more input catalogues. The output
*     catalogue inherits the WCS and reference diection of the first input
*     catalogue.

*  Usage:
*     polconcat in out

*  ADAM Parameters:
*     IN = LITERAL (Write)
*        A group of two or more input vector catalogues.
*     OUT = LITERAL (Write)
*        The output catalogue.

*  Notes:
*     - All input catalogues must contain WCS information (i.e. they must
*     have been created using polpack).
*     - This command does not currently support concatenating catalogues
*     that contain circular polarisation values.
*     - All input catalogues must use the same units for I, Q and U.
*     - If the first input catalogue contains variance columns, then all
*     input catalogues must contain variance columns. If the first input
*     catalogue does not contains variance columns, then none of the
*     input catalogues must contain variance columns.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
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
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     27-SEP-2017 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! First input catalogue name
      CHARACTER ONAME*(GRP__SZNAM)! Full file spec for output catalogue
      CHARACTER UNITS*(CAT__SZUNI)! Units of the I column
      CHARACTER UNITS1*(CAT__SZUNI)! I units from 1st input catalogue
      INTEGER CI                 ! CAT identifier for an input catalogue
      INTEGER CIOUT              ! CAT identifier for output catalogue
      INTEGER GI                 ! CAT identifiers for a column
      INTEGER I                  ! Index of current input catalogue
      INTEGER IGRP               ! Group of paths for input catalogues
      INTEGER NIN                ! Number of input catalogues
      INTEGER TWCS               ! WCS FrameSet from 1st input catalogue
      LOGICAL FLAG               ! User wants to supply more inputs?
      LOGICAL VAR                ! Current input cat has variances?
      LOGICAL VAR1               ! First input cat has variances?

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a group of input catalogue paths from the environment. Loop until
*  a group expression is given which is not terminated by a flag character.
      IGRP = GRP__NOID
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
         CALL CTG_ASSOC( 'IN', .FALSE., IGRP, NIN, FLAG, STATUS )
         IF( FLAG ) THEN
            CALL PAR_CANCL( 'IN', STATUS )
            CALL MSG_SETC( 'P', 'IN' )
            CALL MSG_OUT( ' ', 'Please supply more values for '//
     :                    'parameter %^P.', STATUS )
         END IF
      END DO

*  Must have at least two input catalogues.
      IF( NIN .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'At least two input catalogues must be '//
     :                 'supplied', STATUS )
      END IF

*  Open the output catalogue.
      CALL CTG_CREA1( 'OUT', CIOUT, ONAME, STATUS )

*  Loop round all input catalogues.
      DO I = 1, NIN

*  Get an identifier for the I'th input catalogue.
         CALL CTG_CATAS( IGRP, I, 'READ', CI, STATUS )

*  If there is a column named "V", report an error since this application
*  cannot currently handle circular polarisation.
         CALL POL1_GTCOL( CI, 'V', .FALSE., GI, STATUS )
         IF( GI .NE. CAT__NOID .AND. STATUS .EQ. SAI__OK ) THEN
            CALL CAT_TRLSE( GI, STATUS )
            CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', NAME )
            CALL ERR_REP( ' ', 'Input catalogue "^C" contains '//
     :                    'circular polarisation values.', status )
            CALL ERR_REP( ' ', 'The polconcat command does not '//
     :                    'yet support circular polarisation.', status )
         END IF

*  Get the units string from total intensity column of the input catalogue.
         CALL POL1_GTCOL( CI, 'I', .TRUE., GI, STATUS )
         UNITS = ' '
         CALL CAT_TIQAC( GI, 'UNITS', UNITS, STATUS)
         CALL CAT_TRLSE( GI, STATUS )

*  See if the current catalogue contains variances.
         CALL POL1_GTCOL( CI, 'DI', .TRUE., GI, STATUS )
         IF( GI .NE. CAT__NOID ) THEN
            CALL CAT_TRLSE( GI, STATUS )
            VAR = .TRUE.
         ELSE
            VAR = .FALSE.
         END IF

*  If this is the first input catalogue, save the above values for use
*  when checking subsequent input catalogues.
         IF( I .EQ. 1 ) THEN
            UNITS1 = UNITS
            VAR1 = VAR

*  Otherwise check that the values for the current input catalogue match
*  those of the first input catalogue.
         ELSE IF( UNITS1 .NE. UNITS .AND. STATUS .EQ. SAI__OK ) THEN
            CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', NAME )
            CALL MSG_SETC( 'U', UNITS )
            CALL MSG_SETC( 'U1', UNITS1 )
            CALL ERR_REP( ' ', 'The I column has units of "^U" in '//
     :                    'input catalogue "^C" but has units of '//
     :                    '"^U1" in the first input catalogue.',
     :                    status )

         ELSE IF( VAR1 .AND. .NOT. VAR .AND. STATUS .EQ. SAI__OK ) THEN
            CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', NAME )
            CALL ERR_REP( ' ', 'Input catalogue "^C" has no variances'//
     :                    ' but the first input catalogue does have '//
     :                    'variances.', status )

         ELSE IF( VAR .AND. .NOT. VAR1 .AND. STATUS .EQ. SAI__OK ) THEN
            CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', NAME )
            CALL ERR_REP( ' ', 'Input catalogue "^C" has variances'//
     :                    ' but the first input catalogue does not '//
     :                    'have variances.', status )
         END IF

*  If this is the first input catalogue, attempt to read an AST
*  FrameSet from it. This WCS information will be copied to the output
*  catalogue when the output catalogue is closed.
         IF( I .EQ. 1 ) THEN
            CALL POL1_GTCTW( CI, TWCS, STATUS )
            IF( TWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
               CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'C', NAME )
               CALL ERR_REP( ' ', 'No WCS information found in input '//
     :                       'catalogue "^C".', STATUS )
            END IF

*  Copy the polpack version number from first input to output.
            CALL POL1_CPVRC( CI, CIOUT, STATUS )

         END IF

*  Append the input to the output.
         CALL POL1_CPCAT( CI, CIOUT, TWCS, .TRUE., STATUS )

*  Release the identifier for the first input catalogue.
         CALL CAT_TRLSE( CI, STATUS )
      END DO

*  Close the output catalogue, storing a copy of the WCS information from
*  the first input catalogue.
      CALL POL1_CLCAT( TWCS, CIOUT, STATUS )

*  If an error has occurred, delete the output catalogue.
      IF( STATUS .NE. SAI__OK ) CALL POL1_RM( ONAME )

*  Delete the group of input catalogues.
      CALL GRP_DELET( IGRP, STATUS )

*  Release the AST resources.
      CALL AST_END( STATUS )

*  Add a context message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'POLCONCAT: Failed to concatenate two or '//
     :                 'more vector catalogues.', STATUS )
      END IF

      END
