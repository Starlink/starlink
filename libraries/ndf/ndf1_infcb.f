      SUBROUTINE NDF1_INFCB( STATUS )
*+
*  Name:
*     NDF1_INFCB

*  Purpose:
*     Initialise the NDF_ Format Conversion Block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_INFCB( STATUS )

*  Description:
*     The routine initialises the NDF_ Format Conversion Block (FCB)
*     which maintains information about the foreign data formats which
*     should be recognised by the NDF_ library and which may be
*     converted to and from native NDF format. This information is
*     obtained by translating appropriate environment variables.
*
*     This routine only executes once. If called again after a
*     successful initial invocation it will return without further
*     action.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1993 (RFWS):
*        Original version.
*     12-NOV-1993 (RFWS):
*        Added initialisation of NDF extension name list.
*     15-APR-1994 (RFWS):
*        Added initialisation of the anonymous converter array.
*     31-OCT-1994 (RFWS):
*        Removed initialisation of NDF extension list information (now
*        performed separately for each foreign file).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FEX1( 2 * NDF__MXFMT ) = INTEGER (Write)
*           Starting positions of each foreign format file name
*           extension in the FCB_FMT string.
*        FCB_FEX2( 2 * NDF__MXFMT ) = INTEGER (Write)
*           Ending positions of each foreign format file name extension
*           in the FCB_FMT string.
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Write)
*           String to contain list of input and output foreign format
*           specifications.
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Write)
*           Starting positions of each foreign format name in the
*           FCB_FMT string.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Write)
*           Ending positions of each foreign format name in the FCB_FMT
*           string.
*        FCB_NIN = INTEGER (Write)
*           Number of foreign formats to recognise on input.
*        FCB_NOUT = INTEGER (Write)
*           Number of foreign formats to recognise on output.

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER E1                 ! First extension character position
      INTEGER E2                 ! Last extension character position
      INTEGER F                  ! First character position
      INTEGER F1                 ! First format character position
      INTEGER F2                 ! Last format character position
      INTEGER IFMT               ! Loop counter for formats
      INTEGER L                  ! Last character position
      INTEGER LVAL               ! Length of environment variable value
      LOGICAL DEF                ! Environment variable defined?
      LOGICAL FIRST              ! First invocation of this routine?
      LOGICAL SDF                ! File extension is reserved (.sdf)?

      SAVE FIRST                 ! Remember if previously invoked

*  Local Data:
      DATA FIRST / .TRUE. /      ! Routine initially not yet invoked

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Only perform initialisation on the first invocation of this routine.
      IF ( FIRST ) THEN

*  Input formats.
*  =============
*  Translate the environment variable NDF_FORMATS_IN to obtain a list
*  of foreign data formats which are to be recognised on input. Put the
*  translation into the first half of the FCB format list string. Note
*  if there is no translation.
         CALL NDF1_GTENV( 'NDF_FORMATS_IN', DEF,
     :                    FCB_FMT( : NDF__SZFMT ), LVAL, STATUS )

*  If no translation was found, then no foreign data formats will be
*  recognised on input. Otherwise, parse the format list to split it
*  into separate format fields, storing the field positions temporarily
*  at the start of the format name field position list.
         FCB_NIN = 0
         IF ( ( STATUS .EQ. SAI__OK ) .AND. DEF ) THEN
            CALL NDF1_PSFFL( FCB_FMT( : NDF__SZFMT ), NDF__MXFMT,
     :                       FCB_FMT1( 1 ), FCB_FMT2( 1 ), FCB_NIN,
     :                       STATUS )

*  If OK, then loop to split each field into a foreign format name and
*  its associated file type extension string (which will be used to
*  identify files with that format).
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO 1 IFMT = 1, FCB_NIN
                  F = FCB_FMT1( IFMT )
                  L = FCB_FMT2( IFMT )
                  CALL NDF1_PSFMT( FCB_FMT( F : L ), F1, F2, E1, E2,
     :                             STATUS )

*  Quit looping if an error occurs.
                  IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Correct the returned field positions for the starting position used
*  and store the results at the start of the FCB field position lists.
                  FCB_FMT1( IFMT ) = F1 + F - 1
                  FCB_FMT2( IFMT ) = F2 + F - 1
                  FCB_FEX1( IFMT ) = E1 + F - 1
                  FCB_FEX2( IFMT ) = E2 + F - 1

*  Convert all the foreign format names to upper case and check that the
*  reserved data format name "NDF" has not been specified. Report an
*  error if it has.
                  F = FCB_FMT1( IFMT )
                  L = FCB_FMT2( IFMT )
                  CALL CHR_UCASE( FCB_FMT( F : L ) )
                  IF ( FCB_FMT( F : L ) .EQ. 'NDF' ) THEN
                     STATUS = NDF__FMTIN
                     CALL ERR_REP( 'NDF1_INFCB_NDF1',
     :                    'The format name ''NDF'' is reserved and ' //
     :                    'may not be used to identify a foreign ' //
     :                    'data format.', STATUS )
                     GO TO 2
                  END IF

*  Check whether the specified file extension will clash with that used
*  for native format NDF files (be case insensitive if necessary).
                  F = FCB_FEX1( IFMT )
                  L = FCB_FEX2( IFMT )
                  CALL NDF1_CMPFL( FCB_FMT( F : L ), '.sdf', SDF,
     :                             STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 2

*  Report an error if the file extension clashes.
                  IF ( SDF ) THEN
                     STATUS = NDF__FMTIN
                     CALL MSG_SETC( 'SDF', FCB_FMT( F : L ) )
                     CALL ERR_REP( 'NDF1_INFCB_SDF1',
     :                    'The file extension ''^SDF'' is reserved ' //
     :                    'and may not be used to identify a ' //
     :                    'foreign format data file.', STATUS )
                     GO TO 2
                  END IF
 1             CONTINUE
 2             CONTINUE
            END IF

*  If an error occurred, then report contextual information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'NDF1_INFCB_IN',
     : 'Error occurred while reading the NDF_FORMATS_IN foreign ' //
     : 'data format list (possible bad environment variable setting).',
     :                       STATUS )
            END IF
         END IF

*  Output formats.
*  ==============
*  In the same way, translate the environment variable NDF_FORMATS_OUT
*  to obtain a list of foreign data formats which are to be recognised
*  on output. Put the translation into the second half of the FCB
*  format list string. Note if there is no translation.
         CALL NDF1_GTENV( 'NDF_FORMATS_OUT', DEF,
     :                    FCB_FMT( NDF__SZFMT + 1 : ), LVAL, STATUS )

*  If no translation was found, then no foreign data formats will be
*  recognised on output. Otherwise, parse the format list to split it
*  into separate format fields, storing the field positions temporarily
*  at the end of the format name field position list.
         FCB_NOUT = 0
         IF ( ( STATUS .EQ. SAI__OK ) .AND. DEF ) THEN
            CALL NDF1_PSFFL( FCB_FMT( NDF__SZFMT + 1 : ), NDF__MXFMT,
     :                       FCB_FMT1( NDF__MXFMT + 1 ),
     :                       FCB_FMT2( NDF__MXFMT + 1 ), FCB_NOUT,
     :                       STATUS )

*  If OK, then loop to split each field into a foreign format name and
*  its associated file type extension string (which will be used to
*  identify files with that format).
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO 3 IFMT = NDF__MXFMT + 1, NDF__MXFMT + FCB_NOUT
                  F = FCB_FMT1( IFMT ) + NDF__SZFMT
                  L = FCB_FMT2( IFMT ) + NDF__SZFMT

*  If the format specified was simply '*' or '.', then accept it
*  without further checks and set both the format name and the file
*  extension position pointers to identify it.
                  IF ( ( FCB_FMT( F : L ) .EQ. '*' ) .OR.
     :                 ( FCB_FMT( F : L ) .EQ. '.' ) ) THEN
                     FCB_FMT1( IFMT ) = F
                     FCB_FMT2( IFMT ) = L
                     FCB_FEX1( IFMT ) = F
                     FCB_FEX2( IFMT ) = L

*  Otherwise parse the format specification.
                  ELSE
                     CALL NDF1_PSFMT( FCB_FMT( F : L ), F1, F2, E1, E2,
     :                                STATUS )

*  Quit looping if an error occurs.
                     IF ( STATUS .NE. SAI__OK ) GO TO 4

*  Correct the returned field positions for the starting position used
*  and store the results at the end of the FCB field position lists.
                     FCB_FMT1( IFMT ) = F1 + F - 1
                     FCB_FMT2( IFMT ) = F2 + F - 1
                     FCB_FEX1( IFMT ) = E1 + F - 1
                     FCB_FEX2( IFMT ) = E2 + F - 1

*  Convert all the foreign format names to upper case and check that the
*  reserved data format name "NDF" has not been specified. Report an
*  error if it has.
                     F = FCB_FMT1( IFMT )
                     L = FCB_FMT2( IFMT )
                     CALL CHR_UCASE( FCB_FMT( F : L ) )
                     IF ( FCB_FMT( F : L ) .EQ. 'NDF' ) THEN
                        STATUS = NDF__FMTIN
                        CALL ERR_REP( 'NDF1_INFCB_NDF2',
     :                       'The format name ''NDF'' is reserved ' //
     :                       'and may not be used to identify a ' //
     :                       'foreign data format.', STATUS )
                        GO TO 4
                     END IF

*  Check whether the specified file extension will clash with that used
*  for native format NDF files (be case insensitive if necessary).
                     F = FCB_FEX1( IFMT )
                     L = FCB_FEX2( IFMT )
                     CALL NDF1_CMPFL( FCB_FMT( F : L ), '.sdf', SDF,
     :                                STATUS )
                     IF ( STATUS .NE. SAI__OK ) GO TO 4

*  Report an error if the file extension clashes.
                     IF ( SDF ) THEN
                        STATUS = NDF__FMTIN
                        CALL MSG_SETC( 'SDF', FCB_FMT( F : L ) )
                        CALL ERR_REP( 'NDF1_INFCB_SDF2',
     :                       'The file extension ''^SDF'' is ' //
     :                       'reserved and may not be used to ' //
     :                       'identify a foreign format data file.',
     :                       STATUS )
                        GO TO 4
                     END IF
                  END IF
 3             CONTINUE
 4             CONTINUE
            END IF

*  If an error occurred, then report contextual information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'NDF1_INFCB_OUT',
     : 'Error occurred while reading the NDF_FORMATS_OUT foreign ' //
     : 'data format list (possible bad environment variable setting).',
     :                       STATUS )
            END IF
         END IF

*  Note if the first invocation of this routine has completed
*  successfully.
         IF ( STATUS .EQ. SAI__OK ) FIRST = .FALSE.

*  Call error tracing routine if necessary.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_INFCB',
     :                                               STATUS )
      END IF

      END
