      SUBROUTINE NDF_HINFO( INDF, ITEM, IREC, VALUE, STATUS )
*+
*  Name:
*     NDF_HINFO

*  Purpose:
*     Obtain information about an NDF's history component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HINFO( INDF, ITEM, IREC, VALUE, STATUS )

*  Description:
*     The routine returns character information about an NDF's history
*     component or about one of the history records it contains.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the information item required: 'APPLICATION',
*        'CREATED', 'DATE', 'DEFAULT', 'HOST', 'MODE', 'NLINES',
*        'NRECORDS', 'REFERENCE', 'USER', 'WIDTH' or 'WRITTEN' (see the
*        "General Items" and "Specific Items" sections for details).
*        This value may be abbreviated, to no less than three
*        characters.
*     IREC = INTEGER (Given)
*        History record number for which information is required. This
*        argument is ignored if information is requested about the
*        history component as a whole. See the "Specific Items" section
*        for details of which items require this argument.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The history information requested (see the "Returned String
*        Lengths" section for details of the length of character
*        variable required to receive this value).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  General Items:
*     The following ITEM values request general information about the
*     history component and do not use the IREC argument:
*     -  'CREATED': return a string giving the date and time of
*     creation of the history component as a whole in the format
*     'YYYY-MMM-DD HH:MM:SS.SSS' (e.g. '1993-JUN-16 11:30:58.001').
*     -  'DEFAULT': return a logical value indicating whether default
*     history information has yet to be written for the current
*     application. A value of 'F' is returned if it has already been
*     written or has been suppressed by a previous call to NDF_HPUT,
*     otherwise the value 'T' is returned.
*     -  'MODE': return the current update mode of the history
*     component (one of the strings 'DISABLED', 'QUIET', 'NORMAL' or
*     'VERBOSE').
*     -  'NRECORDS': return the number of history records present (an
*     integer formatted as a character string). Note that for
*     convenience this value may also be obtained directly as an
*     integer via the routine NDF_HNREC.
*     -  'WRITTEN': return a logical value indicating whether the
*     current application has written a new history record to the NDF's
*     history component. A value of 'T' is returned if a new record has
*     been written, otherwise 'F' is returned.

*  Specific Items:
*     The following ITEM values request information about specific
*     history records and should be accompanied by a valid value for
*     the IREC argument specifying the record for which information is
*     required:
*     -  'APPLICATION': return the name of the application which
*     created the history record.
*     -  'DATE': return a string giving the date and time of creation
*     of the specified history record in the format 'YYYY-MMM-DD
*     HH:MM:SS.SSS' (e.g. '1993-JUN-16 11:36:09.021').
*     -  'HOST': return the name of the machine on which the
*     application which wrote the history record was running (if this
*     has not been recorded, then a blank value is returned).
*     -  'NLINES': return the number of lines of text contained in the
*     history record (an integer formatted as a character string).
*     -  'REFERENCE': return a name identifying the NDF dataset in which
*     the history component resided at the time the record was written
*     (if this has not been recorded, then a blank value is returned).
*     This value is primarily of use in identifying the ancestors of a
*     given dataset when history information has been repeatedly
*     propagated through a sequence of processing steps.
*     -  'USER': return the user name for the process which wrote the
*     history record (if this has not been recorded, then a blank value
*     is returned).
*     -  'WIDTH': return the width in characters of the text contained
*     in the history record (an integer formatted as a character
*     string).

*  Returned String Lengths:
*     -  If ITEM is set to 'CREATED', 'DATE', 'MODE', 'NLINES',
*     'NRECORDS' or 'WIDTH', then an error will result if the length of
*     the VALUE argument is too short to accommodate the returned
*     result without losing significant (non-blank) trailing
*     characters.
*     -  If ITEM is set to 'APPLICATION', 'HOST', 'REFERENCE' or 'USER',
*     then the returned value will be truncated with an ellipsis '...'
*     if the length of the VALUE argument is too short to accommodate
*     the returned result without losing significant (non-blank)
*     trailing characters. No error will result.
*     -  When declaring the length of character variables to hold the
*     returned result, the constant NDF__SZHDT may be used for the
*     length of returned date/time strings for the 'CREATED' and 'DATE'
*     items, the constant NDF__SZHUM may be used for the length of
*     returned update mode strings for the 'MODE' item, and the
*     constant VAL__SZI may be used for the length of returned integer
*     values formatted as character strings.
*     -  Use of the constant NDF__SZAPP is recommended when declaring
*     the length of a character variable to hold the returned
*     application name for the 'APPLICATION' item. Similarly, use of
*     the constant NDF__SZHST is recommended when requesting the 'HOST'
*     item, NDF__SZREF when requesting the 'REFERENCE' item and
*     NDF__SZUSR when requesting the 'USER' item. Truncation of the
*     returned values may still occur, however, if longer strings were
*     specified when the history record was created.
*     -  The NDF__SZAPP, NDF__SZHDT, NDF__SZHST, NDF__SZHUM, NDF__SZREF
*     and NDF__SZUSR constants are defined in the include file NDF_PAR.
*     The VAL__SZI constant is defined in the include file PRM_PAR (see
*     SUN/39).

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
*     5-MAY-1993 (RFWS):
*        Original version.
*     19-MAY-1993 (RFWS):
*        Added support for further information items.
*     4-AUG-1993 (RFWS):
*        Added the DEFAULT and WRITTEN information items and changed to
*        return all date/time strings in standard format.
*     27-SEP-1993 (RFWS):
*        Improved error messages.
*     1-OCT-1993 (RFWS):
*        Added the HOST and USER items.
*     26-APR-1994 (RFWS):
*        Added the REFERENCE item.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Read)
*           Whether default history information is to be written.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for array of history records.
*        DCB_HTLEN( NDF__MXDCB ) = INTEGER (Read)
*           History record text width in characters.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) ITEM
      INTEGER IREC

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELL ! Array cell locator
      CHARACTER * ( DAT__SZLOC ) LOC ! Component locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component data type string
      CHARACTER * ( NDF__SZHDT ) CREATD ! Creation date/time string
      CHARACTER * ( NDF__SZHDT ) DATE ! History record date/time string
      CHARACTER * ( NDF__SZHUM ) HMODE ! History update mode string
      CHARACTER * ( VAL__SZI ) NLINES ! Number of history record lines
      CHARACTER * ( VAL__SZI ) NREC ! Number of history records
      CHARACTER * ( VAL__SZI ) WIDTH ! Text width of history record
      INTEGER CLEN               ! Character string length
      INTEGER DIM( DAT__MXDIM )  ! Component dimension sizes
      INTEGER IACB               ! Index of NDF entry in the ACB
      INTEGER IDCB               ! Index of data object entry in the DCB
      INTEGER NC                 ! Number of formatted characters
      INTEGER NDIM               ! Number of component dimensions
      INTEGER PNTR               ! Pointer to mapped value
      INTEGER SUB( 1 )           ! Cell subscript
      INTEGER YMDHM( 5 )         ! Date/time integer field values
      LOGICAL THERE              ! Is component present?
      REAL SEC                   ! Seconds field value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If OK, obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that history information is available in the DCB.
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no history component present in the NDF, then report an
*  error.
            IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOHIS
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_HINFO_NOHIS',
     :                       'There is no history component present ' //
     :                       'in the NDF structure ^NDF (possible ' //
     :                       'programming error).', STATUS )

*  CREATED.
*  =======
*  If the creation date is required, then obtain a locator to the
*  CREATED component in the history structure (note we do not validate
*  this component, since this will already have been done by the
*  NDF1_DH routine). Map it for reading and determine its character
*  string length.
            ELSE IF ( NDF1_SIMLR( ITEM, 'CREATED', NDF__MINAB ) ) THEN
               CALL DAT_FIND( DCB_HLOC( IDCB ), 'CREATED', LOC, STATUS )
               CALL DAT_MAPC( LOC, 'READ', 0, DIM, PNTR, STATUS )
               CALL DAT_CLEN( LOC, CLEN, STATUS )

*  If OK, parse the resulting string to obtain the creation date and
*  time.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_PSHDT( %VAL( CNF_PVAL( PNTR ) ), YMDHM, SEC,
     :                             STATUS, %VAL( CNF_CVAL( CLEN ) ) )

*  If an error occurred, then report contextual information.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )
                     CALL ERR_REP( 'NDF_HINFO_CRE',
     :                             'Error reading the history ' //
     :                             'creation date from the CREATED ' //
     :                             'component in the NDF history ' //
     :                             'structure ^HIST', STATUS )
                  END IF
               END IF

*  Annul the component locator (thereby unmapping it).
               CALL DAT_ANNUL( LOC, STATUS )

*  Re-format the creation date and time into standard history date/time
*  format and return the resulting string.
               CALL NDF1_FMHDT( YMDHM, SEC, CREATD, STATUS )
               CALL NDF1_CCPY( CREATD, VALUE, STATUS )

*  DEFAULT.
*  =======
*  If the default history writing flag is required, then return 'T' or
*  'F' to reflect its current value.
            ELSE IF ( NDF1_SIMLR( ITEM, 'DEFAULT', NDF__MINAB ) ) THEN
               IF ( DCB_HDEF( IDCB ) ) THEN
                  VALUE = 'T'
               ELSE
                  VALUE = 'F'
               END IF

*  MODE.
*  ====
*  If the history update mode is required, then obtain a character
*  string appropriate to the setting stored in the DCB.
            ELSE IF ( NDF1_SIMLR( ITEM, 'MODE', NDF__MINAB ) ) THEN
               IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HDISA ) THEN
                  HMODE = 'DISABLED'
               ELSE IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HQUIE ) THEN
                  HMODE = 'QUIET'
               ELSE IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HNORM ) THEN
                  HMODE = 'NORMAL'
               ELSE IF ( DCB_HUMOD( IDCB ) .EQ. NDF__HVERB ) THEN
                  HMODE = 'VERBOSE'

*  If the DCB entry is not recognised, then report an error.
               ELSE
                  STATUS = NDF__FATIN
                  CALL MSG_SETI( 'BADHUM', DCB_HUMOD( IDCB ) )
                  CALL ERR_REP( 'NDF_HINFO_HUM',
     :                          'Invalid history update mode code ' //
     :                          '(^BADHUM) encountered in the NDF_  ' //
     :                          'system Data Control Block ' //
     :                          '(internal programming error).',
     :                          STATUS )
               END IF

*  Return the value.
               CALL NDF1_CCPY( HMODE, VALUE, STATUS )

*  NRECORDS.
*  ========
*  If the number of history records is required, then format this value
*  as a character string. Return the value.
            ELSE IF ( NDF1_SIMLR( ITEM, 'NRECORDS', NDF__MINAB ) ) THEN
               CALL CHR_ITOC( DCB_HNREC( IDCB ), NREC, NC )
               CALL NDF1_CCPY( NREC( : NC ), VALUE, STATUS )

*  WRITTEN.
*  =======
*  If the information required is whether the history record has been
*  written to by the current application, then examine the current
*  history record text width in the DCB and return 'T' or 'F' as
*  appropriate.
            ELSE IF ( NDF1_SIMLR( ITEM, 'WRITTEN', NDF__MINAB ) ) THEN
               IF ( DCB_HTLEN( IDCB ) .NE. 0 ) THEN
                  VALUE = 'T'
               ELSE
                  VALUE = 'F'
               END IF

*  Validate IREC.
*  =============
*  If none of the above items were requested, then validate the history
*  record argument, which is required by all the later items.  First
*  check that the history record number supplied is greater than zero
*  and report an error if it is not.
            ELSE IF ( IREC .LE. 0 ) THEN
               STATUS = NDF__HRNIN
               CALL MSG_SETI( 'BADREC', IREC )
               CALL ERR_REP( 'NDF_HINFO_IREC1',
     :                       'Invalid history record number ' //
     :                       '^BADREC specified; this value ' //
     :                       'should be at least 1 (possible ' //
     :                       'programming error).', STATUS )

*  Then check that the record number does not exceed the number of
*  records actually present and report an error if it does.
            ELSE IF ( IREC .GT. DCB_HNREC( IDCB ) ) THEN
               STATUS = NDF__HRNIN
               CALL MSG_SETI( 'BADREC', IREC )
               CALL MSG_SETI( 'NREC', DCB_HNREC( IDCB ) )
               CALL DAT_MSG( 'HIST', DCB_HLOC( IDCB ) )

*  Adjust the error message according to how many records are actually
*  present.
               IF ( DCB_HNREC( IDCB ) .EQ. 0 ) THEN
                  CALL ERR_REP( 'NDF_HINFO_IREC2',
     :                          'Invalid history record number ' //
     :                          '^BADREC specified; there are no ' //
     :                          'history records present in ' //
     :                          'the NDF history structure ^HIST ' //
     :                          '(possible programming error).',
     :                          STATUS )
               ELSE IF ( DCB_HNREC( IDCB ) .EQ. 1 ) THEN
                  CALL ERR_REP( 'NDF_HINFO_IREC2',
     :                          'Invalid history record number ' //
     :                          '^BADREC specified; there is ' //
     :                          'only 1 history record present ' //
     :                          'in the NDF history structure ^HIST ' //
     :                          '(possible programming error).',
     :                          STATUS )
               ELSE
                  CALL ERR_REP( 'NDF_HINFO_IREC2',
     :                          'Invalid history record number ' //
     :                          '^BADREC specified; there are ' //
     :                          'only ^NREC history records ' //
     :                          'present in the NDF history ' //
     :                          'structure ^HIST (possible ' //
     :                          'programming error).', STATUS )
               END IF

*  For subsequent items, we must obtain information about an individual
*  history record.
            ELSE

*  APPLICATION.
*  ===========
*  If the history record application name is required, then locate the
*  required record array cell and check whether the mandatory COMMAND
*  component is present.
               IF ( NDF1_SIMLR( ITEM, 'APPLICATION', NDF__MINAB ) ) THEN
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )
                  CALL DAT_THERE( CELL, 'COMMAND', THERE, STATUS )

*  If the COMMAND component is absent, then report an error.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        STATUS = NDF__NOHCM
                        CALL DAT_MSG( 'STRUCT', CELL )
                        CALL ERR_REP( 'NDF_HINFO_COMM',
     :                                'The COMMAND component is ' //
     :                                'missing from the NDF history ' //
     :                                'record structure ^STRUCT',
     :                                STATUS )

*  Otherwise, obtain a locator to the COMMAND component and determine
*  its type and shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'COMMAND', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the COMMAND component is of type '_CHAR' and report an
*  error if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF_HINFO_CTYPE',
     :                                      'The COMMAND component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC has an invalid ' //
     :                                      'type of ''^BADTYPE''; ' //
     :                                      'it should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the COMMAND component is scalar and report an error
*  if it is not.
                           ELSE IF ( NDIM .NE. 0 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF_HINFO_CNDIM',
     :                                      'The COMMAND component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be scalar.',
     :                                      STATUS )
                           END IF
                        END IF

*  Mark the error stack and read the value of the COMMAND component.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           CALL ERR_MARK
                           CALL DAT_GET0C( LOC, VALUE, STATUS )

*  If character string truncation occurred, then annul the error and
*  append an ellipsis to the returned value.
                           IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :                          ( STATUS .EQ. DAT__TRUNC ) ) THEN
                              CALL ERR_ANNUL( STATUS )
                              VALUE( MAX( 1, LEN( VALUE ) - 2 ) : ) =
     :                           '...'
                           END IF

*  Release the error stack and annul the component locator.
                           CALL ERR_RLSE
                        END IF
                        CALL DAT_ANNUL( LOC, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )

*  DATE.
*  ====
*  If the history record date is required, then obtain its date and
*  time field values.
               ELSE IF ( NDF1_SIMLR( ITEM, 'DATE', NDF__MINAB ) ) THEN
                  CALL NDF1_GTHDT( IDCB, IREC, YMDHM, SEC, STATUS )

*  Re-format the date and time into standard history date/time format
*  and return the resulting string.
                  CALL NDF1_FMHDT( YMDHM, SEC, DATE, STATUS )
                  CALL NDF1_CCPY( DATE, VALUE, STATUS )

*  HOST.
*  ====
*  If the history record host machine node name is required, then
*  locate the required record array cell and check whether the optional
*  HOST component is present.
               ELSE IF ( NDF1_SIMLR( ITEM, 'HOST', NDF__MINAB ) ) THEN
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )
                  CALL DAT_THERE( CELL, 'HOST', THERE, STATUS )

*  If the HOST component is absent, then return a blank result.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        VALUE = ' '

*  Otherwise, obtain a locator to the HOST component and determine its
*  type and shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'HOST', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the HOST component is of type '_CHAR' and report an error
*  if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF_HINFO_HTYPE',
     :                                      'The HOST component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC has an invalid ' //
     :                                      'type of ''^BADTYPE''; ' //
     :                                      'it should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the HOST component is scalar and report an error if
*  it is not.
                           ELSE IF ( NDIM .NE. 0 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF_HINFO_HNDIM',
     :                                      'The HOST component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be scalar.',
     :                                      STATUS )
                           END IF
                        END IF

*  Mark the error stack and read the value of the HOST component.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           CALL ERR_MARK
                           CALL DAT_GET0C( LOC, VALUE, STATUS )

*  If character string truncation occurred, then annul the error and
*  append an ellipsis to the returned value.
                           IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :                          ( STATUS .EQ. DAT__TRUNC ) ) THEN
                              CALL ERR_ANNUL( STATUS )
                              VALUE( MAX( 1, LEN( VALUE ) - 2 ) : ) =
     :                           '...'
                           END IF

*  Release the error stack and annul the component locator.
                           CALL ERR_RLSE
                        END IF
                        CALL DAT_ANNUL( LOC, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )

*  NLINES.
*  ======
*  If the number of lines of history information is required, then
*  locate the required record array cell and check whether the
*  mandatory TEXT component is present.
               ELSE IF ( NDF1_SIMLR( ITEM, 'NLINES', NDF__MINAB ) ) THEN
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )
                  CALL DAT_THERE( CELL, 'TEXT', THERE, STATUS )

*  If the TEXT component is absent, then report an error.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        STATUS = NDF__NOHTX
                        CALL DAT_MSG( 'STRUCT', CELL )
                        CALL ERR_REP( 'NDF_HINFO_TEXT1',
     :                                'The TEXT component is ' //
     :                                'missing from the NDF history ' //
     :                                'record structure ^STRUCT',
     :                                STATUS )

*  Otherwise, obtain a locator to the TEXT component and determine its
*  type and shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'TEXT', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the TEXT component is of type '_CHAR' and report an error
*  if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF_HINFO_TT1',
     :                                      'The TEXT component in ' //
     :                                      'the NDF history record ' //
     :                                      'structure ^STRUC has ' //
     :                                      'an invalid type of ' //
     :                                      '''^BADTYPE''; it ' //
     :                                      'should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the TEXT component is 1-dimensional and report an
*  error if it is not.
                           ELSE IF ( NDIM .NE. 1 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF_HINFO_TND1',
     :                                      'The TEXT component in ' //
     :                                      'the NDF history record ' //
     :                                      'structure ^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be ' //
     :                                      '1-dimensional.', STATUS )
                           END IF
                        END IF

*  Annul the TEXT locator.
                        CALL DAT_ANNUL( LOC, STATUS )

*  Format the number of text lines as a character string. Return the
*  value.
                        CALL CHR_ITOC( DIM( 1 ), NLINES, NC )
                        CALL NDF1_CCPY( NLINES( : NC ), VALUE, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )

*  REFERENCE.
*  =========
*  If the history record dataset reference name is required, then locate
*  the required record array cell and check whether the optional DATASET
*  component is present.
               ELSE IF ( NDF1_SIMLR( ITEM, 'REFERENCE',
     :                               NDF__MINAB ) ) THEN
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )
                  CALL DAT_THERE( CELL, 'DATASET', THERE, STATUS )

*  If the DATASET component is absent, then return a blank result.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        VALUE = ' '

*  Otherwise, obtain a locator to the DATASET component and determine
*  its type and shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'DATASET', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the DATASET component is of type '_CHAR' and report an
*  error if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF_HINFO_DTYPE',
     :                                      'The DATASET component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC has an invalid ' //
     :                                      'type of ''^BADTYPE''; ' //
     :                                      'it should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the DATASET component is scalar and report an error
*  if it is not.
                           ELSE IF ( NDIM .NE. 0 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF_HINFO_DNDIM',
     :                                      'The DATASET component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be scalar.',
     :                                      STATUS )
                           END IF
                        END IF

*  Mark the error stack and read the value of the DATASET component.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           CALL ERR_MARK
                           CALL DAT_GET0C( LOC, VALUE, STATUS )

*  If character string truncation occurred, then annul the error and
*  append an ellipsis to the returned value.
                           IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :                          ( STATUS .EQ. DAT__TRUNC ) ) THEN
                              CALL ERR_ANNUL( STATUS )
                              VALUE( MAX( 1, LEN( VALUE ) - 2 ) : ) =
     :                           '...'
                           END IF

*  Release the error stack and annul the component locator.
                           CALL ERR_RLSE
                        END IF
                        CALL DAT_ANNUL( LOC, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )

*  USER.
*  ====
*  If the history record user name is required, then locate the
*  required record array cell and check whether the optional USER
*  component is present.
               ELSE IF ( NDF1_SIMLR( ITEM, 'USER', NDF__MINAB ) ) THEN
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )
                  CALL DAT_THERE( CELL, 'USER', THERE, STATUS )

*  If the USER component is absent, then return a blank result.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        VALUE = ' '

*  Otherwise, obtain a locator to the USER component and determine its
*  type and shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'USER', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the USER component is of type '_CHAR' and report an error
*  if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF_HINFO_UTYPE',
     :                                      'The USER component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC has an invalid ' //
     :                                      'type of ''^BADTYPE''; ' //
     :                                      'it should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the USER component is scalar and report an error if
*  it is not.
                           ELSE IF ( NDIM .NE. 0 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF_HINFO_UNDIM',
     :                                      'The USER component ' //
     :                                      'in the NDF history ' //
     :                                      'record structure ' //
     :                                      '^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be scalar.',
     :                                      STATUS )
                           END IF
                        END IF

*  Mark the error stack and read the value of the USER component.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           CALL ERR_MARK
                           CALL DAT_GET0C( LOC, VALUE, STATUS )

*  If character string truncation occurred, then annul the error and
*  append an ellipsis to the returned value.
                           IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :                          ( STATUS .EQ. DAT__TRUNC ) ) THEN
                              CALL ERR_ANNUL( STATUS )
                              VALUE( MAX( 1, LEN( VALUE ) - 2 ) : ) =
     :                           '...'
                           END IF

*  Release the error stack and annul the component locator.
                           CALL ERR_RLSE
                        END IF
                        CALL DAT_ANNUL( LOC, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )

*  WIDTH.
*  =====
*  If the history text width is required, then locate the required
*  record array cell and check whether the mandatory TEXT component is
*  present.
               ELSE IF ( NDF1_SIMLR( ITEM, 'WIDTH', NDF__MINAB ) ) THEN
                  SUB( 1 ) = IREC
                  CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL,
     :                           STATUS )
                  CALL DAT_THERE( CELL, 'TEXT', THERE, STATUS )

*  If the TEXT component is absent, then report an error.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. THERE ) THEN
                        STATUS = NDF__NOHTX
                        CALL DAT_MSG( 'STRUCT', CELL )
                        CALL ERR_REP( 'NDF_HINFO_TEXT2',
     :                                'The TEXT component is ' //
     :                                'missing from the NDF history ' //
     :                                'record structure ^STRUCT',
     :                                STATUS )

*  Otherwise, obtain a locator to the TEXT component and determine its
*  type and shape.
                     ELSE
                        CALL DAT_FIND( CELL, 'TEXT', LOC, STATUS )
                        CALL DAT_TYPE( LOC, TYPE, STATUS )
                        CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM,
     :                                  STATUS )

*  Check that the TEXT component is of type '_CHAR' and report an error
*  if it is not.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = NDF__TYPIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'NDF_HINFO_TT2',
     :                                      'The TEXT component in ' //
     :                                      'the NDF history record ' //
     :                                      'structure ^STRUC has ' //
     :                                      'an invalid type of ' //
     :                                      '''^BADTYPE''; it ' //
     :                                      'should be of type ' //
     :                                      '''_CHAR''.', STATUS )

*  Also check that the TEXT component is 1-dimensional and report an
*  error if it is not.
                           ELSE IF ( NDIM .NE. 1 ) THEN
                              STATUS = NDF__NDMIN
                              CALL DAT_MSG( 'STRUC', CELL )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'NDF_HINFO_TND2',
     :                                      'The TEXT component in ' //
     :                                      'the NDF history record ' //
     :                                      'structure ^STRUC is ' //
     :                                      '^BADNDIM-dimensional; ' //
     :                                      'it should be ' //
     :                                      '1-dimensional.', STATUS )
                           END IF
                        END IF

*  Obtain the length of the TEXT lines and annul the TEXT locator.
                        CALL DAT_CLEN( LOC, CLEN, STATUS )
                        CALL DAT_ANNUL( LOC, STATUS )

*  Format the number of text lines as a character string and return the
*  value.
                        CALL CHR_ITOC( CLEN, WIDTH, NC )
                        CALL NDF1_CCPY( WIDTH( : NC ), VALUE, STATUS )
                     END IF
                  END IF

*  Annul the history record cell locator.
                  CALL DAT_ANNUL( CELL, STATUS )

*  If the item was not recognised, then report an error.
               ELSE
                  STATUS = NDF__HITIN
                  CALL MSG_SETC( 'BADITEM', ITEM )
                  CALL ERR_REP( 'NDF_HINFO_ITEM',
     :                          'Invalid history information item ' //
     :                          '''^BADITEM'' specified (possible ' //
     :                          'programming error).', STATUS )
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HINFO_ERR',
     :   'NDF_HINFO: Error obtaining information about an NDF''s ' //
     :   'history component.', STATUS )
         CALL NDF1_TRACE( 'NDF_HINFO', STATUS )
      END IF

      END
