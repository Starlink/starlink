      SUBROUTINE PON_GETSEL( COLLAB, SELCOL, ICOND, CHRSEL, SELVAL1,
     :                       SELVAL2, NLIST, CLIST, STATUS )
*+
*  Name:
*     PON_GETSEL

*  Purpose:
*     Get the PONGO selection criteria.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_GETSEL( COLLAB, SELCOL, ICOND, CHRSEL, SELVAL1,
*    :                 SELVAL2, NLIST, CLIST, STATUS )

*  Description:
*     Accesses the ADAM parameter FULL. If this is true it gets the
*     selection criterion via ADAM parameter SELCOND and parses it to
*     determine the selection and return the various parts of the
*     criterion.

*  Arguments:
*     COLLAB( * ) = CHARACTER * ( * ) (Given)
*        The array containing the column labels within the file.
*     SELCOL = INTEGER (Returned)
*        The number of the selection column (0 indicates no selection).
*     ICOND = INTEGER (Returned)
*        The index number of the selection criterion:
*
*           1  '='   REAL equality
*           2  '#'   REAL inequality
*           3  '>'   REAL greater than
*           4  '<'   REAL less than
*           5  'CE'  CHARACTER equality
*           6  'C#'  CHARACTER inequality
*           7  'RA'  in range from SELVAL1 to SELVAL2
*           8  'LI'  equal to any one of following list
*           9  'IN'  substring contained in value
*           10 'A>'  absolute greater than
*           11 'A<'  absolute less than
*           12 'EX'  excluding the following list
*     CHRSEL = CHARACTER * ( * ) (Returned)
*        Character value of the selection criterion.
*     SELVAL1 = REAL (Returned)
*        Real value of the select criterion.
*     SELVAL2 = REAL (Returned)
*        Real value of the second select criterion.
*     NLIST = INTEGER (Returned)
*        Number of values in the character list.
*     CLIST( MAXSELST ) = CHARACTER * ( LENLAB ) (Returned)
*        The actual list of values (note the first item of the array is
*        the selection specifier).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     2-JUN-1994 (PDRAPER):
*        Removed unused variables.
*     6-JUN-1994 (PDRAPER):
*        Now uses PON_PARSE instead of PARSE.
*     20-MAR-1995 (PDRAPER):
*        Fixed to work for RA. This has never worked in a Starlink
*        release. The qualifiers were not parsed into parts!
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      CHARACTER * ( * ) COLLAB( * )

*  Arguments Returned:
      INTEGER SELCOL
      INTEGER ICOND

      CHARACTER*(*) CHRSEL

      REAL SELVAL1
      REAL SELVAL2

      INTEGER NLIST

      CHARACTER * ( LENLAB ) CLIST( MAXSELST )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER INTCMD             ! Return index

*  Local Constants:
      INTEGER NCOND              ! Number of conditions
      PARAMETER ( NCOND = 12 )

*  Local Variables:
      CHARACTER * ( 14 ) RANGE( 2 ) ! Range of value for RA selection
      CHARACTER * ( 2 ) CONDITIONS( NCOND + 1 )
      CHARACTER * ( 80 ) SELCOND ! Selection condition
      INTEGER NR                 ! Number of values in RA
      LOGICAL ALL                ! Switch to allow all to be read in
      REAL RTEMP

*  Local Data:
      DATA CONDITIONS / '=', '#', '>', '<', 'CE', 'C#', 'RA', 'LI',
     :                  'IN', 'A>', 'A<', 'EX', ' ' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SELCOL = 0
      CALL PAR_GET0L( 'ALL', ALL, STATUS )

*  If ALL flag is set, select condition will not be requested.
      IF ( .NOT. ALL ) THEN

*     If the select column is specified then get a select condition
         CALL PAR_GET0C( 'SELCOND', SELCOND, STATUS )
         CALL PON_PARSE( SELCOND, ' ', MAXSELST, CLIST, NLIST, STATUS )

*     Get the select column.
         CALL CHR_CTOI( CLIST( 1 ), SELCOL, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

*     ...see if it is a valid column name.
            SELCOL = INTCMD( COLLAB, CLIST( 1 ) )

            IF ( SELCOL .GT. 0 ) THEN
               STATUS = SAI__OK
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'PON_GETSEL_BADC',
     :                       'Selection column incorrectly specified.',
     :                       STATUS )
               NLIST = MAX( NLIST-2, 1 )
               GO TO 999
            END IF
         END IF

         IF ( SELCOL .GT. 0 ) THEN
            ICOND = INTCMD( CONDITIONS, CLIST( 2 ) )

            IF ( ( ICOND .GT. 0 )
     :           .AND. ( NLIST .GE. 2 ) ) THEN
               CHRSEL = CLIST( 3 )
               IF ( ( ICOND .LE. 4 )
     :              .OR. ( ICOND .EQ. 10 )
     :              .OR. ( ICOND .EQ. 11 ) ) THEN

*  Read a single value
                  CALL CHR_CTOR( CLIST( 3 ), SELVAL1, STATUS )
                  CALL MSG_SETC( 'SELVAL', CLIST( 3 ) )
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_REP(
     :                                          'PON_GETSEL_BADV',
     :                                          '^SELVAL not ' //
     :                                          'recognised.', STATUS )
               ELSE IF ( ICOND .EQ. 7 ) THEN

*  Select from RANGE. This is in the last word.
                  IF ( NLIST .EQ. 3 ) THEN
                     CALL PON_PARSE( CLIST( 3 ), ',', 2, RANGE, NR,
     :                               STATUS )
                     IF ( NR .EQ. 2 ) THEN

*  Have two comma separated items at end of selection criteria. Extra
*  these as real values.
                        CALL CHR_CTOR( RANGE( 1 ), SELVAL1, STATUS )
                        CALL CHR_CTOR( RANGE( 2 ), SELVAL2, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           CLIST( 3 ) = RANGE( 1 )
                           CLIST( 4 ) = RANGE( 2 )
                           NLIST = 4
                        END IF
                     ELSE

*  Invalid range specified.
                        STATUS = SAI__ERROR
                     END IF
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL MSG_SETC( 'SELVAL', CLIST( 3 ) )
                        CALL ERR_REP( 'PON_GETSEL_BADR', 'Could not ' //
     :                       'recognise range of ' //
     :                       'values ''^SELVAL''as numeric range ' //
     :                       '(LOW,HIGH).',
     :                       STATUS )
                     ELSE IF ( SELVAL1 .GT. SELVAL2 ) THEN
                        RTEMP = SELVAL1
                        SELVAL1 = SELVAL2
                        SELVAL2 = RTEMP
                     END IF
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'SELVAL', CLIST( 3 ) )
                     CALL ERR_REP( 'PON_GETSEL_BADR', 'Could not ' //
     :                    'recognise range of values (LOW,HIGH) in ' //
     :                    'string ''^SELVAL''.  Invalid number of ' //
     :                    'elements.',
     :                    STATUS )
                  END IF
               END IF
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'PON_GETSEL_BADS',
     :                       'Bad selection criterion.', STATUS )
            END IF
         END IF

*     Set NLIST appropriately.
         NLIST = MAX( NLIST-2, 1 )
      END IF

 999  CONTINUE

      END
* $Id$
