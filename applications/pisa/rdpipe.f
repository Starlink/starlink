
      SUBROUTINE RDPIPE( LI, MAXENT, INDEX, PRATIO, IBYP, ELL, SXY,
     :                   NOBJ, STATUS )
*+
*  Name:
*     RDPICH - READ PISA PEAK DATA

*  Purpose:
*     To read in the data from an PISAPEAK results file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPICH( LI, MAXENT, INDEX, PRATIO, IBYP, ELL, SXY,
*                  NOBJ, STATUS )

*  Description:
*     The routine reads in the PISAPEAK data in FIO file LI, one line at
*     a time entering the values in the passed arrays. The lines are
*     parsed using word extraction routines not fortran formatting.

*  Arguments:
*     LI = INTEGER (Given)
*        The FIO file descriptor
*     MAXENT = INTEGER (Given)
*        The maximum number of entries allowed in the output data
*        arrays.
*     INDEX = INTEGER (Returned)
*        Array of the first column values (indices) in the file LI.
*     PRATIO = DOUBLE PRECISION (Returned)
*        Array of the second column values in the file the LI.
*     IBYP = DOUBLE PRECISION (Returned)
*        Array of the third column values in the file the LI.
*     ELL = DOUBLE PRECISION (Returned)
*        Array of the fourth column values in the file the LI.
*     SXY = DOUBLE PRECISION (Returned)
*        Array of the fifth column values in the file the LI.
*     NOBJ = INTEGER (Returned)
*        The number of good lines read in.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAR-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system error codes

*  Arguments Given:
      INTEGER LI
      INTEGER MAXENT

*  Arguments Returned:
      INTEGER INDEX( MAXENT )
      DOUBLE PRECISION PRATIO( MAXENT )
      DOUBLE PRECISION IBYP( MAXENT )
      DOUBLE PRECISION ELL( MAXENT )
      DOUBLE PRECISION SXY( MAXENT )
      INTEGER NOBJ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXWRD              ! maximum words returned from file line
                                 ! buffer
      PARAMETER ( MXWRD = 5 )
      INTEGER WRDLEN             ! maximum length of words returned
      PARAMETER ( WRDLEN = 14 )

*  Local Variables:
      INTEGER NREAD              ! number of characters read from file
      INTEGER NRET               ! actual number of words in buf
      INTEGER START( MXWRD )     ! starting positions of words located
      INTEGER STOP( MXWRD )      ! end positions of words located
      CHARACTER*( WRDLEN ) WORDS( MXWRD ) ! the first mxwrds if they
                                 ! exist
      INTEGER LSTAT              ! local status return
      CHARACTER BUF*132          ! buffer to read in line from data file
      REAL VAL1, VAL2, VAL3, VAL4 ! dummy variables to hold REAL values

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      NOBJ = 0
1     CONTINUE

*  Read in loop -- check that have not exceeded maximum number of
*  entries.
         IF ( NOBJ.GE. MAXENT ) THEN
            CALL MSG_SETI( 'MAXENT', MAXENT )
            CALL MSG_OUT( 'MAXENTS', ' The number of input objects has'
     :                    //' been restricted to ^MAXENT', STATUS)
          GO TO 2
        END IF

*  Read in the line from file
         CALL FIO_READ( LI, BUF, NREAD, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Read in line attempt to extract first three words
            CALL CHR_DCWRD( BUF, MXWRD, NRET, START, STOP, WORDS,
     :                      LSTAT )
            IF ( NRET .EQ. MXWRD ) THEN

*  Have all fields convert them into values
               CALL CHR_CTOI( WORDS( 1 ), INDEX( NOBJ + 1 ), STATUS )
               CALL CHR_CTOR( WORDS( 2 ), VAL1, STATUS )
               CALL CHR_CTOR( WORDS( 3 ), VAL2, STATUS )
               CALL CHR_CTOR( WORDS( 4 ), VAL3, STATUS )
               CALL CHR_CTOR( WORDS( 5 ), VAL4, STATUS )
            ELSE

*  Bad return probably a blank line skip it
               GO TO 1
            END IF

*  Increase object count
            NOBJ = NOBJ + 1

*  Add values to buffers, converting from reals to double precision
            PRATIO( NOBJ ) = DBLE( VAL1 )
            IBYP( NOBJ ) = DBLE( VAL2 )
            ELL( NOBJ ) = DBLE( VAL3 )
            SXY( NOBJ ) = DBLE( VAL4 )

*  Next line
            GO TO 1
         END IF

*  End of read in loop
2     CONTINUE

*  End of file - check status.
      IF ( STATUS .EQ. FIO__EOF ) THEN
          CALL ERR_ANNUL( STATUS )
      ENDIF

*  If no objects have been found then report error and set status
      IF ( NOBJ .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'NOENTRIES',
     :                'input file contains no valid entries',
     :                 STATUS )
      ENDIF
      END
* $Id$
