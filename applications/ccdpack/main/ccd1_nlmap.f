      SUBROUTINE CCD1_NLMAP( FD, LINE, LINLEN, IP, NREC, NVAL, STATUS )
*+
*  Name:
*     CCD1_NLMAP

*  Purpose:
*     Reads in all the data in file FD, writing the values to
*     dynamic workspace. (non-standard list)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NLMAP( FD, LINE, LINLEN, IP, NREC, NVAL, STATUS )

*  Description:
*     This routine reads in the data in the formatted file pointed to
*     by the FIO system file descriptor FD. The data is read into a
*     single dynamic memory array which is pointed to by IP on exit.
*     Using this routine allows lists of formatted data to be read in
*     without fixed array restrictions.
*
*     The record structure is just a fixed number of values which may be
*     represented using double precision. This is NOT the usual CCDPACK
*     standard of a integer identifier followed by values.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO system file descriptor. This file contains the lists
*        of values which are required.
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        A character buffer which will be used to contain the
*        lines read from the input file.
*     LINLEN = INTEGER (Given)
*        Length of the character buffer.
*     IP = INTEGER (Returned)
*        Pointer to a DOUBLE PRECISION array (NREC,NVAL) large. This
*        array contains the data read in from the input file.
*     NREC = INTEGER (Returned)
*        The number of records read from the input file.
*     NVAL = INTEGER (Returned)
*        The number of values which are read from the input file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FD
      INTEGER LINLEN

*  Arguments Given and Returned:
      CHARACTER * ( * ) LINE

*  Arguments Returned:
      INTEGER IP
      INTEGER NREC
      INTEGER NVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FNAME   ! File name
      INTEGER LINNUM             ! Current line number
      INTEGER NACT               ! Actual number of values last time
      INTEGER NCHAR              ! Number of characters in line
      INTEGER NLINES             ! Number of lines (records) read from
                                 ! file
      INTEGER NUMVAL             ! Number of values this time
      LOGICAL EOF                ! End-of-file flag
      LOGICAL HAVIND             ! Flag showing if identfier values are
                                 ! present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the file.
      CALL FIO_FNAME( FD, FNAME, STATUS )

*  Read the file once to estimate the number of records and the
*  number of values.
      LINNUM = 0
      NLINES = 0
      EOF = .FALSE.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN
         CALL CCD1_RDLIN( FD, LINLEN, LINE, NCHAR, LINNUM, EOF,
     :                    STATUS )

*  Decode line to find out how many values it has.
         IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN 
            CALL CCD1_DECL2( LINE, LINNUM, HAVIND, NUMVAL, STATUS )

*  Correct for removal of index value from count.
            IF ( HAVIND ) NUMVAL = NUMVAL + 1

*  If status is BAD issue filename with error message
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'FILENAME', FNAME )
               CALL ERR_REP( 'FILEREADERR',
     :         '  Error reading file ^FILENAME', STATUS )

*  Stop loop.
               GO TO 1
            END IF

*  Increment the record count.
            NLINES = NLINES + 1

*  Check that the number of values is invariant.
            IF ( NLINES .NE. 1 ) THEN 
               IF ( .NOT. ( NUMVAL .EQ. NACT ) ) THEN

*  Number of values has changed. Set STATUS and issue error. 
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CCD1_LMAPERR1',
     :            '  File has illegal record structure', STATUS )
                  CALL MSG_SETI( 'LINNUM', LINNUM )
                  CALL ERR_REP( 'CCD1_LMAPERR1',
     :            '  Variable number of values - line ^LINNUM', STATUS )
                  CALL MSG_SETC( 'FILENAME', FNAME )
                  CALL ERR_REP( 'FILEREADERR',
     :            '  Error reading file ^FILENAME', STATUS )

*  Stop loop.
                  GO TO 1
               END IF
            END IF

*  Store this NUMVAL for reference
            
             NACT = NUMVAL
          END IF

*  Return for next line.
         GO TO 1
      END IF

*  Trap any errors before proceeding.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Set the output memory sizes.
      NVAL = NUMVAL
      NREC = NLINES

*  Allocate the space necessary read in the data.
      CALL CCD1_MALL( NLINES * NUMVAL, '_DOUBLE', IP, STATUS )

*  Rewind input file.
      CALL FIO_RWIND( FD, STATUS )

*  Read in the values.
      LINNUM = 0
      NLINES = 0
      EOF = .FALSE.
 2    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN
         CALL CCD1_RDLIN( FD, LINLEN, LINE, NCHAR, LINNUM, EOF,
     :                    STATUS )

*  Decode line into its values.
         IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN 

*  Increment the record count.
            NLINES = NLINES + 1
            CALL CCD1_DECL4( LINE, LINNUM, NREC, NVAL, NLINES,
     :                       %VAL( IP ), STATUS )

*  If status is BAD issue filename with error message
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'FILENAME', FNAME )
               CALL ERR_REP( 'FILEREADERR',
     :         '  Error reading file ^FILENAME', STATUS )

*  Stop loop.
               GO TO 2
            END IF
          END IF

*  Return for next line.
         GO TO 2
      END IF

 99   CONTINUE
      END
* $Id$
