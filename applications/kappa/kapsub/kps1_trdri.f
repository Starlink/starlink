      SUBROUTINE KPS1_TRDRI( FD, DIM1, DIM2, POSCOD, COUNT, CODATA,
     :                        VADATA, LBND, UBND, CMPLET, STATUS )

*+
*  Name:
*     KPS1_TRDRx

*  Purpose:
*     Reads co-ordinate and value data from an ASCII free-format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_TRDRx( FD, DIM1, DIM2, POSCOD, COUNT, CODATA, VADATA,
*    :                 LBND, UBND, CMPLET, STATUS )

*  Description:
*     This routine reads co-ordinate information and data value from
*     an ASCII free-format file.  The file is like a relational
*     catalogue with one record per related set of co-ordinate and data
*     values, however, exact column alignment is not necessary.  The
*     input data are copied to two arrays, the first uses a new line
*     for each record of the file, its columns stores the given
*     positional data in the order x,y,z etc. The second stores the
*     data value. If the end of file is not reached a flag is returned.

*  Arguments:
*     FD = INTEGER (Given)
*        Fortran file identifier.
*     DIM1 = INTEGER (Given)
*        The first dimension of the output array, equal to the
*        dimensionality of the output NDF.
*     DIM2 = INTEGER (Given)
*        The second dimension of the output array.
*     POSCOD( DIM1+1 ) = INTEGER (Given)
*        The column numbers of the co-ordinate information in order
*        x,y,z, etc. followed by the column number of the data value.
*        They must be positive.
*     COUNT = INTEGER (Given and Returned)
*        The number of the line in the output array to be written first.
*        If the file has been completely read this becomes the number
*        of data sets stored in the array (i.e. one less).  It should
*        be initialised externally the first time this routine is
*        called, but not subsequently.
*     CODATA( DIM1, DIM2 ) = REAL (Given and Returned)
*        The array to store the co-ordinates read from the ASCII file.
*        It should be initialised externally the first time this routine
*        is called, but not subsequently.
*     VADATA( DIM2 ) = ? (Given and Returned)
*        The array to store the data values read from the ASCII file.
*        It should be initialised externally the first time this routine
*        is called, but not subsequently.
*     LBND( DIM1 ) = REAL (Given and Returned)
*        The lower bounds of the input data, i.e. the minimum value for
*        each co-ordinate.  It should be initialised externally the
*        first time this routine is called, but not subsequently.
*     UBND( DIM1 ) = REAL (Given and Returned)
*        The upper bounds of the input data, i.e. the maximum value for
*        each co-ordinate.  It should be initialised externally the
*        first time this routine is called, but not subsequently.
*     CMPLET = LOGICAL (Returned)
*        If true the ASCII file has been completely read.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is a server subroutine for TRANDAT.  The code would be at
*        the top level (i.e. TRANDAT) but for the requirement to
*        transfer ASCII-file data into mapped work arrays.
*     -  There is a routine for integer and floating-point data types:
*        replace "x" in the routine name by I, D, or R as appropriate.
*     -  The file is not closed on exit.
*     -  The record number is not initialised and so this routine reads
*        from the current line in the file.  Hence this routine can be
*        called repeatedly as the the output array is expanded to
*        accommodate the input data.

*  Algorithm:
*     -  Get the number of the furthest column to be read.
*     -  While the buffer is not full and there is no error, read a file
*        record into a buffer and extract the required columns, storing
*        them in the output file, and computing the bounds of the array.
*        Report the context of an error should one occur.
*     -  If the file was completely read, set a flag to indicate this.

*  Prior Requirements:
*     -  The Fortran ASCII file must already be opened.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Apr 17 (MJC):
*        Original version.
*     1990 Dec 22 (MJC):
*        Fixed bug which caused an extra loop of parsing each line buffer
*        when there where columns beyond the last column used.
*     1996 January 31 (MJC):
*        Replaced NAG calls.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'FIO_ERR'          ! Fortran-I/O-system errors
      INCLUDE 'CHR_ERR'          ! CHR error codes

*  Arguments Given:
      INTEGER
     :  FD,
     :  DIM1,
     :  DIM2,
     :  POSCOD( DIM1 + 1 )

*  Arguments Given and Returned:
      INTEGER
     :  COUNT

      REAL
     :  CODATA( DIM1, DIM2 ),
     :  LBND( DIM1 ),
     :  UBND( DIM1 )

      INTEGER
     :  VADATA( DIM2 )

*  Arguments Returned:
      LOGICAL
     :  CMPLET

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                  ! Length of the string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER NCHLIN             ! Maximum number of characters in an
                                 ! input record
      PARAMETER ( NCHLIN = 256 )

*  Local Variables:
      INTEGER
     :  HASH,                    ! Column where a hash is found in the
                                 ! input buffer
     :  I, J,                    ! Counters
     :  INDEXE,                  ! Location within the string of the end
                                 ! of the current word
     :  INDEXS,                  ! Location within the string of the
                                 ! start of the current word
     :  INDPOS( DAT__MXDIM + 1 ),! Index to the sorted positions
     :  NCHAR,                   ! Number of characters in a record
     :  NCOM,                    ! Length of the input string less
                                 ! trailing blanks
     :  NVAL,                    ! Number of values stored
     :  ORDPOS( DAT__MXDIM + 1 ),! Sorted column positions of values
                                 ! co-ordinates
     :  SHRIEK,                  ! Column where a shriek is found in the
                                 ! input buffer
     :  WSKIP                    ! Number of words (columns to skip)

      CHARACTER*( NCHLIN )
     :  BUFFER                   ! Buffer to store input and output
                                 ! strings

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Assume for the moment that the array will not be big enough to
*    store all the points.

      CMPLET = .FALSE.

*    Initialise counter.

      IF ( COUNT .LT. 1 )  COUNT  =  1

*    Sort the columns positions.
*    ===========================

*    Copy the columns.

      DO  I = 1, DIM1 + 1
         ORDPOS( I ) = POSCOD( I )
      END DO

*    Sort the columns to be able to read them in sequence and
*    obtain an index to old positions.

      CALL PDA_QSIAI( DIM1 + 1, ORDPOS, INDPOS )

*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*    Start a new error context.

      CALL ERR_MARK


*    Now run through free-format file picking up the required
*    co-ordinate and data values (up to the maximum allowed within the
*    array bounds).

      DO WHILE ( COUNT .LE. DIM2 .AND. STATUS .EQ. SAI__OK )

*       Read from the free-format file.

         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Extract the data from the character buffer.
*          ===========================================

*          Look for a comment.

            SHRIEK = INDEX( BUFFER, '!' )
            HASH = INDEX( BUFFER, '#' )

*          Get the length of the string, watching for blank lines
*          or comment lines. A hash or shriek in the first column
*          indicates a comment line.  In such cases the line can be
*          ignored.

            IF ( SHRIEK .EQ. 1 .OR. HASH .EQ. 1 .OR.
     :           BUFFER .EQ. ' ' ) THEN
               NCOM = -1
            ELSE
               NCOM = CHR_LEN( BUFFER )
            END IF

*          Initialise to the start of the string, and the number of
*          values stored.

            NVAL = 0
            INDEXE = -1

*          Find the number of words to skip at the start.

            WSKIP = ORDPOS( NVAL + 1 ) - 1

*          Loop until the end of the buffer. The number of values gets
*          updated in the loop so the test must be less than its maximum
*          value.

            DO WHILE ( INDEXE .LT. NCOM .AND. NVAL .LT. DIM1 + 1 .AND.
     :                 STATUS .EQ. SAI__OK )

*             Skip to the correct word.

               IF ( WSKIP .GT. 0 ) THEN

                  DO  I = 1, WSKIP

*                   Shift the search to the next value.

                     INDEXS = INDEXE + 1

*                   Find the start and end indices of the value.

                     CALL CHR_FIWS( BUFFER, INDEXS, STATUS )
                     INDEXE = INDEXS
                     CALL CHR_FIWE( BUFFER, INDEXE, STATUS )
                  END DO
               END IF

*             Shift the search to the next value actually required.

               INDEXS = INDEXE + 1

*             Find the start and end indices of the value.

               CALL CHR_FIWS( BUFFER, INDEXS, STATUS )
               INDEXE = INDEXS
               CALL CHR_FIWE( BUFFER, INDEXE, STATUS )

*             Watch for the case where the word terminates the line.
*             Since this is quite normal for the last word (value) in
*             the buffer, the error should be annulled.

               IF ( NVAL .EQ. DIM1 ) THEN
                  IF ( STATUS .EQ. CHR__ENDOFSENT )
     :              CALL ERR_ANNUL( STATUS )
               END IF

*             Extract the value required and convert to INTEGER type for
*             the data value, or to real for the co-ordinates.

               NVAL = NVAL + 1
               IF ( INDPOS( NVAL ) .EQ. DIM1 + 1 ) THEN
                  CALL CHR_CTOI( BUFFER( INDEXS:INDEXE ),
     :                             VADATA( COUNT ), STATUS )
               ELSE
                  J = INDPOS( NVAL )
                  CALL CHR_CTOR( BUFFER( INDEXS:INDEXE ),
     :                           CODATA( J, COUNT ), STATUS )

*                Find the bounds of the array.

                  UBND( J )  =  MAX( UBND( J ), CODATA( J, COUNT ) )
                  LBND( J )  =  MIN( LBND( J ), CODATA( J, COUNT ) )
               END IF

*             Find the number of words to skip, unless the last word
*             required has been extracted.

               IF ( NVAL .LT. DIM1 + 1 )
     :            WSKIP = ORDPOS( NVAL + 1 ) - ORDPOS( NVAL ) - 1

            END DO

*          Increment the count of pixels by one if it is not a comment
*          line in the file.

            IF ( NCOM .NE. -1 ) COUNT = COUNT + 1

*          Something may have gone wrong interpreting the file, either
*          a typographical error, or it is not arranged in columns, e.g.
*          perhaps with no co-ordinate data.

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'BUFFER', BUFFER )
               CALL ERR_REP( 'KPS1_TRDRx_FORMAT',
     :           'KPS1_TRDRx: Input file is not in the correct '/
     :           /'format.  For example, it must contain co-ordinate '/
     :           /'data.  The last buffer was "^BUFFER".', STATUS )
            END IF

         ELSE IF ( STATUS .NE. FIO__EOF ) THEN

            CALL MSG_SETC( 'BUFFER', BUFFER )

*          Report an error and abort

            CALL ERR_REP( 'KPS1_TRDRx_RDATA',
     :        'KPS1_TRDRx: Error reading data file : ^STATUS.  Line '/
     :        /'was ^BUFFER', STATUS )
            GOTO 980

*       End of no-error-reading-record-in-file check.

         END IF

      END DO

*    Last record in the file has been read successfully.  If has not
*    the CMPLET logical will still be false, and so a larger array will
*    be created.

      IF ( STATUS .EQ. FIO__EOF ) THEN

*       Record that no more points are to be read.

         CMPLET = .TRUE.

*       Correct the actual number of points.

         COUNT = COUNT - 1

         CALL ERR_ANNUL( STATUS )

      END IF

 980  CONTINUE

*    Release the error context.

      CALL ERR_RLSE

  999 CONTINUE

      END
