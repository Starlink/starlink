      SUBROUTINE CCD1_WRIDI( FD, ID, RANK, NOUT, DATA, NREC, NVAL,
     :                       BUFFER, BLEN, STATUS, x, y )
*+
*  Name:
*     CCD1_WRIDI

*  Purpose:
*     Write identifiers and data values from an array to a list file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_WRIDI( FD, ID, RANK, NOUT, DATA, NREC, NVAL, BUFFER,
*                      BLEN, STATUS )

*  Description:
*     This routine writes out the ID values from an array in order, and
*     data values from a two-dimensional array in the order determined
*     by an accompanying indirection array.  Hence in line I of the
*     output file will be written the index integer ID( I ), followed
*     by a list of all the elements in column RANK( I ) of the DATA
*     array.
*
*     If the output buffer is potentially too short to hold all the
*     values in each column of the array, then a message to this 
*     effect will be written, and every line of output will contain
*     the same, reduced, number of items.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO file descriptor.
*     ID( NOUT ) = INTEGER (Given)
*        The identifiers for the lines.
*     RANK( NOUT ) = INTEGER (Given)
*        An array saying which column of DATA should be output on
*        which line of the file.
*     NOUT = INTEGER (Given)
*        The number of lines to write to the output file.
*     DATA( NREC, NVAL ) = DOUBLE PRECISION (Given)
*        The array containing the data values to be written to the file.
*     NREC = INTEGER (Given)
*        The first dimension of DATA.
*     NVAL = INTEGER (Given)
*        The second dimension of DATA; also the number of values to be
*        written on each line after the identifier.
*     BUFFER = CHARACTER * ( BLEN ) (Given and Returned)
*        Buffer to hold an output line.
*     BLEN = INTEGER (Given)
*        Length of BUFFER.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      
*  Arguments Given:
      INTEGER FD
      INTEGER NREC
      INTEGER NVAL
      INTEGER NOUT
      INTEGER ID( NOUT )
      INTEGER RANK( NOUT )
      INTEGER BLEN
      DOUBLE PRECISION DATA( NREC, NVAL )
      
*  Arguments Given and Returned:
      CHARACTER * ( * ) BUFFER
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of character string

*  Local Variables:
      INTEGER GAP                ! Gap between columns
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in buffer
      INTEGER IEND               ! Last filled position in string
      INTEGER J                  ! Loop variable
      INTEGER NCHAR              ! Number of characters used to encode value
      INTEGER NV                 ! Number of values which can be safely output
      CHARACTER * ( VAL__SZD ) WORD ! Buffer to contain values as a string

      double precision x(*), y(*)

*  Local Data:
      DATA GAP / 2 /
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Work out how many columns we can be sure of outputting.
      NV = MIN( NVAL, ( BLEN - VAL__SZI + GAP ) / ( VAL__SZD + GAP ) )

*  If that is fewer than requested, warn that this is the case.
      IF ( NV .LT. NVAL ) THEN
         CALL MSG_SETI( 'NV', NV )
         CALL MSG_SETI( 'NVAL', NVAL )
         CALL CCD1_MSG( ' ', '     Warning: can only output ^NV ' //
     :                  'data columns out of ^NVAL.', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
      END IF

*  Loop for all output lines.
      DO I = 1, NOUT

*  Clear the output buffer.
         BUFFER = ' '

*  Set the identifier value.
         CALL CHR_ITOC( ID( I ), WORD, NCHAR )

*  Insert it into the output buffer.
         BUFFER( 1 : NCHAR ) = WORD( 1 : NCHAR )

*  Increment position within output buffer.
         IAT = MAX( NCHAR, 8 ) + 2

*  Insert the data columns.
         DO J = 1, NV
            CALL CHR_DTOC( DATA( RANK( I ), J ), WORD, NCHAR )
            IEND = IAT + NCHAR
            BUFFER( IAT : IEND ) = WORD( 1 : NCHAR )
            IAT = IAT + MAX( NCHAR, 8 ) + GAP
         END DO
         call chr_dtoc( x(i), word, nchar )
         iend = iat + nchar
         buffer( iat: iend ) = word( 1:nchar )
         iat = iat + max( nchar, 8 ) + gap
         call chr_dtoc( y(i), word, nchar )
         iend = iat + nchar
         buffer( iat: iend ) = word( 1:nchar )
         iat = iat + max( nchar, 8 ) + gap

*  Now write the buffer to file.
         CALL FIO_WRITE( FD, BUFFER( 1 : CHR_LEN( BUFFER ) ), STATUS )
      END DO

      END
* $Id$
