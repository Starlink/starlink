      SUBROUTINE POL1_WRTCL( CI, GOTRD, MAKERD, MAP, NCOL, GCOL, NROW, 
     :                       IDCOL, FD, SZBAT, LBND, UBND, WORK1, WORK2, 
     :                       WORK3, STATUS )
*+
*  Name:
*     POL1_WRTCL

*  Purpose:
*     Writes out a Tcl list holding the column data in a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_WRTCL( CI, GOTRD, MAKERD, MAP, NCOL, GCOL, NROW, IDCOL, 
*                      FD, SZBAT, LBND, UBND, WORK1, WORK2, WORK3, STATUS )

*  Description:
*     This routine writes out a Tcl list holding the column data in a 
*     catalogue.

*  Arguments:
*     CI = INTEGER (Given)
*        A CAT identifier for the catalogue.
*     GOTRD = LOGICAL (Given)
*        Will RA and DEC be stored in columns 3 and 4 of the output catalogue?
*     MAKERD = LOGICAL (Given)
*        Should RA and DEC values be created on the basis of the supplied
*        Mapping?
*     MAP = INTEGER (Given)
*        A pointer to an AST Mapping from X/Y to RA/DEC. Only accessed if
*        MAKERD is .TRUE.
*     NCOL = INTEGER (Given)
*        No. of columns to write. Should be at least 4.
*     GCOL( NCOL ) = INTEGER (Given)
*        The CAT identifiers for the columns within the input catalogue
*        to be used for each output column. Columns 1 and 2 should be the
*        X and Y columns. If GOTRD is .TRUE., columns 3 and 4 should be 
*        the RA and DEC columns.
*     NROW = INTEGER (Given)
*        No. of rows.
*     IDCOL = INTEGER (Given)
*        Index of the ID column.
*     FD = INTEGER (Given)
*        FIO identifier for text file to hold output catalogue.
*     SZBAT = INTEGER (Given)
*        First dimension of work arrays.
*     LBND( 2 ) = REAL (Retuned)
*        The lower bounds of columns 1 (X) and 2 (Y).
*     UBND( 2 ) = REAL (Retuned)
*        The upper bounds of columns 1 (X) and 2 (Y).
*     WORK1( SZBAT, 2 ) = DOUBLE PRECISION (Returned)
*        Work array.
*     WORK2( SZBAT, 2 ) = DOUBLE PRECISION (Returned)
*        Work array.
*     WORK3( SZBAT, * ) = REAL (Returned)
*        Work array. The second dimension should be equal to NCOL - 4 if
*        GOTRD is .TRUE., or NCOL - 2 otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-SEP-2000 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CAT_PAR'          ! CAT__ constants

*  Arguments Given:
      INTEGER CI
      LOGICAL GOTRD
      LOGICAL MAKERD
      INTEGER MAP
      INTEGER NCOL
      INTEGER GCOL( NCOL )
      INTEGER NROW
      INTEGER IDCOL
      INTEGER FD
      INTEGER SZBAT
      
*  Arguments Returned:
      REAL LBND( 2 )
      REAL UBND( 2 )
      DOUBLE PRECISION WORK1( SZBAT, 2 ) 
      DOUBLE PRECISION WORK2( SZBAT, 2 ) 
      REAL WORK3( SZBAT, * ) 

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DSEP  ! Distance in radians between 2 points
      DOUBLE PRECISION SLA_DRANRM! Normalizes an angle to 0 to 2.PI
      DOUBLE PRECISION SLA_DRANGE! Normalizes an angle to -PI to +PI

*  Local Constants:
      DOUBLE PRECISION NRAN      ! No. of positions to use for guessing
      PARAMETER ( NRAN = 20 )    ! central RA and DEC, and radius.

      DOUBLE PRECISION RTOD      ! Radians to degrees conversion factor
      PARAMETER ( RTOD = 57.2957795130823208767981548141051703324054 )

*  Local Variables:
      CHARACTER SIGN*1           ! Sign for formatted value
      CHARACTER TEXT*80          ! Formatted value
      DOUBLE PRECISION DEC       ! Dec
      DOUBLE PRECISION DECC      ! Estimate of central Dec
      DOUBLE PRECISION DECG      ! A sample good DEC
      DOUBLE PRECISION DECRAN( NRAN )! DEC values at saved rows
      DOUBLE PRECISION RA        ! RA
      DOUBLE PRECISION RAC       ! Estimate of central RA
      DOUBLE PRECISION RAG       ! A sample good RA
      DOUBLE PRECISION RANDEC( NRAN ) ! DEC value for random rows
      DOUBLE PRECISION RANRA( NRAN ) ! RA value for random rows
      DOUBLE PRECISION RARAN( NRAN ) ! RA values at saved rows
      DOUBLE PRECISION RR        ! Distance in radians
      DOUBLE PRECISION SS        ! Sum of scale values (radians per pixel)
      INTEGER BATSZ              ! Size of this batch
      INTEGER FIELDS(4)          ! Integer fields for formatted value
      INTEGER I                  ! Loop index
      INTEGER IAT                ! Length of formatted value
      INTEGER IC                 ! Index of saved row nearest to field centre
      INTEGER IRAN               ! Index of next random row number
      INTEGER IROW               ! Index of first row in bacth
      INTEGER JROW               ! Index of next row to be read
      INTEGER J                  ! Loop index
      INTEGER K                  ! Loop index
      INTEGER KK                 ! Loop index
      INTEGER K0                 ! Loop index
      INTEGER MRAN               ! Number of unique random rows
      INTEGER RANROW( NRAN )     ! Random row numbers
      INTEGER SAVRAN             ! No. of random rows saved so far
      INTEGER UNIT               ! Fortran IO unit for output text file
      LOGICAL MAKEID             ! Create ID values?
      LOGICAL SAVE               ! Is this row to be saved?
      REAL RAN( NRAN )           ! Random values between 0 and 1
      REAL RANX( NRAN )          ! X value for random rows
      REAL RANY( NRAN )          ! Y value for random rows
      REAL RP                    ! Distance in pixels
      REAL RP2                   ! Squared distance in pixels**2
      REAL RP2MIN                ! Min value of RP2
      REAL X                     ! X column value
      REAL XC                    ! Estimate of central X 
      REAL XRAN( NRAN )          ! X values at saved rows
      REAL Y                     ! Y column value
      REAL YC                    ! Estimate of central Y
      REAL YRAN( NRAN )          ! Y values at saved rows

*  Store NRAN nearly random values between 0 and 1, in increasing order.
      DATA RAN/ 0.112368822, 0.149562612, 0.169489801, 0.202944234,
     :		0.207126677, 0.235560641, 0.235643834, 0.337286144,
     :		0.35384509, 0.451915026, 0.497318864, 0.534611225,
     :		0.549462259, 0.557423711, 0.613845289, 0.62577945,
     :		0.842357278, 0.897625804, 0.938356161, 0.981330872 /

*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we need to construct ID values.
      MAKERD = ( GCOL( IDCOL ) .EQ. CAT__NOID )

*  Store some random row numbers. The X/Y and RA/DEC values at these rows
*  are saved in local arrays as the rows are accessed. The row which is 
*  closest to the centre of the final bounding box is used to estimate a 
*  central RA and DEC. This will not of course necessarily be the actual 
*  field centre but it will usually be somewhere near, which is good enough.
*  Ensure that no dupliate row numbers are included in the RANROW array.
      RANROW( 1 ) = MIN( NROW, MAX( 1, NINT( RAN( 1 ) * NROW ) ) )
      MRAN = 1
      DO I = 2, NRAN
         IROW = MIN( NROW, MAX( 1, NINT( RAN( I ) * NROW ) ) )
         IF( IROW .GT. RANROW( MRAN ) ) THEN
            MRAN = MRAN + 1
            RANROW( MRAN ) = IROW
         END IF
      END DO
   
*  So far we have saved no random rows.
      SAVRAN = 0

*  We use Fortran WRITE statement to write the data values to the output
*  text file, rather than the FIO_WRITE call, since it is a lot faster.
*  Get the Fortran IO unit number associated with the output file.
      CALL FIO_UNIT( FD, UNIT, STATUS )

*  Write out the initial part of the Tcl assignment statement.
      WRITE( UNIT, * ) 'set data_ { \\'

*  Initialise the PIXEL Frame bound box for the catalogue.
      LBND( 1 ) = VAL__MAXR
      LBND( 2 ) = VAL__MAXR
      UBND( 1 ) = VAL__MINR
      UBND( 2 ) = VAL__MINR

*  Initialize some safe RA/DEC values.
      RAG = 0.0
      DECG = 0.0

*  Initialise the row number.
      IROW = 1

*  Do each batch.
      DO WHILE( IROW .LE. NROW .AND. STATUS .EQ. SAI__OK )

*  Find the size of this batch
         BATSZ = MIN( ( NROW - IROW ) + 1, SZBAT )

*  Read in the next batch of X and Y values.
         CALL POL1_GCOLD( CI, GCOL( 1 ), IROW, BATSZ, WORK1( 1, 1 ),
     :                    STATUS )     
         CALL POL1_GCOLD( CI, GCOL( 2 ), IROW, BATSZ, WORK1( 1, 2 ),
     :                    STATUS )     

*  If the output file is to contain RA/DEC values in columns 3 and 4...
         IF( GOTRD ) THEN 

*  If required, create RA/DEC values by transforming the X/Y values.
            IF( MAKERD ) THEN 
               CALL AST_TRANN( MAP, BATSZ, 2, SZBAT, WORK1, .TRUE., 2, 
     :                         SZBAT, WORK2, STATUS ) 

*  Otherwise, read the RA and DEC values in from the input catalogue.
            ELSE
               CALL POL1_GCOLD( CI, GCOL( 3 ), IROW, BATSZ, 
     :                          WORK2( 1, 1 ), STATUS )     
               CALL POL1_GCOLD( CI, GCOL( 4 ), IROW, BATSZ, 
     :                          WORK2( 1, 2 ), STATUS )     
            END IF

*  Read the remaining columns.
            DO I = 5, NCOL
               IF( MAKEID .AND. I .EQ. IDCOL ) THEN 
                  DO KK = 1, BATSZ
                     WORK3( KK, I - 4 ) = KK + IROW - 1
                  END DO
               ELSE
                  CALL POL1_GCOLR( CI, GCOL( I ), IROW, BATSZ, 
     :                             WORK3( 1, I - 4 ), STATUS )     
               END IF
            END DO

*  Look out for the first random row.
            IRAN = 1

*  Now write out the data to the text file as a series of rows, and
*  update the bounding box.
            DO I = 1, BATSZ
               X = REAL( WORK1( I, 1 ) )
               Y = REAL( WORK1( I, 2 ) )
               JROW = IROW + I - 1

*  Set a flag if this is one of the selected random rows. If so, get
*  ready to find the next random row.
               SAVE = ( JROW .EQ. RANROW( IRAN ) )
               IF( SAVE ) IRAN = MIN( MRAN, IRAN + 1 )

*  If the RA and DEC are good...
               RA = WORK2( I, 1 )
               DEC = WORK2( I, 2 )
               IF( RA .NE. VAL__BADD .AND. DEC .NE. VAL__BADD ) THEN

*  Normalize the RA and DEC values.
                  RA = SLA_DRANRM( RA )
                  DEC = SLA_DRANGE( DEC )

*  If this is one of the selected random rows, store the X,Y,RA and DEC.
                  IF( SAVE ) THEN
                     SAVRAN = SAVRAN + 1
                     XRAN( SAVRAN ) = X
                     YRAN( SAVRAN ) = Y
                     RARAN( SAVRAN ) = RA
                     DECRAN( SAVRAN ) = DEC
                  END IF

*  Save these good values.
                  RAG = RA
                  DECG = DEC

*  Convert from radians to degrees.
                  RA = RA*RTOD
                  DEC = DEC*RTOD

               END IF
               
*  Write out the values, appending a backslash at the end of each line to
*  tell tcl to ignore the line break.
               WRITE( UNIT, * ) '{', X, Y, ' \\'
               WRITE( UNIT, * ) RA, DEC, ' \\'
               DO J = 1, NCOL - 4, 4
                  K0 = MIN( 3, NCOL - 4 - J )
                  WRITE( UNIT, * ) ( WORK3( I, J + K ), K = 0, K0),' \\'
               END DO
               WRITE( UNIT, * ) '} \\'

*  Update the pixel bounding box.
               IF( X .LT. LBND( 1 ) ) LBND( 1 ) = X
               IF( Y .LT. LBND( 2 ) ) LBND( 2 ) = Y
               IF( X .GT. UBND( 1 ) ) UBND( 1 ) = X
               IF( Y .GT. UBND( 2 ) ) UBND( 2 ) = Y

            END DO

*  Now deal with cases where no RA/DEC columns are to be included in the
*  output catalogue.
         ELSE

*  Read the remaining columns.
            DO I = 3, NCOL
               IF( MAKEID .AND. I .EQ. IDCOL ) THEN 
                  DO KK = 1, BATSZ
                     WORK3( KK, I - 2 ) = KK + IROW - 1
                  END DO
               ELSE
                  CALL POL1_GCOLR( CI, GCOL( I ), IROW, BATSZ, 
     :                             WORK3( 1, I - 2 ), STATUS )     
               END IF
            END DO

*  Now write out the data to the text file as a series of rows, and
*  update the bounding box.
            DO I = 1, BATSZ
               X = REAL( WORK1( I, 1 ) )
               Y = REAL( WORK1( I, 2 ) )
               JROW = IROW + I - 1

*  Write out the values, appending a backslash at the end of each line to
*  tell tcl to ignore the line break.
               WRITE( UNIT, * ) '{', X, Y, ' \\'
               DO J = 1, NCOL - 2, 4
                  K0 = MIN( 3, NCOL - 2 - J )
                  WRITE( UNIT, * ) ( WORK3( I, J + K ), K = 0, K0),' \\'
               END DO
               WRITE( UNIT, * ) '} \\'

               IF( X .LT. LBND( 1 ) ) LBND( 1 ) = X
               IF( Y .LT. LBND( 2 ) ) LBND( 2 ) = Y
               IF( X .GT. UBND( 1 ) ) UBND( 1 ) = X
               IF( Y .GT. UBND( 2 ) ) UBND( 2 ) = Y

            END DO

         END IF

*  Increment the index of the next row to be read.
         IROW = IROW + BATSZ

      END DO        

*  Write out the final part of the Tcl assignment statement.
      WRITE( UNIT, * ) '}'

*  Store the pixel co-ords at the centre of the field.
      XC = 0.5*( LBND( 1 ) + UBND( 1 ) )
      YC = 0.5*( LBND( 2 ) + UBND( 2 ) )

*  Write out the central RA/DEC values and radius.
      IF( GOTRD ) THEN
         IF( SAVRAN .GT. 1 ) THEN

*  Initialise the minimum distance found so far.
            RP2MIN = VAL__MAXR       

*  Loop round the saved rows.
            DO I = 1, SAVRAN

*  Find the squared distance from this row to the field centre in pixels.
               RP2 = ( XRAN( I ) - XC )**2 + ( YRAN( I ) - YC )**2            

*  If this is less than the current minimum, note the row index and
*  update the minimum squared distance.
               IF( RP2 .LT. RP2MIN ) THEN
                  IC = I
                  RP2MIN = RP2
               END IF

            END DO

*  Save the values at the row which is closest to the field centre.
            XC = XRAN( IC )
            YC = YRAN( IC )
            RAC = RARAN( IC )
            DECC = DECRAN( IC )

*  We have found our estimate of the field centre in RA/DEC (the values
*  for row IC). We now need an estimate of the pixel scale.  Loop round
*  each saved row again, skipping over the one found above.
            DO I = 1, SAVRAN
               IF( I .NE. IC ) THEN

*  Find the distance in pixels from this row to the central row.
                  RP = SQRT( ( XRAN( I ) - XC )**2 + 
     :                       ( YRAN( I ) - YC )**2 )

*  Find the distance between the two points in radians.
                  RR = SLA_DSEP( RARAN( I ), DECRAN( I ), RAC, DECC )

*  Get the pixel scale in radians per pixels, and increment the sum of
*  all scale values.
                  SS = SS + ( RR/ RP )

               END IF
            END DO

*  Write out the central X and Y values, relative to the bottom left corner 
*  of the bounding box.
            WRITE( UNIT, * ) 'set xrefpix_ ', NINT( XC - LBND( 1 ) + 1 )
            WRITE( UNIT, * ) 'set yrefpix_ ', NINT( YC - LBND( 2 ) + 1 )

*  Write out the pixel size in arcseconds
            WRITE( UNIT, * ) 'set secpix_ ', 
     :                        SS/( SAVRAN - 1 )*RTOD*3600.0

*  If less than two rows were saved with good RA and DEC, use a guess
*  based on any good RA DEC value found.
         ELSE 
            RAC = RAG
            DECC = DECG
            WRITE( UNIT, * ) 'set xrefpix_ 1.0'
            WRITE( UNIT, * ) 'set yrefpix_ 1.0'
            WRITE( UNIT, * ) 'set secpix_ 1.0'
         END IF

*  Write out the central RA in "h:m:s" format
         CALL SLA_DR2TF( 3, SLA_DRANRM( RAC ), SIGN, FIELDS ) 
         IF( FIELDS( 1 ) .EQ. 24 ) THEN
            TEXT = 'set ra_ "0:0:0"'
            IAT = 15
         ELSE
            TEXT = 'set ra_ "'
            IAT = 9
            CALL CHR_PUTI( FIELDS( 1 ), TEXT, IAT )
            CALL CHR_PUTC( ':', TEXT, IAT )
            CALL CHR_PUTI( FIELDS( 2 ), TEXT, IAT )
            CALL CHR_PUTC( ':', TEXT, IAT )
            CALL CHR_PUTI( FIELDS( 3 ), TEXT, IAT )
            CALL CHR_PUTC( '.', TEXT, IAT )
            CALL CHR_PUTI( FIELDS( 4 ), TEXT, IAT )
            CALL CHR_PUTC( '"', TEXT, IAT )
         END IF
         
         WRITE( UNIT, * ) TEXT( : IAT )

*  Write out the central DEC in "d:m:s" format
         CALL SLA_DR2AF( 3, SLA_DRANGE( DECC ), SIGN, FIELDS ) 
         TEXT = 'set dec_ "'
         IAT = 10
         CALL CHR_PUTC( SIGN, TEXT, IAT )
         CALL CHR_PUTI( FIELDS( 1 ), TEXT, IAT )
         CALL CHR_PUTC( ':', TEXT, IAT )
         CALL CHR_PUTI( FIELDS( 2 ), TEXT, IAT )
         CALL CHR_PUTC( ':', TEXT, IAT )
         CALL CHR_PUTI( FIELDS( 3 ), TEXT, IAT )
         CALL CHR_PUTC( '.', TEXT, IAT )
         CALL CHR_PUTI( FIELDS( 4 ), TEXT, IAT )
         CALL CHR_PUTC( '"', TEXT, IAT )
         WRITE( UNIT, * ) TEXT( : IAT )

      END IF

*  Write out the dimensions of the bounding box.
      WRITE( UNIT, * ) 'set nxpix_ ', NINT( UBND( 1 ) - LBND( 1 ) + 1 )
      WRITE( UNIT, * ) 'set nypix_ ', NINT( UBND( 2 ) - LBND( 2 ) + 1 )

      END 
