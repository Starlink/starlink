      SUBROUTINE POLCENT( STATUS )
*+
*  Name:
*     POLCENT

*  Purpose:
*     Find the centroid of a set of positions in an image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLCENT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine is equivalent to CCDPACK:FINDCENT, but is simpler and
*     therefore faster. It is tailored to the needs of the Polka
*     application, and is not intended for public use. The algorithm
*     used by CCG1_CENR has been modified to improve the background
*     subtraction prior to forming the marginal profiles.

*  ADAM Parameters:
*     ISIZE = _INTEGER (Read)
*        The size of a box side (in pixels) centered on current
*        position which will be used to form the marginal profiles used
*        to estimate the centroid.
*        [9]
*     MAXITER = _INTEGER (Read)
*        The maximum number of iterations which may be used in
*        estimating the centroid. Only used if the tolerance criterion
*        is not met in this number of iterations.
*        [3]
*     MAXSHIFT = _DOUBLE (Read)
*        The maximum shift (in pixels) allowed from an initial position.
*        [5.5]
*     NDF = NDF (Read)
*        The NDF in which to search for the image feature.
*     POSITIVE = _LOGICAL (Read)
*        If TRUE then the image features have increasing values
*        otherwise they are negative.
*        [TRUE]
*     TOLER = _DOUBLE (Read)
*        The required tolerance in the positional accuracy of the
*        centroid. On each iteration the box of data from which the
*        centroid is estimated is updated. If the new centroid does not
*        differ from the previous value by more than this amount (in X
*        and Y) then iteration stops. Failure to meet this level of
*        accuracy does not result in the centroid being rejected, the
*        centroiding process just stops after the permitted number of
*        iterations (MAXITER).
*        [0.05]
*     INFILE = LITERAL (Read)
*        The name of a text file containing the the X and Y values to
*        be used as the initial guess at accurate positions. Each line
*        should hold the X value followed by the Y value separated by
*        spaces.
*     OUTFILE = LITERAL (Read)
*        The name of a text file to create containing the accurate
*        positions. Each line will hold the accurate X and Y values for
*        the corresponding input position, separated by spaces. Positions
*        which cannot be found are set to -100000 (both X and Y).
*     XYOUT = LITERAL (Write)
*        A string holding the last accurate X and Y values, separated by
*        a space.

*  Implementation Status:
*     - Bad pixels can be handled. The NDF is accessed as an array of
*     single precision values.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1997 (DSB):
*        Original version, derived from CCDPACK:FINDCENT. The CCDPACK
*        version was slow, and it crashed sometimes because it did not
*        release its IRH groups before returning.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO_ error constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MXPNT
      PARAMETER ( MXPNT = 300 )  ! Max. no. of points to be processed

*  Local Variables:
      CHARACTER TEXT*80          ! Buffer for results string
      DOUBLE PRECISION MAXSHF    ! Maximum shift in position
      DOUBLE PRECISION TOLER     ! Tolerance required in centroid
      DOUBLE PRECISION XACC      ! Accurate position
      DOUBLE PRECISION XPOS      ! Original guess at input position
      DOUBLE PRECISION YACC      ! Accurate position
      DOUBLE PRECISION YPOS      ! Original guess at input position
      INTEGER EL                 ! Number of elements in input NDF
      INTEGER INDF               ! Input NDF identifier
      INTEGER IPIN               ! Pointer to input data array
      INTEGER IPNT               ! Index of position being processed
      INTEGER ISIZE              ! Size of box to search
      INTEGER LBND( 2 )          ! Lower bounds of NDF data component
      INTEGER MAXIT              ! Maximum number of iterations
      INTEGER NCOL               ! First dimension of input NDF
      INTEGER NDIM               ! Dimensionality of input NDF
      INTEGER NLINE              ! Second dimension of input NDF
      INTEGER NPNT               ! No. of points to centroid
      INTEGER UBND( 2 )          ! Upper bounds of NDF data component
      LOGICAL SIGN               ! If true features are positive
      REAL XIN( MXPNT )          ! Original guess at input position
      REAL XOUT( MXPNT )         ! Accurate position
      REAL YIN( MXPNT )          ! Original guess at input position
      REAL YOUT( MXPNT )         ! Accurate position
      INTEGER FD, FDO, NBUFF
      CHARACTER BUFFER*80
      LOGICAL TOPEN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NDF.
      CALL NDF_ASSOC ( 'NDF', 'READ', INDF, STATUS )

*  Get the size of the data array.
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
      NCOL = UBND( 1 ) - LBND( 1 ) + 1
      NLINE = UBND( 2 ) - LBND( 2 ) + 1

*  Map in the data component of the NDF.
      CALL NDF_MAP( INDF, 'Data', '_REAL', 'READ', IPIN, EL, STATUS )

*  Get the other parameter values...

*  The initial position guesses.
      CALL CCD1_ASFIO( 'INFILE', 'READ', 'LIST', 0, FD, TOPEN, STATUS )

      NPNT = 0
      DO WHILE( NPNT .LT. MXPNT .AND. STATUS .EQ. SAI__OK )
         CALL FIO_READ( FD, BUFFER , NBUFF, STATUS )
         IF( STATUS .EQ. SAI__OK .AND. BUFFER .NE. ' ' ) THEN
            NPNT = NPNT + 1
            READ( BUFFER, * ) XIN( NPNT ), YIN( NPNT )
         END IF
      END DO

      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  Close the input file.
      CALL FIO_CLOSE( FD, STATUS )

*  Size of the search box. Larger than 3 and odd.
      CALL PAR_GET0I( 'ISIZE', ISIZE, STATUS )
      ISIZE = MAX( 3, ISIZE )
      ISIZE = ( ISIZE / 2 ) * 2 + 1

*  The maximum shift in the centroid position.
      CALL PAR_GET0D( 'MAXSHIFT', MAXSHF, STATUS )
      MAXSHF = MAX( 0.0001D0, MAXSHF )

*  The maximum number of iterations to achieve tolerance.
      CALL PAR_GET0I( 'MAXITER', MAXIT, STATUS )
      MAXIT = MAX( 1, MAXIT )

*  The tolerance in the positions estimate that is required.
      CALL PAR_GET0D( 'TOLER', TOLER, STATUS )
      TOLER = ABS( TOLER )

*  The sign of the features to cenroid +ve or -ve.
      CALL PAR_GET0L( 'POSITIVE', SIGN, STATUS )

*  The output file.
      CALL CCD1_ASFIO( 'OUTFILE', 'WRITE', 'LIST', 0, FDO, TOPEN,
     :                 STATUS )

*  Check the status so that we can be sure that any error detected
*  within the following loop was generated by CCG1_CENR.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Do each position.
      DO IPNT = 1, NPNT

*  Transform the initial position guess to pixel indices.
         XPOS = DBLE( XIN( IPNT ) - REAL( LBND( 1 ) ) + 1.5 )
         YPOS = DBLE( YIN( IPNT ) - REAL( LBND( 2 ) ) + 1.5 )

*  Centroid the position.
         CALL CCG1_CENR( XPOS, YPOS, %VAL( CNF_PVAL( IPIN ) ),
     :                   NCOL, NLINE,
     :                   ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                   XACC, YACC, STATUS )

*  If the accurate position could not be found, annul the error
*  and store a magic value of -100000 for the X and Y values.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            XOUT( IPNT ) = -100000.0
            YOUT( IPNT ) = -100000.0

*  Otherwise, transform this position back to input coordinates.
         ELSE
            XOUT( IPNT ) = REAL( XACC + DBLE( LBND( 1 ) ) - 1.5D0 )
            YOUT( IPNT ) = REAL( YACC + DBLE( LBND( 2 ) ) - 1.5D0 )
         END IF

*  Write out the results values to the screen and output file.
         CALL MSG_SETR( 'XIN',  XIN( IPNT ) )
         CALL MSG_SETR( 'YIN',  XIN( IPNT ) )
         CALL MSG_SETR( 'XOUT',  XOUT( IPNT ) )
         CALL MSG_SETR( 'YOUT',  XOUT( IPNT ) )
         CALL MSG_OUT( ' ', '(^XIN,^YIN) -> (^XOUT,^YOUT)', STATUS )

         WRITE( TEXT, * ) XOUT( IPNT ), YOUT( IPNT )
         CALL FIO_WRITE( FDO, TEXT, STATUS )

      END DO

*  Close the outut file.
      CALL FIO_CLOSE( FDO, STATUS )

*  Write the last result to the output parameter.
      CALL PAR_PUT0C( 'XYOUT', TEXT( : CHR_LEN( TEXT ) ), STATUS )

*  Annul the NDF identifier.
 999  CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested,
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'POLCENT_ERR', 'POLCENT: Unable to locate '//
     :                    'object centroid.', STATUS )
         END IF

      END IF

      END
