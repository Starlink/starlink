      SUBROUTINE RTD_CENT( TYPE, IPIN, NX, NY, XIN, YIN, NIN, ISIZE, 
     :                     MAXSHF, MAXIT, TOLER, XOUT, YOUT, NOUT, 
     :                     STATUS )
*+
*  Name:
*     RTD_CENT

*  Purpose:
*     Finds the centroid of features in a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD_CENT( TYPE, IPIN, NX, NY, XIN, YIN, NIN, ISIZE, 
*                    MAXSHF, MAXIT, TOLER, IDOUT, XOUT, YOUT, NOUT, 
*                    STATUS )

*  Description:
*     This routine locates the centroids of objects whose positions are
*     specified in the XIN and YIN arrays. The input data is pointed to
*     by IPIN and has size NX by NY. The input data may be of any
*     non-complex numeric data type.  If no centroid is located at a
*     position then the input value for that position is returned.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the data pointed to by IPIN. This must be one of
*        the non-complex HDS numeric types.
*     IPIN = INTEGER (Given)
*        Pointer to an array of data of type TYPE which contains the
*        image features to be centroided.
*     NX = INTEGER (Given)
*        First dimension of the data pointed to by IPIN.
*     NY = INTEGER (Given)
*        Second dimension of the data pointed to by IPIN.
*     IDIN( NIN ) = INTEGER (Given)
*        Identifiers of the XIN and YIN data. The correspondence of
*        these values to XIN and YIN are maintained when the XIN and YIN
*        positions are centroided.
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        X positions of features whose centroid is to be determined.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        Y positions of features whose centroid is to be determined.
*     ISIZE = INTEGER (Given)
*        The size of the search square side.
*     MAXSHF = DOUBLE PRECISION (Given)
*        Maximum allowed shift in image centre.
*     MAXIT = INTEGER (Given)
*        Maximum number of refining iterations.
*     TOLER = DOUBLE PRECISION (Given)
*        Accuracy with which the centroid should be located.
*     XOUT( NIN ) = DOUBLE PRECISION (Returned)
*        X positions of features whose centroid has been determined, otherwise
*        the same value as input is returned.
*     YOUT( NIN ) = DOUBLE PRECISION (Returned)
*        Y positions of features whose centroid has been determined, otherwise
*        the same value as input is returned.
*     NOUT = INTEGER (Returned)
*        The number of positions successfully centroided.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (PDRAPER):
*        Original version.
*     12-DEC-1997 (PDRAPER):
*        Converted for use in GAIA (was CCDPACK CCD1_CENT).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER IPIN
      INTEGER NX
      INTEGER NY
      INTEGER NIN
      DOUBLE PRECISION XIN( NIN )
      DOUBLE PRECISION YIN( NIN )
      INTEGER ISIZE
      DOUBLE PRECISION MAXSHF
      INTEGER MAXIT
      DOUBLE PRECISION TOLER
      
*  Arguments Returned:
      DOUBLE PRECISION XOUT( NIN )
      DOUBLE PRECISION YOUT( NIN )
      INTEGER NOUT

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      LOGICAL SIGN              ! Sign of features (positive!)
      DOUBLE PRECISION XACC     ! Centroid position
      DOUBLE PRECISION YACC     ! Centroid position 
      INTEGER I                 ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up an error context to trap all error messages. These are
*  informational messages from the centroiding routine.
      CALL ERR_MARK

*  Only look for postive features.
      SIGN = .TRUE.

*  Try to determine the centroid for all positions.
      NOUT = 0 
      DO 1 I = 1, NIN 

*  Centroid this position.
         IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL RTD1_CENUB( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_BYTE' ) THEN 
            CALL RTD1_CENB( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN 
            CALL RTD1_CENUW( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN 
            CALL RTD1_CENW( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN 
            CALL RTD1_CENI( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN 
            CALL RTD1_CENR( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL RTD1_CEND( XIN( I ), YIN( I ), %VAL( IPIN ), NX, NY,
     :                       ISIZE, SIGN, MAXSHF, MAXIT, TOLER,
     :                       XACC, YACC, STATUS )
         ELSE

*  Unknown data type.
            STATUS  = SAI__ERROR
            CALL ERR_REP( 'RTD_CENT_UNKN',
     :                    '  RTD_CENT: Unknown data type'  , STATUS )
            GO TO 99
         END IF

*  Check status. No values for this position - just annul the error and
*  continue, replacing the output values with those of the input ones.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            XOUT( I ) = XIN( I )
            YOUT( I ) = YIN( I )
         ELSE

*  Increment successful output count.
            NOUT = NOUT + 1

*  Set the output position.
            XOUT( I ) = XACC
            YOUT( I ) = YACC
         END IF
 1    CONTINUE
      CALL ERR_RLSE

*  Check that some centroids have been found.
      IF ( NOUT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'RTD_CENT_NONE',
     : '  No object centroids have been located', STATUS )
      END IF

 99   CONTINUE
      END
