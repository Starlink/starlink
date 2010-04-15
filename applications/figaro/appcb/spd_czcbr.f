      SUBROUTINE SPD_CZCBR( XISND, VARUSE, MNDF, COMP,
     :   XNELM, NROWS, ROWLEN, BIAS, XDATA, YDATA, VAR, STATUS )
*+
*  Name:
*     SPD_CZCB{DR}

*  Purpose:
*     Spectral moments for each row of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZCBR( XISND, VARUSE, MNDF, COMP, XNELM, NROWS, ROWLEN,
*        BIAS, XDATA, YDATA, VAR, STATUS )

*  Description:
*     This routine loops for the rows of an array and determines for
*     each row the spectral moments. The moments are stored in the
*     result structure of a given main NDF. The assumption is that the
*     given arrays are from that main NDF. The result structure must
*     have been accessed and the given component number must be valid
*     for the purpose.

*  Arguments:
*     XISND = LOGICAL (Given)
*        True if XDATA is an array of the same shape as YDATA. False if
*        XDATA is one-dimensional and to be re-used for each row in
*        YDATA.
*     VARUSE = LOGICAL (Given)
*        True if VAR array is given. VAR is ignored if VARUSE is false.
*     MNDF = INTEGER (Given)
*        The identifier of the main NDF. This is used to look up the
*        result structure where to store the moment values. The result
*        structure must have been accessed with SPD_FAAA.
*     COMP = INTEGER (Given)
*        The number of the component in the result structure that is to
*        be used to store the moments.
*     XNELM = INTEGER (Given)
*        The size of the XDATA array.
*     NROWS = INTEGER (Given)
*        The number of rows in YDATA.
*     ROWLEN = INTEGER (Given)
*        The length of the rows in YDATA.
*     BIAS = REAL (Given)
*        y(x) - bias is the probability function. The bias is useful
*        when the continuum or baseline level is not zero (but
*        constant).
*     XDATA( XNELM ) = REAL (Given)
*        The array of x values. This can be a vector of length ROWLEN or
*        an array of size ROWLEN*NROWS.
*     YDATA( ROWLEN, NROWS ) = REAL (Given)
*        The array of y values.
*     VAR( ROWLEN, NROWS ) = REAL (Given)
*        The array of the variances of the y values. This is ignored if
*        VARUSE is false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Mar 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      LOGICAL XISND
      LOGICAL VARUSE
      INTEGER MNDF
      INTEGER COMP
      INTEGER XNELM
      INTEGER NROWS
      INTEGER ROWLEN
      REAL BIAS
      REAL XDATA( XNELM )
      REAL YDATA( ROWLEN, NROWS )
      REAL VAR(   ROWLEN, NROWS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NPARA              ! Number of moments worked out
      PARAMETER ( NPARA = 15 )

*  Local Variables:
      INTEGER I                  ! Row number
      INTEGER INDEX              ! Start of row in XDATA
      REAL MDATA( NPARA )      ! The moments for a row
      REAL MVAR(  NPARA )      ! Their variances

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  INDEX is used to point into the arrays at the start of the row in
*  question. There are separate loops depending on whether XDATA is 1-D
*  or N-D and whether variance is available or not.
*  For each row there are two calls, one to work out the moments FDATA
*  from the spectrum YDATA(XDATA), and another to store FDATA into the
*  COMP-th component of the result structure of the main MNDF.
      IF ( XISND .AND. VARUSE ) THEN

         INDEX = 1
         DO 1 I = 1, NROWS
            CALL SPD_WZCAR( BIAS, ROWLEN, XDATA(INDEX), YDATA(1,I),
     :         VAR(1,I), MDATA, MVAR, STATUS )
            CALL SPD_FACBR( MNDF, COMP, I, NPARA, MDATA, MVAR, STATUS )
            INDEX = INDEX + ROWLEN
 1       CONTINUE

      ELSE IF ( XISND ) THEN

         INDEX = 1
         DO 2 I = 1, NPARA
            MVAR(I) = VAL__BADR
 2       CONTINUE
         DO 3 I = 1, NROWS
            CALL SPD_WZCBR( BIAS, ROWLEN, XDATA(INDEX), YDATA(1,I),
     :         MDATA, STATUS )
            CALL SPD_FACBR( MNDF, COMP, I, NPARA, MDATA, MVAR, STATUS )
            INDEX = INDEX + ROWLEN
 3       CONTINUE

      ELSE IF ( VARUSE ) THEN

         DO 4 I = 1, NROWS
            CALL SPD_WZCAR( BIAS, ROWLEN, XDATA(1), YDATA(1,I),
     :         VAR(1,I), MDATA, MVAR, STATUS )
            CALL SPD_FACBR( MNDF, COMP, I, NPARA, MDATA, MVAR, STATUS )
 4       CONTINUE

      ELSE

         DO 5 I = 1, NPARA
            MVAR(I) = VAL__BADR
 5       CONTINUE
         DO 6 I = 1, NROWS
            CALL SPD_WZCBR( BIAS, ROWLEN, XDATA(1), YDATA(1,I),
     :         MDATA, STATUS )
            CALL SPD_FACBR( MNDF, COMP, I, NPARA, MDATA, MVAR, STATUS )
 6       CONTINUE

      END IF

      END
