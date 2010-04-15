      SUBROUTINE SPD_WAAAR( VARUSE, COVRSX, INELM, MSKDIM,
     :   INX, INDAT, INVAR, INCVRS,
     :   MSKUSE, MASK, MSKELM, MXDWC, STATUS )
*+
*  Name:
*     SPD_WAAA{DR}

*  Purpose:
*     Mask x, data, weights, covariance row sums.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WAAAR( VARUSE, COVRSX, INELM, MSKDIM,
*        INX, INDAT, INVAR, INCVRS,
*        MSKUSE, MASK, MSKELM, MXDWC, STATUS )

*  Description:
*     This routine performs the masking of one-dimensional data. Up to 6
*     mask intervals are possible. The masked data are also free of bad
*     values in data or variance and free of zero-variance data. The
*     parameters describing the mask may be read from the ADAM
*     parameters MASK1, MASK2.
*
*     If the given abscissa values seem to be decreasing (if the last x
*     value is smaller than the first), then the order of data is
*     reversed in the masked arrays. Hopefully the given abscissa values
*     are monotonic, in which case the masked data will be monotonic and
*     increasing.

*  Arguments:
*     VARUSE = INTEGER (Given)
*        Non-zero if errors available and to be used.
*     COVRSX = INTEGER (Given)
*        Non-zero if covariance row sums are available and to be used. This
*        flag is taken into account only if VARUSE is non-zero.
*     INELM = INTEGER (Given)
*        Size of arrays (except MASK).
*     MSKDIM = INTEGER (Given)
*        Size of MASK array. Must be even.
*     INX( INELM ) = REAL (Given)
*        Given x value array. Data passing the mask are required to have
*        INX(I) inside one of the mask intervals.
*     INDAT( INELM ) = REAL (Given)
*        Given data array. Data passing the mask are required to have
*        a non-bad INDAT(I).
*     INVAR( INELM ) = REAL (Given)
*        Given error or variance array. Data passing the mask are
*        required to have an INVAR(I) that is neither bad, nor zero, nor
*        negative.
*     INCVRS( INELM ) = REAL (Given)
*        Given covariance row sum array. Data passing the mask are
*        required to have an INCVRS(I) that is neither bad, nor zero.
*     MSKUSE = INTEGER (Given)
*        Number of valid mask intervals.
*     MASK( MSKDIM ) = REAL (Given)
*        The mask is put together from up to MSKDIM/2 intervals:
*           complex mask = [MASK(1);MASK(1+MSKDIM/2)]
*                        U [MASK(2);MASK(2+MSKDIM/2)]
*                        U ...
*                        U [MASK(MSKUSE);MASK(MSKUSE+MSKDIM/2)].
*        The elements of MASK are not checked for monotony. Thus
*        intervals may be empty or overlapping.
*     MSKELM = INTEGER (Returned)
*        Number of data points that passed the mask.
*     MXDWC( 4*INELM ) = REAL (Returned)
*        The packed array of masked x, data, weight and covariance row
*        sums. Weights are defined as 1/INVAR. All weights are set to 1
*        if VARUSE.EQ.0. The 2nd weights are defined as 1/INCVRS. They
*        are not set to any particular value if covariance row sums are
*        unavailable.
*        MXDWC(          1 :   MSKELM ): x values,
*        MXDWC(   MSKELM+1 : 2*MSKELM ): data values,
*        MXDWC( 2*MSKELM+1 : 3*MSKELM ): weight values,
*        MXDWC( 3*MSKELM+1 : 4*MSKELM ): 2nd weight values.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set to SAI__ERROR if the none of the
*        given data pass the mask.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 May 1991 (hme):
*        Original version (DOMASK).
*     03 May 1991 (hme):
*        PAR_ sequence DEF, GET, CANCL, always define and get all
*        elements. Reduce MASK to size 12 and split up into two
*        parameter arrays of size 6.
*        Take precautions for errors in last bit in the PAR_DEF/PAR_GET
*        sequence.
*     31 May 1991 (hme):
*        Try a different way to deal with array parameters.
*     26 Jun 1991 (hme):
*        Parameter MSKUSE.
*        Sort data if reverse in x. Error handling.
*        Masked x, data, weights are DOUBLE.
*     19 Jul 1991 (hme):
*        Pack masked x, data, weights into one array.
*     22 Jul 1991 (hme):
*        MSKUSE no longer parameter.
*     25 Nov 1991 (hme):
*        Change error reporting.
*     23 Apr 1992 (hme):
*        Adapt from SPFMSK. Output can be _DOUBLE or _REAL. Covariance
*        row sums can be masked with the other three arrays. No
*        assumption about meaning of INVAR, weights always 1/INVAR.
*     22 Apr 1994 (hme):
*        Adapt from SPABXR. Simplify: no ADAM parameters, no
*        calculation of extrema.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER VARUSE
      INTEGER COVRSX
      INTEGER INELM
      INTEGER MSKDIM
      REAL INX(    INELM )
      REAL INDAT(  INELM )
      REAL INVAR(  INELM )
      REAL INCVRS( INELM )

*  Arguments Given and Returned:
      INTEGER MSKUSE
      REAL MASK( MSKDIM )

*  Arguments Returned:
      INTEGER MSKELM
      REAL MXDWC( 4*INELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop index
      INTEGER ISTA, IEND, ISTP   ! Loop start, end, step
      REAL DAT                   ! INDAT element
      REAL X                     ! INX element
      REAL VAR                   ! INVAR element
      LOGICAL TAKE               ! True if x value within mask

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Prepare for masking.
      MSKELM = 0

*  Forward or reverse loop through data.
      IF ( INX(1) .LE. INX(INELM) ) THEN
         ISTA = 1
         IEND = INELM
         ISTP = 1
      ELSE
         ISTA = INELM
         IEND = 1
         ISTP = -1
      END IF

*  This routine is coded in four loops, one for each combination of (i)
*  VARUSE being true or false and (ii) MSKUSE being positive or zero.
*  ====================================================================

*  If variance to be used and mask non-trivial.
      IF ( VARUSE .NE. 0 .AND. MSKUSE .GT. 0 ) THEN
         DO 2 I = ISTA, IEND, ISTP
            X = INX(I)
            DAT = INDAT(I)
            VAR = INVAR(I)

*        X value must be in one of the mask intervals.
            TAKE = .FALSE.
            DO 1 J = 1, MSKUSE
               IF ( X .GE. MASK(J) .AND. X .LE. MASK(J+MSKDIM/2) )
     :            TAKE = .TRUE.
 1          CONTINUE

*        Data and variance must be non-bad, variance must be positive.
            IF ( DAT .EQ. VAL__BADR ) TAKE = .FALSE.
            IF ( VAR .EQ. VAL__BADR  .OR.  VAR .LE. 0. ) TAKE = .FALSE.

*        Covariance row sum must be non-bad and non-zero.
            IF ( TAKE .AND. COVRSX .NE. 0 ) THEN
               IF ( INCVRS(I) .EQ. VAL__BADR .OR. INCVRS(I) .EQ. 0. )
     :            TAKE = .FALSE.
            END IF

*        If the data point passed the mask.
            IF ( TAKE ) THEN

*           Copy data to masked data.
               MSKELM = MSKELM + 1
               MXDWC(        MSKELM) = X
               MXDWC(  INELM+MSKELM) = DAT
               MXDWC(2*INELM+MSKELM) = 1./VAR
               IF (COVRSX.NE.0) MXDWC(3*INELM+MSKELM) = 1./INCVRS(I)
            END IF
 2       CONTINUE

*  Else if mask non-trivial (and variance to be ignored).
      ELSE IF ( MSKUSE .GT. 0 ) THEN
         DO 4 I = ISTA, IEND, ISTP
            X = INX(I)
            DAT = INDAT(I)

*        X value must be in one of the mask intervals.
            TAKE = .FALSE.
            DO 3 J = 1, MSKUSE
               IF ( X .GE. MASK(J) .AND. X .LE. MASK(J+MSKDIM/2) )
     :            TAKE = .TRUE.
 3          CONTINUE

*        Data must be non-bad.
            IF ( TAKE  .AND.  DAT .NE. VAL__BADR ) THEN

*           Copy data to masked data. Weights are set 1.
               MSKELM = MSKELM + 1
               MXDWC(        MSKELM) = X
               MXDWC(  INELM+MSKELM) = DAT
               MXDWC(2*INELM+MSKELM) = 1.
            END IF
 4       CONTINUE

*  Else if variance to be used (and mask is trivial).
      ELSE IF ( VARUSE .NE. 0 ) THEN
         DO 5 I = ISTA, IEND, ISTP
            X = INX(I)
            DAT = INDAT(I)
            VAR = INVAR(I)

*        Data and variance must be non-bad, variance must be positive.
            TAKE = .TRUE.
            IF ( DAT .EQ. VAL__BADR ) TAKE = .FALSE.
            IF ( VAR .EQ. VAL__BADR  .OR.  VAR .LE. 0. ) TAKE = .FALSE.

*        Covariance row sum must be non-bad and non-zero.
            IF ( TAKE .AND. COVRSX .NE. 0 ) THEN
               IF ( INCVRS(I) .EQ. VAL__BADR .OR. INCVRS(I) .EQ. 0. )
     :            TAKE = .FALSE.
            END IF

*        If the data point passed the mask.
            IF ( TAKE ) THEN

*           Copy data to masked data.
               MSKELM = MSKELM + 1
               MXDWC(        MSKELM) = X
               MXDWC(  INELM+MSKELM) = DAT
               MXDWC(2*INELM+MSKELM) = 1./VAR
               IF (COVRSX.NE.0) MXDWC(3*INELM+MSKELM) = 1./INCVRS(I)
            END IF
 5       CONTINUE

*  Else (variance to be ignored and mask is trivial).
      ELSE
         DO 6 I = ISTA, IEND, ISTP
            X = INX(I)
            DAT = INDAT(I)

*        Data must be non-bad.
            IF ( DAT .NE. VAL__BADR ) THEN

*           Copy data to masked data. Weights are set 1.
               MSKELM = MSKELM + 1
               MXDWC(        MSKELM) = X
               MXDWC(  INELM+MSKELM) = DAT
               MXDWC(2*INELM+MSKELM) = 1.
            END IF
 6       CONTINUE
      END IF

*  Error handling.
      IF ( MSKELM .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WAAA_E01', 'SPD_WAAAR: ' //
     :      'Error: Mask is empty.', STATUS )
         GO TO 500
      END IF

*  Pack the masked arrays.
*  We now know that each array is only MSKELM long, not INELM. So we can
*  move the 2nd, 3rd, and possibly 4th array forward.
      DO 7 I = 1, MSKELM
         MXDWC(MSKELM+I) = MXDWC(INELM+I)
 7    CONTINUE
      DO 8 I = 1, MSKELM
         MXDWC(2*MSKELM+I) = MXDWC(2*INELM+I)
 8    CONTINUE
      IF ( COVRSX .NE. 0 ) THEN
         DO 9 I = 1, MSKELM
            MXDWC(3*MSKELM+I) = MXDWC(3*INELM+I)
 9       CONTINUE
      END IF

*  Return.
 500  CONTINUE
      END
