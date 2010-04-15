      SUBROUTINE FIG_ISPIKE( NX, XDATA, ENDS, LOGFIT, LINEND,
     :   ORDER, NP, WORK, DATA, STATUS )
*+
*  Name:
*     FIG_ISPIKE

*  Purpose:
*     Interpolate spiketrum into spectrum

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_ISPIKE( NX, XDATA, ENDS, LOGFIT, LINEND,
*        ORDER, NP, WORK, DATA, STATUS )

*  Description:
*     Converts a spiketrum array (one where all but a few values are
*     zero, these representing sample values of what is assumed to be
*     a smooth continuum) into a spectrum (the continuum represented),
*     by spline interpolation between the points, or by global polynomial
*     fitting.
*
*     (If the spiketrum has fewer than 4 points, spline interpolation
*     cannot be used, and a global polynomial of the maximum degree
*     possible will be used instead.)

*  Arguments:
*     NX = INTEGER (Given)
*        The number of points in the spiketrum (and the spectrum to be).
*     XDATA( NX ) = REAL (Given)
*        The x-values for the elements of the spiketrum. The values
*        must be monotonically increasing or decreasing.
*     ENDS( 4 ) = REAL (Given)
*        In case there are values known to the left and right of the X
*        values given in XDATA, these may be specified in ENDS.  ENDS(1)
*        gives an X value that would precede XDATA(1), ENDS(3) an X
*        value that would follow XDATA(NX), and ENDS(2) and (4) are the
*        corresponding data values.  If such values are not available,
*        the ENDS(1) and/or ENDS(3) should be set to zero.
*     LOGFIT = LOGICAL (Given)
*        Fit to the log of the data.
*     LINEND = LOGICAL (Given)
*        Use linear interpolation outside the range of the spiketrum
*        points.
*     ORDER = INTEGER (Given)
*        Order of polynomial to be used for a global fit.  If negative,
*        indicates that spline fitting is to be used. If positive,
*        ORDER must be less than NP. The maximum polynomial order is 10
*        (even if ORDER is given greater than that).
*     NP = INTEGER (Given)
*        The maximum number of spiketrum points - this is really used
*        just to make sure the workspace array is not exceeded.
*
*     WORK( 11*NP + 4 ) = DOUBLE PRECISION (Returned)
*        Workspace. The use of the workspace need not concern the
*        calling routine. Anyway, the use is:
*
*        WORK(1:NP):
*           X input to fit.
*        WORK(NP+1:2*NP):
*           Y input to fit.
*
*        WORK(KPTR:KPTR+NP+4-1):
*           Knots given to PDA_DBINTK.
*        WORK(CPTR:CPTR+NP-1):
*           B spline coefficients returned by PDA_DBINTK.
*        WORK(LPTR:LPTR+(2*4-1)*NP-1):
*           Triangular factorisation returned by PDA_DBINTK.
*
*        WORK(KPTR:KPTR+NP-1):
*           Weights for PDA_DPOLFT.
*        WORK(CPTR:CPTR+NP-1):
*           Fit evaluation returned by PDA_DPOLFT.
*        WORK(LPTR:LPTR+3*NP+3*(ORDER+1)-1):
*           Coefficients returned by PDA_DPOLFT.
*           (Since ORDER+1 <= NP, the above length 7*NP is more
*           demanding.)
*
*        KPTR=2*NP+1
*        CPTR=KPTR+NP+4
*        LPTR=CPTR+NP
*
*     DATA( NX ) = REAL (Given and Returned)
*        Given is the spiketrum, mostly zero values, but up to NP valid
*        spikes. Returned the interpolated spectrum.
*     STATUS = INTEGER (Returned)
*        Return status, 0 for OK, 1 for error.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     21 May 1984 (ks):
*        Original version.
*     26 Mar 1985 (ks):
*        Rewritten to use the NAG library. Note that this involves a
*        change to the calling sequence: WORK is larger, DWORK is no
*        longer used.
*     29 Jan 1987 (ks):
*        If LOGFIT was specified, 1.0 was being added to each element of
*        the result.  This is now fixed.
*     15 Nov 1993 (hme):
*        Fix bug whereby XDATA(1) was used as start point when
*        XDATA(IXST) should be.
*     19 Apr 1995 (hme):
*        Re-write so as to no longer use NAG. The workspace requirement
*        is slightly reduced from 10*NP+24 to 10*NP+4. So there is no
*        need to modify calling routines. Neither is such modification
*        worthwhile.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NX
      REAL XDATA( NX )
      REAL ENDS( 4 )
      LOGICAL LOGFIT
      LOGICAL LINEND
      INTEGER ORDER
      INTEGER NP

*  Arguments Given and Returned:
      REAL DATA( NX )

*  Arguments Returned:
      DOUBLE PRECISION WORK( 11 * NP + 4 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      LOGICAL STFLAG, ENFLAG
      LOGICAL REV
      INTEGER I, IP
      INTEGER IX, IOFF
      INTEGER IXST, IXEN, INCR
      INTEGER IEST, IEND
      INTEGER IPTR
      INTEGER RPTS
      INTEGER KPTR, CPTR, LPTR
      INTEGER IFAIL, IFAIL2, NDEG
      INTEGER INVB
      INTEGER MAXDEG
      REAL AMIN, CVAL, X1, X2, Y1, Y2
      DOUBLE PRECISION EPS, DVAL, DUMMY
      DOUBLE PRECISION WTS( 5 ), R( 5 )
      DOUBLE PRECISION COEFF( 27 ), WORK2( 12 )

*  Internal References:
      DOUBLE PRECISION PDA_DBVALU

*.

*  Set up pointers into the work space.
      KPTR = 2 * NP + 1
      CPTR = KPTR + NP + 4
      LPTR = CPTR + NP

*  Fill up the arrays of pairs of X and Y points, allowing for
*  any end values.  This is complicated not only by the possibility
*  of there being end values in ENDS, but also because, since NAG is
*  fussy about not allowing extrapolation, there MUST be points at
*  the extreme ends of the X range we need to fill in over.  So, if
*  no ENDS values are supplied, and there are no spiketrum points
*  in the end elements, we have to introduce dummy end points which
*  will have to be filled individually before a spline fit can take
*  place, or zero-weighted before a polynomial fit is performed.
*  STFLAG and ENFLAG are used to indicate that the start and end
*  points are in fact dummies.
*  Since NAG is not used any more, it is not clear how important the
*  details are. The code is unchanged.
      STFLAG = .FALSE.
      ENFLAG = .FALSE.
      REV = XDATA(1) .GT. XDATA(NX)
      IF ( REV ) THEN
         IXST = NX
         IXEN =  1
         INCR = -1
         IEST =  3
         IEND =  1
      ELSE
         IXST =  1
         IXEN = NX
         INCR =  1
         IEST =  1
         IEND =  3
      END IF
      IPTR = 0
      IF ( ENDS(IEST) .NE. 0. ) THEN
         IPTR = 1
         WORK(1)    = ENDS(IEST)
         WORK(NP+1) = ENDS(IEST+1)
      ELSE IF ( DATA(IXST) .EQ. 0. ) THEN
         WORK(1)    = XDATA(IXST)
         WORK(NP+1) = 0.
         STFLAG     = .TRUE.
         IPTR       = 1
      END IF
      DO IX = IXST, IXEN, INCR
         IF ( DATA(IX) .NE. 0. ) THEN
            IPTR = MIN( IPTR+1, NP )
            WORK(IPTR)    = XDATA(IX)
            WORK(IPTR+NP) =  DATA(IX)
         END IF
      END DO
      IF ( ENDS(IEND) .NE. 0. ) THEN
         IPTR = MIN(NP,IPTR+1)
         WORK(IPTR)    = ENDS(IEND)
         WORK(IPTR+NP) = ENDS(IEND+1)
      ELSE IF ( DATA(IXEN) .EQ. 0. ) THEN
         IPTR = MIN(NP,IPTR+1)
         WORK(IPTR)    = XDATA(IXEN)
         WORK(IPTR+NP) = 0.
         ENFLAG        = .TRUE.
      END IF

*  RPTS is the 'real' number of spiketrum points.
      RPTS = IPTR
      IF ( STFLAG ) RPTS=RPTS-1
      IF ( ENFLAG ) RPTS=RPTS-1

*  If the interpolation is performed on the log of the data,
*  first make sure none of it is negative.
      IF ( LOGFIT ) THEN
         AMIN = WORK(NP+1)
         DO IP = NP+2, NP+IPTR
            IF ( WORK(IP) .LT. AMIN ) AMIN = WORK(IP)
         END DO
         DO IP = NP+1, NP+IPTR
            WORK(IP) = LOG10(WORK(IP)-AMIN+1.0)
         END DO
      END IF

*  Spline interpolation wanted, and have we enough points?
      STATUS = 1
      IF ( ( ORDER .LT. 0 ) .AND. ( RPTS .GE. 4 ) ) THEN

*     Before we can perform the spline fit, we have to
*     see if we need to generate the dummy end values.  We do this
*     by performing a cubic fit to the end points of the data,
*     giving (nearly) zero weight to the dummy points, then
*     evaluating the fits at those points.
         DO I = 1, 5
            WTS(I) = 1D0
         END DO
         IF ( STFLAG ) THEN
            WTS(1) = 1D-12
            IFAIL2 = 0
            EPS = 0D0
            CALL PDA_DPOLFT( 5, WORK(1), WORK(NP+1), WTS, 3, NDEG, EPS,
     :         R, IFAIL, COEFF, IFAIL2 )
            IF ( NDEG .NE. 3 .OR. IFAIL .NE. 1 .OR. IFAIL2 .NE. 0 ) THEN
               CALL PAR_WRUSER( 'Error in PDA_DPOLFT', I )
               GO TO 500
            END IF
            WORK(NP+1) = R(1)
            WTS(1) = 1D0
         END IF
         IF ( ENFLAG ) THEN
            WTS(5) = 1D-12
            IFAIL2 = 0
            EPS = 0D0
            CALL PDA_DPOLFT( 5, WORK(IPTR-4), WORK(NP+IPTR-4), WTS,
     :         3, NDEG, EPS, R, IFAIL, COEFF, IFAIL2 )
            IF ( NDEG .NE. 3 .OR. IFAIL .NE. 1 .OR. IFAIL2 .NE. 0 ) THEN
               CALL PAR_WRUSER( 'Error in PDA_DPOLFT', I )
               GO TO 500
            END IF
            WORK(NP+IPTR) = R(5)
         END IF

*     Finally, we get to do the spline fit.
*     First create the outer knots.
*     Then work out the interior knots.
         DO I = 1, 4
            WORK(KPTR-1+I)      = WORK(1)
            WORK(KPTR-1+IPTR+I) = WORK(IPTR)
         END DO
         DO I = 5, IPTR
            WORK(KPTR-1+I) = WORK(I-2)
         END DO
         IFAIL2=0
         CALL PDA_DBINTK( WORK(1), WORK(NP+1), WORK(KPTR), IPTR, 4,
     :      WORK(CPTR), WORK(LPTR), WORK2, IFAIL2 )
         IF ( IFAIL2 .NE. 0 ) THEN
            CALL PAR_WRUSER( 'Error in PDA_DBINTK', I )
            GO TO 500
         END IF

*     and then, evaluate the fit at each point in the spectrum.
         INVB = 1
         IFAIL2 = 0
         DO IX = 1, NX
            DATA(IX) = PDA_DBVALU( WORK(KPTR), WORK(CPTR), IPTR, 4, 0,
     :         DBLE(XDATA(IX)), INVB, WORK2, IFAIL2 )
            IF ( IFAIL2 .NE. 0 ) THEN
               CALL PAR_WRUSER( 'Error in PDA_DBVALU', I )
               GO TO 500
            END IF
         END DO
         STATUS = 0

*  Else (no spline interpolation).
      ELSE

*     Do the best we can with what we have.  If no points
*     are available, leave the data as it is - all zero.

*     If one point is given, set all values to match it.
         IF ( RPTS .EQ. 1 ) THEN
            IF ( STFLAG ) THEN
               CVAL = WORK(NP+2)
            ELSE
               CVAL = WORK(NP+1)
            END IF
            DO IX = 1, NX
               DATA(IX) = CVAL
            END DO

*     Else if two or more points are given, use a polynomial fit.
         ELSE IF ( RPTS .GT. 1 ) THEN

*        Perform the polynomial fit.  If there are dummy
*        end points set their weights to 'zero'.  Set all
*        other weights to 1.
            IF ( ORDER .GE. 0 ) THEN
               MAXDEG = MIN( 10, RPTS-1, ORDER )
            ELSE
               MAXDEG = MIN( 10, RPTS-1 )
            END IF
            DO IP = 1, IPTR
               WORK(KPTR-1+IP) = 1D0
            END DO
            IF ( STFLAG ) WORK(KPTR)        = 1D-12
            IF ( ENFLAG ) WORK(KPTR-1+IPTR) = 1D-12
            IFAIL2 = 0
            EPS = 0D0
            CALL PDA_DPOLFT( IPTR, WORK(1), WORK(NP+1), WORK(KPTR),
     :         MAXDEG, NDEG, EPS, WORK(CPTR), IFAIL, WORK(LPTR),
     :         IFAIL2 )
            IF ( NDEG .NE. MAXDEG .OR. IFAIL  .NE. 1 .OR.
     :                                 IFAIL2 .NE. 0 ) THEN
               CALL PAR_WRUSER( 'Error in PDA_DPOLFT', I )
               GO TO 500
            END IF

*        Evaluate the polynomial for each element.
            IFAIL2 = 0
            DO IX = 1, NX
               CALL PDA_DP1VLU( MAXDEG, 0, DBLE(XDATA(IX)), DVAL, DUMMY,
     :            WORK(LPTR), IFAIL2 )
               IF ( IFAIL2 .NE. 0 ) THEN
                  CALL PAR_WRUSER( 'Error in PDA_DP1VLU', I )
                  GO TO 500
               END IF
               DATA(IX) = DVAL
            END DO
         END IF
         STATUS = 0

      END IF

*  If end values are to be set using linear interpolation,
*  recalculate them.
      IF ( LINEND .AND. ( RPTS .GE. 2 ) ) THEN
         IOFF = 1
         IF ( STFLAG ) IOFF = 2
         X1 = WORK(IOFF)
         X2 = WORK(IOFF+1)
         Y1 = WORK(NP+IOFF)
         Y2 = WORK(NP+IOFF+1)
         IX = 1
         DO WHILE ( XDATA(IX) .LT. X1 )
            DATA(IX) = Y1
     :               + ( Y2 - Y1 ) * ( XDATA(IX) - X1 ) / ( X2 - X1 )
            IX = IX + 1
         END DO
         IOFF = 0
         IF ( ENFLAG ) IOFF = 1
         X1 = WORK(IPTR-IOFF-1)
         X2 = WORK(IPTR-IOFF)
         Y1 = WORK(NP+IPTR-IOFF-1)
         Y2 = WORK(NP+IPTR-IOFF)
         IX = NX
         DO WHILE ( XDATA(IX) .GT. X2 )
            DATA(IX) = Y1
     :               + ( Y2 - Y1 ) * ( XDATA(IX) - X1 ) / ( X2 - X1 )
            IX = IX - 1
         END DO
      END IF

*  Now, back to a linear scale, if necessary.
      IF ( LOGFIT ) THEN
         DO IX = 1, NX
            DATA(IX) = ( 10 ** DATA(IX) ) + AMIN - 1.0
         END DO
      END IF

*  Return.
 500  CONTINUE

      END
