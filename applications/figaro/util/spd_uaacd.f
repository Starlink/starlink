      SUBROUTINE SPD_UAACD( VARUSE, IDIM, IELM, OELM,
     :   BADVAL, IDIMS, ODIMS, INDAT, INVAR,
     :   BADDAT, BADVAR, OUTDAT, OUTVAR, SIZE, STATUS )
*+
*  Name:
*     SPD_UAAC{DR}

*  Purpose:
*     Average a cube along one or more axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAACD( VARUSE, IDIM, IELM, OELM, BADVAL, IDIMS, ODIMS,
*        INDAT, INVAR, BADDAT, BADVAR, OUTDAT, OUTVAR, SIZE, STATUS )

*  Description:
*     This routine reduces an N-dimensional cube into an
*     (N-M)-dimensional one by averaging the pixels that differ only in
*     their position along the disappearing axes. The average can be
*     weighted with reciprocal variances, if variances are available.
*     The output will in any case have associated variances, either by
*     propagation of the input variances, or calculated from the
*     standard deviation of each pixel average.

*  Arguments:
*     VARUSE = LOGICAL (Given)
*        True if INVAR exists.
*     IDIM = INTEGER (Given)
*        The no. of axes in input.
*     IELM = INTEGER (Given)
*        Length of the input arrays, which are assumed one-dimensional.
*     OELM = INTEGER (Given)
*        Length of the output arrays, which are assumed one-dimensional.
*     BADVAL = DOUBLE PRECISION (Given)
*        The bad value. Pixels with data or variance equal to this value
*        are ignored in the averaging. Also, this value is used to mark
*        unknown averages or variances in the output. Usually this
*        argument should be given as VAL__BADD.
*     IDIMS( IDIM ) = INTEGER (Given)
*        Lengths of axes in input.
*     ODIMS( IDIM ) = INTEGER (Given)
*        Lengths of axes in output. Contrary to what is used for the
*        output file, the i-th element of ODIMS must correspond to the
*        i-th element of IDIMS, having either the same value (retained
*        axis) or the value 1 (collapsed axis).
*     INDAT( IELM ) = DOUBLE PRECISION (Given)
*        Data array to extract from.
*     INVAR( IELM ) = DOUBLE PRECISION (Given)
*        Variance array going with INDAT.
*     BADDAT = LOGICAL (Returned)
*        True, if a bad data value is created.
*     BADVAR = LOGICAL (Returned)
*        True, if a bad variance value is created.
*     OUTDAT( OELM ) = DOUBLE PRECISION (Returned)
*        Extracted data array. Used internally to add up the data
*        values.
*     OUTVAR( OELM ) = DOUBLE PRECISION (Returned)
*        Variance array going with OUTDAT. Used internally to add up
*        either (i) the reciprocal variances (VARUSE) or (ii) the
*        squared data values (.NOT.VARUSE).
*     SIZE( OELM ) = INTEGER (Returned)
*        Used to count the input pixels contributing to any output
*        pixel. This is used only if .NOT.VARUSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     06 Mar 1991 (hme):
*        Original.
*     27 Jun 1991 (hme):
*        Cleanup, while EXTRACT does on-the-fly subset.
*     25 Nov 1991 (hme):
*        Change error report.
*     13 Jun 1992 (hme):
*        Reduce to 7-D.
*     03 May 1993 (hme):
*        For VARUSE false set numerically negative variances to zero.
*     05 May 1994 (hme):
*        Change from EXTRC{DR}.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL VARUSE
      INTEGER IDIM
      INTEGER IELM
      INTEGER OELM
      DOUBLE PRECISION BADVAL
      INTEGER IDIMS( IDIM )
      INTEGER ODIMS( IDIM )
      DOUBLE PRECISION INDAT( IELM )
      DOUBLE PRECISION INVAR( IELM )

*  Arguments Returned:
      LOGICAL BADDAT
      LOGICAL BADVAR
      DOUBLE PRECISION OUTDAT( OELM )
      DOUBLE PRECISION OUTVAR( OELM )
      INTEGER SIZE( OELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER MAXDIM             ! Max. number of axes
      PARAMETER ( MAXDIM = 7 )
      DOUBLE PRECISION ZEROD     ! Itself
      PARAMETER ( ZEROD = 0D0 )
      DOUBLE PRECISION ONED      ! Itself
      PARAMETER ( ONED = 1D0 )
      REAL ZEROR                 ! Itself
      PARAMETER ( ZEROR = 0. )
      REAL ONER                  ! Itself
      PARAMETER ( ONER = 1. )

*  Local Variables:
      INTEGER IDIMC( MAXDIM )    ! Local copy of IDIMS
      INTEGER ODIMC( MAXDIM )    ! Local copy of ODIMS
      INTEGER OINC( MAXDIM )     ! OIND-increments
      INTEGER IIND, OIND         ! Index for IN and OUT
      INTEGER N1, N2, N3, N4, N5, N6, N7 ! Loop indices
      DOUBLE PRECISION DATA, VAR

*.

*  Check and set STATUS.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( IDIM .GT. MAXDIM  ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'XTRACT_MAXDIM',
     :      'XTRACT: Error: IN has too many axes.', STATUS )
         GO TO 500
      ENDIF

*  We loop through a 7-D input array INDAT now, since this is
*  larger than OUTDAT. The array indices are N1 ... Ni ... N7.
*  However we use 1-D arrays for addressing the array elements, the
*  indices being IIND and OIND.
*  IIND increases always by 1 as we loop through INDAT.
*  OIND could be calculated from:
*
*  OIND =   MIN(N1,ODIMS(1))
*         + SUM{i=2,IDIM}
*              [ (MIN(Ni,ODIMS(i)) - 1) * PROD{j=1,i-1} [ODIMS(j)] ]
*
*  ODIMS is the 7-D size of OUTDAT, its values are 1 for a collapsed
*  axis and IDIMS for a retained axis. Thus the MIN functions take values
*  of 1 (collapsed) or Ni (retained).
*
*  It is faster to increment OIND instead of recalculating it. For this,
*  we work out the increments necessary when any index Ni is increased
*  by 1. These increments are:
*
*  OINC(1) = 0 for collapsed axis
*            1 for retained axis
*
*  OINC(i) =   ( MIN(Ni+1,ODIMS(i)) - MIN(Ni,ODIMS(i)) )
*            * PROD{j=1,i-1} [ODIMS(j)]
*
*          = 0                       for collapsed axis
*            PROD{j=1,i-1} [ODIMS(j)] for retained axis

*  Make filled local copy of dimensions.
*  OIND increments, preliminary values.
      OINC(1) = 1
      DO 1 N1 = 1, MAXDIM
         IF ( N1 .LE. IDIM ) THEN
            IDIMC(N1) = IDIMS(N1)
            ODIMC(N1) = ODIMS(N1)
         ELSE
            IDIMC(N1) = 1
            ODIMC(N1) = 1
         END IF
         IF ( N1 .GT. 1 ) OINC(N1) = OINC(N1-1) * ODIMC(N1-1)
 1    CONTINUE

*  OIND increments, actual values.
      DO 2 N1 = 1, MAXDIM
         IF ( ODIMC(N1) .EQ. 1 ) OINC(N1) = 0
 2    CONTINUE

*  Initialize.
      DO 3 OIND = 1, OELM
         OUTDAT(OIND) = ZEROD
         OUTVAR(OIND) = ZEROD
         SIZE  (OIND) = 0
 3    CONTINUE
      OIND   = 1
      IIND   = 1
      BADDAT = .FALSE.
      BADVAR = .FALSE.

*  7-dimensional loop through INDAT, INVAL to add up data,
*  data squared, reciprocal variances, pixel numbers.
*  This loop is coded separately for VARUSE and .NOT.VARUSE.
      IF ( VARUSE ) THEN
          DO 10 N7 = 1, IDIMC(7)
           DO  9 N6 = 1, IDIMC(6)
            DO  8 N5 = 1, IDIMC(5)
             DO  7 N4 = 1, IDIMC(4)
              DO  6 N3 = 1, IDIMC(3)
               DO  5 N2 = 1, IDIMC(2)
                DO  4 N1 = 1, IDIMC(1)
                 DATA = INDAT(IIND)
                 VAR  = INVAR(IIND)
                 IF ( DATA .NE. BADVAL .AND.
     :                 VAR .NE. BADVAL .AND. VAR .NE. ZEROD ) THEN
                    OUTDAT(OIND) = OUTDAT(OIND) + DATA / VAR
                    OUTVAR(OIND) = OUTVAR(OIND) +  ONED / VAR
                 ENDIF
                 IIND = IIND + 1
                 OIND = OIND + OINC(1)
 4              CONTINUE
                OIND = OIND - ODIMC(1) * OINC(1) + OINC(2)
 5             CONTINUE
               OIND = OIND - ODIMC(2) * OINC(2) + OINC(3)
 6            CONTINUE
              OIND = OIND - ODIMC(3) * OINC(3) + OINC(4)
 7           CONTINUE
             OIND = OIND - ODIMC(4) * OINC(4) + OINC(5)
 8          CONTINUE
            OIND = OIND - ODIMC(5) * OINC(5) + OINC(6)
 9         CONTINUE
           OIND = OIND - ODIMC(6) * OINC(6) + OINC(7)
 10       CONTINUE
      ELSE
          DO 20 N7 = 1, IDIMC(7)
           DO 19 N6 = 1, IDIMC(6)
            DO 18 N5 = 1, IDIMC(5)
             DO 17 N4 = 1, IDIMC(4)
              DO 16 N3 = 1, IDIMC(3)
               DO 15 N2 = 1, IDIMC(2)
                DO 14 N1 = 1, IDIMC(1)
                 DATA = INDAT(IIND)
                 IF ( DATA .NE. BADVAL ) THEN
                    OUTDAT(OIND) = OUTDAT(OIND) + DATA
                    OUTVAR(OIND) = OUTVAR(OIND) + DATA * DATA
                    SIZE  (OIND) = SIZE  (OIND) + 1
                 ENDIF
                 IIND = IIND + 1
                 OIND = OIND + OINC(1)
 14             CONTINUE
                OIND = OIND - ODIMC(1) * OINC(1) + OINC(2)
 15            CONTINUE
               OIND = OIND - ODIMC(2) * OINC(2) + OINC(3)
 16           CONTINUE
              OIND = OIND - ODIMC(3) * OINC(3) + OINC(4)
 17          CONTINUE
             OIND = OIND - ODIMC(4) * OINC(4) + OINC(5)
 18         CONTINUE
            OIND = OIND - ODIMC(5) * OINC(5) + OINC(6)
 19        CONTINUE
           OIND = OIND - ODIMC(6) * OINC(6) + OINC(7)
 20       CONTINUE
      ENDIF

*  Loop through OUTDAT, OUTVAR, SIZE to work out the actual averages and
*  variances. This loop is coded separately for VARUSE and .NOT.VARUSE.
      IF ( VARUSE ) THEN
         DO 24 OIND = 1, OELM
            IF ( OUTVAR(OIND) .NE. ZEROD ) THEN
               OUTDAT(OIND) = OUTDAT(OIND) / OUTVAR(OIND)
               OUTVAR(OIND) =          ONED / OUTVAR(OIND)
            ELSE
               OUTDAT(OIND) = BADVAL
               OUTVAR(OIND) = BADVAL
               BADDAT       = .TRUE.
               BADVAR       = .TRUE.
            ENDIF
 24      CONTINUE
      ELSE
         DO 25 OIND = 1, OELM
            IF ( SIZE(OIND) .GT. 0 ) THEN
               OUTDAT(OIND) = OUTDAT(OIND) / FLOAT(SIZE(OIND))
               IF ( SIZE(OIND) .NE. 1 ) THEN
                  OUTVAR(OIND) = MAX( ZEROD,
     :               ( OUTVAR(OIND)
     :                 - FLOAT(SIZE(OIND))
     :                   * OUTDAT(OIND) * OUTDAT(OIND) )
     :               / FLOAT( SIZE(OIND) * ( SIZE(OIND) - 1 ) ) )
               ELSE
                  OUTVAR(OIND) = BADVAL
                  BADVAR       = .TRUE.
               ENDIF
            ELSE
               OUTDAT(OIND) = BADVAL
               OUTVAR(OIND) = BADVAL
               BADDAT       = .TRUE.
               BADVAR       = .TRUE.
            ENDIF
 25      CONTINUE
      ENDIF

*  That's all.
 500  CONTINUE

      END
