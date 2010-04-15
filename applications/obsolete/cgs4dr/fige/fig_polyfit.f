*+   FIG_POLYFIT - Fits polynomial to (sky) data points
      SUBROUTINE FIG_POLYFIT( NX, NY, NW, I, V, Q, X, Y, W, W1,
     :    SAYS1, SAYE1, SAYS2, SAYE2, SAYS3,
     :    SAYE3, SAYS4, SAYE4, DEGREE, REJECT, WEIGHT )
*    Description:
*     Subroutine to perform the work of the POLYSKY command
*     A polynomial fit is performed along each column of the
*     input image to the points within the several Y fields specified.
*     This polynomial is then subtracted from the image in this
*     area. The data outside the areas covered by the two fields is unchanged.
*    Invocation:
*     CALL FIG_POLYFIT ( arg-list )
*    Parameters:
*     NX           = INTEGER( READ )
*          The first dimension of IMAGE
*     NY           = INTEGER( READ )
*          The second dimension of IMAGE
*     NW           = INTEGER( READ )
*          Size of workspace array
*     I            = REAL ARRAY( MODIFIED )
*          The image array
*     V            = REAL ARRAY( READ )
*          The variances on the image
*     Q            = BYTE ARRAY( READ )
*          The quality array
*     X            = DOUBLE ARRAY( MODIFIED )
*          Array of X values for fit
*     Y            = DOUBLE ARRAY( MODIFIED )
*          Array of Y values for fit
*     W            = DOUBLE ARRAY( MODIFIED )
*          Array of weights for fit
*     W1           = DOUBLE  ARRAY( MODIFIED )
*          Workspace array for E02ADF
*     SAYS1        = INTEGER( READ )
*          The first Y value to for sky field 1
*     SAYE1        = INTEGER( READ )
*          The last Y value to for sky field 1
*     SAYS2        = INTEGER( READ )
*          The first Y value to for sky field 2
*     SAYE2        = INTEGER( READ )
*          The last Y value to for sky field 2
*     SAYS3        = INTEGER( READ )
*          The first Y value to for sky field 3
*     SAYE3        = INTEGER( READ )
*          The last Y value to for sky field 3
*     SAYS4        = INTEGER( READ )
*          The first Y value to for sky field 4
*     SAYE4        = INTEGER( READ )
*          The last Y value to for sky field 4
*     DEGREE       = INTEGER( READ )
*          Degree of polynomial to fit
*     REJECT       = INTEGER( READ )
*          Number of points to reject
*     WEIGHT       = LOGICAL( READ )
*          TRUE if variances are to be used as weights
*    Authors:
*     Jeremy Bailey (JACH::JAB)
*     Phil Daly     (JACH::PND)
*    History:
*     07-Mar-1991: Original version                                    (JAB)
*     02-Jul-1991: Convert to CGS4 data reduction system               (PND)
*     09-Jan-1996: Remove NAG E02ADF->PDA_DPOLFT, E02AEF->PDA_DP1VLU   (PND)
*    endhistory
*    Type Definitions:
      IMPLICIT NONE      ! Inhibit implicit type definitions
*    Status:
      INTEGER STATUS     ! DSA status
*    External references:
*     GEN_FILL           ! (GEN_ package) Fill an array of bytes with constant
*     ICH_CI             ! (ICH_ package) Convert integer to string
*    Local variables:
      INTEGER
     : NX,               ! First dimension of input image
     : NY,               ! Second dimension of input image
     : NW,               ! Size of work arra y
     : SAYS1,            ! Start of first area
     : SAYE1,            ! End of first area
     : SAYS2,            ! Start of second area
     : SAYE2,            ! End of second area
     : SAYS3,            ! Start of third area
     : SAYE3,            ! End of third area
     : SAYS4,            ! Start of fourth area
     : SAYE4,            ! End of fourth area
     : DEGREE,           ! Degree of polynomial to fit
     : REJECT,           ! Number of points to reject from fit
     : MAX_DEGREE,       ! Maximum degree of polynomial
     : NGOOD             ! Number of good points for fit
      INTEGER
     : KPLUS1,           ! Maximum degree plus 1
     : NROWS,            ! Number of rows in A
     : IFAIL,            ! Status return of PDA routine
     : IFAIL2,           ! Status return of PDA routine
     : IX,               ! X index
     : IY,               ! Y index
     : INDEX,            ! An index value
     : IGNORE,           ! An ignoreable status value
     : NREJ,             ! A counter
     : IMAX,             ! Max reject number
     : MINSKY,           ! Minimum sky coordinate
     : MAXSKY            ! Maximum sky coordinate
      PARAMETER
     : (MAX_DEGREE = 10) ! Maximum degree for polynomial fit
      BYTE
     : Q(NX,NY)          ! Quality array
      LOGICAL
     : WEIGHT,           ! TRUE if errors are to be used
     : ERRORS            ! TRUE if errors are to be used
      REAL
     : I(NX,NY),         ! Image array
     : V(NX,NY),         ! Variance array
     : FUNKNOWN_IN       ! Unknown value
      DOUBLE PRECISION
     : X(NY),            ! Array of X values
     : Y(NY),            ! Array of Y values
     : W(NY),            ! Array of weights
     : W1(NW),           ! Array for NAG
     : DX,               ! Subtraction of MAXSKY and MINSKY for E02AEF
     : DX2,              ! Addition of MAXSKY and MINSKY for E02AEF
     : XCAP,             ! Y average for E02AEF
     : VALUE,            ! The value of the polynomial
     : R,                ! Deviation from mean
     : RMAX,             ! Maximum deviation
     : MEAN              ! Mean value of data points
      CHARACTER
     : ICH_CI*14         ! Integer to character
* PDA variables
      INTEGER NDEG
      DOUBLE PRECISION EPS, DUMMY(1)
*-

*    Set status values

      IGNORE = 0
      STATUS = 0

*    Find maximum and minimum sky coordinates

      IF ( SAYS1 .LE. 0) SAYS1 = NY + 1
      IF ( SAYE1 .LE. 0) SAYE1 = NY + 1
      IF ( SAYS2 .LE. 0) SAYS2 = NY + 1
      IF ( SAYE2 .LE. 0) SAYE2 = NY + 1
      IF ( SAYS3 .LE. 0) SAYS3 = NY + 1
      IF ( SAYE3 .LE. 0) SAYE3 = NY + 1
      IF ( SAYS4 .LE. 0) SAYS4 = NY + 1
      IF ( SAYE4 .LE. 0) SAYE4 = NY + 1

      MINSKY = MIN( SAYS1, SAYE1, SAYS2, SAYE2,
     :                SAYS3, SAYE3, SAYS4, SAYE4 )

      IF ( SAYS1 .GT. NY) SAYS1 = -1
      IF ( SAYE1 .GT. NY) SAYE1 = -1
      IF ( SAYS2 .GT. NY) SAYS2 = -1
      IF ( SAYE2 .GT. NY) SAYE2 = -1
      IF ( SAYS3 .GT. NY) SAYS3 = -1
      IF ( SAYE3 .GT. NY) SAYE3 = -1
      IF ( SAYS4 .GT. NY) SAYS4 = -1
      IF ( SAYE4 .GT. NY) SAYE4 = -1

      MAXSKY = MAX( SAYS1, SAYE1, SAYS2, SAYE2,
     :                SAYS3, SAYE3, SAYS4, SAYE4 )

*    Get a flag value

      CALL DSA_GET_FLAG_VALUE( 'FLOAT', FUNKNOWN_IN, STATUS )

*     Set up parameters needed for E02ADF / E02AEF

      KPLUS1 = DEGREE + 1
      NROWS  = MAX_DEGREE + 1
      DX     = DBLE( MAXSKY - MINSKY )
      DX2    = DBLE( MAXSKY + MINSKY )

*     Loop over columns of data

      DO IX = 1,NX

*
*     Build array of values to do polynomial fit to. These are
*     used as the X,Y and W parameters of E02ADF
*     The X value is the Y index in the original array
*     The Y value is the data value in the array.
*     The W value (weight):  is 1 if the errors are zero or unknown
*                            is 1/uncertainty if the error is nonzero
*     NOTE - the weight needed by E02ADF is 1/uncertainty, not 1/variance.
*     This should correctly handle the case where there are no errors
*     in the original data.
*
          INDEX = 1
*
*     First go through the data and count the good points, and
*     find out if all the points have known nonzero variances
*
          ERRORS = WEIGHT
          NGOOD  = 0

*     First sky region

          IF ( ( SAYS1.GE.MINSKY ) .AND. ( SAYS1.LE.MAXSKY ) .AND.
     :         ( SAYE1.GE.MINSKY ) .AND. ( SAYE1.LE.MAXSKY ) ) THEN
             DO IY = SAYS1, SAYE1
                IF ( Q(IX,IY) .EQ. 0 ) THEN
                   X(INDEX) = DBLE(IY)
                   Y(INDEX) = I(IX,IY)
                   NGOOD = NGOOD + 1
                   IF ( ERRORS ) THEN
                      IF ( ( V(IX,IY) .LE. 0 ) .OR.
     :                     ( V(IX,IY) .EQ. FUNKNOWN_IN ) ) THEN
                         ERRORS = .FALSE.
                      ENDIF
                   ENDIF
                   IF ( ERRORS ) THEN
                      W(INDEX) = 1D0/SQRT(V(IX,IY))
                   ELSE
                      W(INDEX) = 1D0
                   ENDIF
                   INDEX = INDEX + 1
                ENDIF
             ENDDO
          ENDIF

*     Second sky region

          IF ( ( SAYS2.GE.MINSKY ) .AND. ( SAYS2.LE.MAXSKY ) .AND.
     :         ( SAYE2.GE.MAXSKY ) .AND. ( SAYE2.LE.MAXSKY ) ) THEN
             DO IY = SAYS2, SAYE2
                IF ( Q(IX,IY) .EQ. 0 ) THEN
                   X(INDEX) = DBLE(IY)
                   Y(INDEX) = I(IX,IY)
                   NGOOD = NGOOD + 1
                   IF ( ERRORS ) THEN
                      IF ( ( V(IX,IY) .LE. 0 ) .OR.
     :                     ( V(IX,IY) .EQ. FUNKNOWN_IN ) ) THEN
                         ERRORS = .FALSE.
                      ENDIF
                   ENDIF
                   IF ( ERRORS ) THEN
                      W(INDEX) = 1D0/SQRT(V(IX,IY))
                   ELSE
                      W(INDEX) = 1D0
                   ENDIF
                   INDEX = INDEX + 1
                ENDIF
             ENDDO
          ENDIF

*     Third sky region

          IF ( ( SAYS3.GE.MINSKY ) .AND. ( SAYS3.LE.MAXSKY ) .AND.
     :         ( SAYE3.GE.MINSKY ) .AND. ( SAYE3.LE.MAXSKY ) ) THEN
             DO IY = SAYS3, SAYE3
                IF ( Q(IX,IY) .EQ. 0 ) THEN
                   X(INDEX) = DBLE(IY)
                   Y(INDEX) = I(IX,IY)
                   NGOOD = NGOOD + 1
                   IF ( ERRORS ) THEN
                      IF ( ( V(IX,IY) .LE. 0 ) .OR.
     :                     ( V(IX,IY) .EQ. FUNKNOWN_IN ) ) THEN
                         ERRORS = .FALSE.
                      ENDIF
                   ENDIF
                   IF ( ERRORS ) THEN
                      W(INDEX) = 1D0/SQRT(V(IX,IY))
                   ELSE
                      W(INDEX) = 1D0
                   ENDIF
                   INDEX = INDEX + 1
                ENDIF
             ENDDO
          ENDIF

*     Fourth sky region

          IF ( ( SAYS4.GE.MINSKY ) .AND. ( SAYS4.LE.MINSKY ) .AND.
     :         ( SAYE4.GE.MAXSKY ) .AND. ( SAYE4.LE.MAXSKY ) ) THEN
             DO IY = SAYS4, SAYE4
                IF ( Q(IX,IY) .EQ. 0 ) THEN
                   X(INDEX) = DBLE(IY)
                   Y(INDEX) = I(IX,IY)
                   NGOOD = NGOOD + 1
                   IF ( ERRORS ) THEN
                      IF ( ( V(IX,IY) .LE. 0 ) .OR.
     :                     ( V(IX,IY) .EQ. FUNKNOWN_IN ) ) THEN
                         ERRORS = .FALSE.
                      ENDIF
                   ENDIF
                   IF ( ERRORS ) THEN
                      W(INDEX) = 1D0/SQRT(V(IX,IY))
                   ELSE
                      W(INDEX) = 1D0
                   ENDIF
                   INDEX = INDEX + 1
                ENDIF
             ENDDO
          ENDIF

*     Remove REJECT worst points

          DO NREJ = 1, MIN( REJECT,NGOOD )

*        by calculating the mean

              MEAN = 0D0
              DO INDEX = 1, NGOOD
                  MEAN = MEAN + Y(INDEX)
              ENDDO
              MEAN = MEAN / NGOOD

*        then calculating the deviation from mean and delete the worst point

              RMAX = -1D0
              DO INDEX = 1, NGOOD
                 R = ABS( Y(INDEX) - MEAN )
                 IF ( R .GT. RMAX ) THEN
                     RMAX = R
                     IMAX = INDEX
                 ENDIF
              ENDDO
              DO INDEX = IMAX, NGOOD-1
                 Y(INDEX) = Y(INDEX+1)
                 W(INDEX) = W(INDEX+1)
                 X(INDEX) = X(INDEX+1)
              ENDDO
              NGOOD = NGOOD - 1

          ENDDO

*     Check that there are enough good points to do a polynomial fit
*     of the required degree - if not set quality to bad for that column

          IF ( NGOOD .LT. KPLUS1 ) THEN

             CALL PAR_WRUSER('FIG_POLYFIT: '/
     :         /'Insufficient good points to fit to column '/
     :         /ICH_CI(IX), IGNORE )
             DO IY = MINSKY, MAXSKY
                Q(IX,IY) = 0
             ENDDO

          ELSE

*     Do the polynomial fit
             EPS=0D0
             IFAIL2=0
             CALL PDA_DPOLFT(NGOOD,X,Y,W,KPLUS1-1,NDEG,EPS,W1,IFAIL,W1(NGOOD+1),IFAIL2)
             IF (NDEG.NE.KPLUS1-1.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) THEN
                CALL PAR_WRUSER('FIG_POLYFIT: Error in PDA_DPOLFT (Ndeg = '//ICH_CI(NDEG)/
     :             /', Ifail = '//ICH_CI(IFAIL)//', Ifail2 = '//ICH_CI(IFAIL2)//')',IGNORE)
                GO TO 500
             ENDIF

*     Subtract sky values

             DO IY = MINSKY, MAXSKY
                IF ( Q(IX,IY) .EQ. 0 ) THEN
                   XCAP = ( 2D0*DBLE(IY) - DX2 ) / DX
                   IFAIL2=0
                   CALL PDA_DP1VLU(KPLUS1-1,0,DBLE(IY),VALUE,DUMMY,W1(NGOOD+1),IFAIL2)
                   IF (IFAIL2 .EQ. 0) THEN
                      I(IX,IY) = I(IX,IY) - VALUE
                   ELSE
                      CALL PAR_WRUSER('FIG_POLYFIT: Error in PDA_DP1VLU, '/
     :                  /'Ifail2 = '//ICH_CI(IFAIL),IGNORE)
                   ENDIF
                ENDIF
             ENDDO
          ENDIF

*     Catch-all

 500      CONTINUE

      ENDDO

      END
