*  History:
*     22 Nov 1993 (hme):
*        Replace LIB$FREE_VM with PSX_FREE.
*     17 Dec 1993 (hme):
*        In order to adapt to new STACKCOMM/PROTOTYPE, no longer use
*        TSYS as start of scan header, since it isn't any more.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Correct type LOGICAL from INTEGER of IGETVM IFREEVM
*        Default DO_ROTATE to .FALSE.
*        Don't split strings across lines
C-----------------------------------------------------------------------

      SUBROUTINE ROTATE_CUBE (IFAIL)

C   "Driver" routine to rotate the data in the cube in the x-y plane.
C   This routine gets all the virtual memory, etc, and calls another
C   routine to do the matrix manipulations.

      IMPLICIT NONE

C     Formal parameters:

      INTEGER*4 IFAIL            ! SPECX error code

C     Include files:

      INCLUDE 'CNF_PAR'
      INCLUDE 'CUBE'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

C     Functions:

      INTEGER*4 IGETVM
      INTEGER*4 IFREEVM

C     Local variables:

      LOGICAL*4 DO_ROTATE           ! Temporary variable
      INTEGER*4 ISTAT               ! GEN status return
      INTEGER*4 LDATA               ! Total size of the cube (words)
      INTEGER*4 IPTR_BUF            ! Pointer to BUF array
      INTEGER*4 NEW_CUBE_ADDRESS    ! Address of VM for created cube.
      INTEGER*4 NEW_INDEX_ADDRESS   ! Address of VM for created index.

C  Ok, go...

      IFAIL = 0

C     Check that there is a cube loaded

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IFAIL = 68
        RETURN
      END IF

C     Check that map is not both rotated and interpolated

      IF (MAP_INTERPOLATED .and. MAP_ROTATED) THEN
        PRINT *,
     &   ' -- rotate_map -- Map is both rotated and interpolated'
        IFAIL = 72
        RETURN
      END IF

C     ... find out the rotation angle required

      CALL GEN_YESNO ('Rotate coordinate frame in plane of sky?',
     &                 .FALSE., DO_ROTATE, ISTAT)
      IF (DO_ROTATE) THEN
        CALL GEN_GETR4('P.A. of plot Y-axis wrt map cell Y-axis (deg)?',
     &                  THETA, 'F6.1', THETA, ISTAT)
      END IF

C     There is a possibility that the user is trying to undo a rotation
C     that was performed previously by setting theta to zero.

      DO_ROTATE = DO_ROTATE .AND. THETA.NE.0.0

C     Sort out titles for X- and Y-axes

CD    PRINT *, ' -- rotate_cube --'
CD    PRINT *, '    DO_ROTATE = ', DO_ROTATE
CD    PRINT *, '    THETA     = ', THETA
CD    PRINT *, '    POS_ANGLE = ', POS_ANGLE

      CALL SET_MAPTITLE (DO_ROTATE, THETA, POS_ANGLE)

C     If map is already rotated and we wish to derotate it,
C     then release the rotated cube (sets pointers back to
C     original unmodified cube)

      IF (.NOT.DO_ROTATE) THEN
        IF (MAP_ROTATED) CALL RELEASE_NEW_CUBE
        MAP_ROTATED = .FALSE.
        MAP_ANGLE   =  0.0
        RETURN
      END IF

C     Equally check that we asked for a real change (no need to do
C     anything if angle is same as before);

      IF (DO_ROTATE.AND.THETA.EQ.MAP_ANGLE.AND.MAP_ROTATED) THEN
        PRINT *, '-- rotate_map -- Rotated cube still loaded: no change'
        RETURN
      END IF

C     Determine the length of the individual spectra in map (under new style
C     map headers this actually stored, but we don't depend on it yet!)

      CALL PUSH
      CALL EXTRACT_HEADER (SCAN_HEADER)
      NPTS1 = NPTS(1)
      CALL POP

C   Get some virtual memory for the new cube and initialize

      CALL MAKE_CUBE2 (NCUBE, NINDEX, NEW_CUBE_ADDRESS,
     &                 NEW_INDEX_ADDRESS, IFAIL)

      LDATA = NPTS1*MSTEP*NSTEP
      CALL INIT_ARRAY (LDATA, %VAL(CNF_PVAL(NEW_CUBE_ADDRESS)), 0.0)

C  Also get virtual memory for BUF and DATA arrays (each NPTS1 long) and
C  for the updated INDEX array

      ISTAT = IGETVM (8*NPTS1, .TRUE., 'ROTATE_CUBE', IPTR_BUF)
      IF (ISTAT .ne. 0) THEN
        PRINT *,
     &   'Error getting virtual memory for work arrays for ROTATE'
        IFAIL = 51
        GO TO 99
      END IF

C  Rotate map

      CALL ROTATE (THETA, %VAL(CNF_PVAL(IPTR_BUF)),
     &             %VAL(CNF_PVAL(IPTR_BUF)+(4*NPTS1)),
     &             MSTEP, NSTEP, NPTS1,
     &             %VAL(CNF_PVAL(CURRENT_INDEX_ADDRESS)),
     &             %VAL(CNF_PVAL(NEW_INDEX_ADDRESS)),
     &             %VAL(CNF_PVAL(CURRENT_CUBE_ADDRESS)),
     &             %VAL(CNF_PVAL(NEW_CUBE_ADDRESS)))

*     Delete any virtual memory belonging to an *intermediate* processed cube.

      IF (CURRENT_CUBE_ADDRESS .NE. CUBE_ADDRESS) THEN
        ISTAT = IFREEVM (CURRENT_CUBE_ADDRESS)
        ISTAT = IFREEVM (CURRENT_INDEX_ADDRESS)
      END IF

      CURRENT_CUBE_ADDRESS  = NEW_CUBE_ADDRESS
      CURRENT_INDEX_ADDRESS = NEW_INDEX_ADDRESS

      NEW_CUBE_LOADED = .TRUE.
      MAP_ROTATED     = .TRUE.
      MAP_ANGLE       =  THETA

      WRITE (6,
     &  '('' Rotated cube now loaded - Theta = '', F7.2, '' deg.'')')
     &  THETA

   99 CONTINUE

      ISTAT = IFREEVM (IPTR_BUF)
      IF (ISTAT .ne. 0) THEN
        IFAIL = 51
        PRINT *,'Trouble freeing virtual memory for workspace in ROTATE'
      END IF

      RETURN
      END



      SUBROUTINE ROTATE (THETA, BUF, DATA, MSTEP, NSTEP, NPTS,
     &                   INDEX, INNEW, VM1, VM2)

C  Routine to rotate spectral data stored in cube VM1 by angle THETA
C  in R.A./Dec. plane and rewrite to array VM2.
C  The map size in both units in MSTEP*NSTEP by NPTS
C  per spectrum. The array INDEX is updated to reflect the
C  presence or absence of data in the rotated plane.

      INTEGER*4 INDEX(*), INNEW(*)
      REAL*4    BUF(*),   DATA(*)
      REAL*4    VM1(*),   VM2(*)

C  Set up statement function to bin real variable unambiguously

      LOC(X) = IFIX(X+0.5+SIGN(0.5,X))

C  Initialize

      CTH = COS(THETA*0.0174533)
      STH = SIN(THETA*0.0174533)
      XC  = FLOAT(MSTEP+1)*0.5
      YC  = FLOAT(NSTEP+1)*0.5
      CALL INIT_ARRAY (MSTEP*NSTEP, INNEW, -1)

C  Iterate over map points one at a time
C  Strategy is to rotate the indexing frame and identify old spectra
C  at corners of square in which old point (I,J) finds itself.
C  These are averaged according to the distance from (I,J)

      DO I = 1,MSTEP
        DO J = 1,NSTEP

C  Offset to map centre
          DI = FLOAT(I)-XC
          DJ = FLOAT(J)-YC

C  Rotate
          DX1 = CTH*DI+STH*DJ
          DX2 = -STH*DI+CTH*DJ

C  Un-offset back to (1,1)
          DX1 = DX1+XC
          DX2 = DX2+YC

C  Work out bin and weights for averaging
          IX1 = LOC(DX1)-1
          IX2 = LOC(DX2)-1
          P   = DX1-FLOAT(IX1)
          Q   = DX2-FLOAT(IX2)

C  fetch spectra and average
          CALL INIT_ARRAY (NPTS, BUF, 0.0)
          WTOT = 0.0

C  First check that rotated grid point is within map boundary.
          IF ( IX1.LT.1
     &        .OR. IX2.LT.1
     &        .OR. IX1.GE.MSTEP
     &        .OR. IX2.GE.NSTEP) GO TO 100

          DO K = IX1,IX1+1
            P = 1.-P
            DO L = IX2,IX2+1
              Q = 1.-Q
              IN = K + (L-1)*MSTEP
              IF (INDEX(IN).GE.0)  THEN

C               get data and average
                CALL XCOPY (4*NPTS, VM1(1+NPTS*(IN-1)), DATA)
                W = P*Q
                DO N = 1,NPTS
                  BUF(N) = BUF(N) + W*DATA(N)
                END DO
                WTOT = WTOT+W
              END IF
            END DO
          END DO

C  Normalize

          IF (WTOT.NE.0.0)   THEN
            IPOS = I+(J-1)*MSTEP
            INNEW(IPOS) = 1
            DO N = 1,NPTS
              BUF(N) = BUF(N)/WTOT
            END DO

C  output to new data cube

            CALL XCOPY (4*NPTS, BUF, VM2(1+NPTS*(IPOS-1)))
          ENDIF
  100     CONTINUE
        END DO
      END DO

      RETURN
      END
