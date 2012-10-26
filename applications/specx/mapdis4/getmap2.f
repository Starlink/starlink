*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused ISTAT, II, STRING
C-----------------------------------------------------------------------

      SUBROUTINE GETMAP2 (BUF1, BUF2, INTERP, VMAP, IFAIL)

      IMPLICIT NONE

C  Works by binning across co-ordinate (1)
C  from 3-d file to a 2-d array.

*     Include files

      INCLUDE 'CUBE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'PLOT2D'
      INCLUDE 'CNF_PAR'

*     Formal parameters:

      REAL      BUF1(*)
      REAL      BUF2(*)
      LOGICAL   INTERP
      REAL      VMAP(*)
      INTEGER   IFAIL

*     Common blocks

      LOGICAL*4 INVERT_AXIS
      COMMON /GOOD_PT/ INVERT_AXIS(3)

*     Local variables

      LOGICAL   GOT_DATA
      INTEGER   IFAC(3)
      INTEGER   IX, IY, IZ
      INTEGER   I,  J,  K,  L
      INTEGER   JJ, KK, LL
      INTEGER   IDEP
      INTEGER   NZ
      INTEGER   NOTJ
      REAL      AV
      REAL      FX, FY, FZ
      REAL      FAC

*  Ok, go...

      IFAIL = 0

      IX = LINK(1)
      IY = LINK(2)
      IZ = LINK(3)

*  Check that LINK array is set properly

      IF (IX+IY+IZ .NE. 6) THEN
        IFAIL = 63
        RETURN
      END IF

CD     PRINT *, ' -- getmap2 --'
CD     PRINT *, '    NAX: ',NAX
CD     PRINT *, '    LINK:',LINK
CD     PRINT *, '    IOFF:',IOFF
CD     PRINT *, '    PFAC:',PFAC
CD     PRINT *, '    PF1: ',PF1
CD     PRINT *, '    PF2: ',PF2
CD     PRINT *, '    Invert_axis:',INVERT_AXIS

C  Then write data to 2-D array, transposing if necessary. First
C  work out factors for indexing to output array (VMAP)

C  Data from third (z) co-ordinate on OUTPUT map is averaged/integrated into
C  a single cell on the map. So the indexing factor for this co-ordinate is
C  zero. I.e., the data point loses its memory of this co-ordinate.
C  NJ is the ORIGINAL index of the quantity binned into z on the output map.

      DO J = 1,3
        IF (IZ.EQ.J)   THEN
          NZ = J
          IFAC(NZ) = 0
        END IF
      END DO

C  Now we need to work out the indexing factors for the other two co-ordinates.
C  One of these must be equal to 1 and the other to the X-axis (row) size of
C  the OUTPUT map array.
C  Go through this loop twice - once for each input index NOT corresponding to
C  the z-axis of the output map, for which the indexing factor is already
C  assigned.

      DO J = 1,3
        IF (J.NE.NZ)   THEN

C         The sum of 1,2 and 3 is 6, so the index which is not equal to J or NZ
C         is given as NOTJ

          NOTJ = 6-NZ-J

          IF (J.EQ.IX)   THEN
            IFAC (J)    = 1
            IFAC (NOTJ) = NAX(IX)
          END IF

        END IF
      END DO

CD    PRINT *, '    IX, IY, IZ = ', IX, IY, IZ
CD    IF (.NOT. INTERP) THEN
CD      PRINT *, '    Getting raw data from cube'
CD      PRINT *, '    INDEX @ address: ', INDEX_PTR
CD    END IF

C  Now fetch data (using the windows) and rewrite to VMAP using indexing
C  worked out above. This also allows for binning across dimension (3).

      IF (     ABS(IFAC(1)).GT.100000 .OR. ABS(IFAC(2)).GT.100000
     &    .OR. ABS(IFAC(3)).GT.100000) PRINT *, 'IFAC undefined!'

C  Need to zero data before use, as we will sum into the output array.

      CALL INIT_ARRAY (NAX(IX)*NAX(IY), VMAP, 0.0)

C  Get the data sequentially from the file and deposit into the 2-D array.
C  This arrangement is probably as efficient as any given that the map array
C  for most mm-wave applications will be tiny, so no paging is involved for
C  this, while it very well may be for the bigger file.
C
C  In this next 'paragraph' all indices refer to the ORIGINAL co-ordinate
C  system, i.e.
C         1 .... R.A.
C         2 .... Dec.
C         3 .... Vel.

      FX = 1.0
      FY = 1.0
      FZ = 1.0

      DO K = 1,NAX(2)     ! Do over the map Y-axis (Declination)
        KK = K
        IF (INVERT_AXIS(2)) KK = NAX(2)+1-K

        IF (IZ.EQ.2) THEN
          FY = 1.0
          IF (KK.EQ.1)      FY = FY - (1.-PF1(2))
          IF (KK.EQ.NAX(2)) FY = FY - (1.-PF2(2))
        END IF

        DO J = 1,NAX(1)     ! Do over  the map X-axis (R.A.)
          JJ = J
          IF (INVERT_AXIS(1)) JJ = NAX(1)+1-J

          IF (IZ.EQ.1) THEN
            FX = 1.0
            IF (JJ.EQ.1)      FX = FX - (1.-PF1(1))
            IF (JJ.EQ.NAX(1)) FX = FX - (1.-PF2(1))
          END IF

C         Get the relevant spectrum from the cube

          CALL GET_CUBE_DATA (J+IOFF(1), K+IOFF(2), NPTS1, BUF2,
     &                        INTERP, 1+IOFF(3), NAX(3)+IOFF(3), BUF1,
     &                        %VAL(CNF_PVAL(INDEX_PTR)), 
     :                        MSTEP, NSTEP, GOT_DATA)

C         If we got real data. add them into the map, else mark
C         affected map points as bad.

          DO L = 1,NAX(3)

            LL = L
            IF(INVERT_AXIS(3)) LL = NAX(3)+1-L

            IDEP = (JJ-1)*IFAC(1) + (KK-1)*IFAC(2)
     &              + (LL-1)*IFAC(3) + 1

            IF (IZ.EQ.3) THEN
              FZ = 1.0
              IF (LL.EQ.1)      FZ = FZ - (1.-PF1(3))
              IF (LL.EQ.NAX(3)) FZ = FZ - (1.-PF2(3))
            END IF

            IF (GOT_DATA .AND. VMAP(IDEP).NE.BADPIX_VAL) THEN
              VMAP(IDEP) = VMAP(IDEP) + ABS(FX*FY*FZ)*BUF1(L)
            ELSE
              VMAP(IDEP) = BADPIX_VAL
            END IF

          END DO
        END DO
      END DO

C Average if indicated by ISUM: If integrating multiply by width of
C pixel in 3rd axis; if averaging divide by number of pixels added together.

      IF (ISUM)   THEN             ! Integrating
        AV  = 1.
        FAC = PFAC(IZ)
      ELSE                         ! Averaging
        DO J = 1,3
          IF (IZ.EQ.J)   AV = NAX(J) + PF1(J) + PF2(J) - 2
        END DO
        FAC = 1.0
      END IF

CD    PRINT *, '    IZ:  ',IZ
CD    PRINT *, '    NAX: ',NAX(IZ)
CD    PRINT *, '    PF1: ',PF1(IZ),' PF2: ',PF2(IZ)
CD    PRINT *, '    AV:  ',AV
CD    PRINT *, '    Fac: ',FAC

C     (also at this step work out if pixels of map are OK - mark bad otherwise)

      DO K = 1,NAX(IY)
        DO J = 1,NAX(IX)
          I = J+(K-1)*NAX(IX)
          IF (VMAP(I).NE.BADPIX_VAL) THEN
            VMAP(I) = VMAP(I)*ABS(FAC/AV)
          END IF
        END DO
      END DO

  998 CONTINUE

      RETURN
      END


