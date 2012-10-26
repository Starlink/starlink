*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused I, II, LOCATION, IPOS
*-----------------------------------------------------------------------

      SUBROUTINE GETPARS (XSCALE, BUF,   VMAP1,  VMAP2, VMAP3, VMAP4,
     &                    VMAP5,  VMAP6, INTERP, IFAIL)

*  Routine derived and simplified from GETMAP to produce maps of the
*  major line parameters: viz peak temperature (Tmax) and velocity of peak
*  (Vmax), total integrated intensity (Tint) and line equivalent width DeltaV

      IMPLICIT  NONE

*     Formal parameters:

      REAL      XSCALE(*), BUF(*)
      REAL      VMAP1(*),  VMAP2(*), VMAP3(*)
      REAL      VMAP4(*),  VMAP5(*), VMAP6(*)
      LOGICAL   INTERP
      INTEGER   IFAIL

*     Include files:

      INCLUDE 'CUBE'
      INCLUDE 'MAPS'
      INCLUDE 'MAPHD'
      INCLUDE 'PLOT2D'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'PROTOTYPE'
      INCLUDE 'CNF_PAR'

*     Common blocks:

      LOGICAL   INVERT_AXIS
      COMMON /GOOD_PT/ INVERT_AXIS(3)

*     Functions:

      REAL      XTRANS

*     Local variables

      LOGICAL   GOT_DATA

      INTEGER   J, JJ
      INTEGER   K, KK
      INTEGER   L

      INTEGER   IDEP

      REAL      CMAX
      REAL      TMAX1
      REAL      TINT
      REAL      DELTAV
      REAL      VMAX1
      REAL      CENTROID
      REAL      WIDTH

*  OK, go..

      IFAIL = 0

*     Initialize the maps

      CALL INIT_ARRAY (NAX(1)*NAX(2), VMAP1, BADPIX_VAL)
      CALL INIT_ARRAY (NAX(1)*NAX(2), VMAP2, BADPIX_VAL)
      CALL INIT_ARRAY (NAX(1)*NAX(2), VMAP3, BADPIX_VAL)
      CALL INIT_ARRAY (NAX(1)*NAX(2), VMAP4, BADPIX_VAL)
      CALL INIT_ARRAY (NAX(1)*NAX(2), VMAP5, BADPIX_VAL)
      CALL INIT_ARRAY (NAX(1)*NAX(2), VMAP6, BADPIX_VAL)

*     Set up the XSCALE array for these data

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.ne.0) RETURN

CD    PRINT *,'XSCALE indexing: IOFF, NAX =',IOFF(3),NAX(3)

*     Get the data sequentially from the file and deposit into the 2-D array.

      DO K = 1,NAX(2)     ! Do over the map Y-axis
        KK = K
        IF (INVERT_AXIS(2)) KK = NAX(2)+1-K

        DO J = 1,NAX(1)     ! Do over  the map X-axis
          JJ = J
          IF (INVERT_AXIS(1)) JJ = NAX(1)+1-J

          IDEP = (KK-1)*NAX(1) + JJ

*         Get the relevant spectrum from the cube

          CALL GET_CUBE_DATA (J+IOFF(1), K+IOFF(2), NPTS(1), XSCALE,
     &                        INTERP, IOFF(3)+1, IOFF(3)+NAX(3),
     &                        BUF, %VAL(CNF_PVAL(INDEX_PTR)), 
     :                        MSTEP, NSTEP,
     &                        GOT_DATA)

*         Work out the desired quantities

          IF (GOT_DATA) THEN

*           Peak temperature and channel

            CALL FINDMX (NAX(3), CMAX, BUF, BADPIX_VAL, TMAX1)
            VMAX1 = XTRANS (XSCALE, CMAX+FLOAT(IOFF(3)), NPTS(1), IFAIL)

*           Integrated intensity and moments

            TINT     = 0.0
            CENTROID = 0.0
            WIDTH    = 0.0
            DO L = 1, NAX(3)
              TINT     = TINT     + BUF(L)
              CENTROID = CENTROID + BUF(L)*XSCALE(IOFF(3)+L)
              WIDTH    = WIDTH    + BUF(L)*XSCALE(IOFF(3)+L)**2
            END DO

            CENTROID = CENTROID/TINT
            WIDTH    = WIDTH/TINT - CENTROID**2

*           Multiply integrated intensity by current channel width

            TINT     = TINT * ABS(XFAC(1))

*           Equivalent width

            IF (TMAX1.NE.0.0) THEN
              DELTAV = TINT/TMAX1
            ELSE
              DELTAV = BADPIX_VAL
            END IF

*           ... and put the data into the map

            VMAP1(IDEP) = TMAX1
            VMAP2(IDEP) = VMAX1
            VMAP3(IDEP) = TINT
            VMAP4(IDEP) = DELTAV
            VMAP5(IDEP) = CENTROID
            VMAP6(IDEP) = WIDTH

          ELSE
CD          PRINT *,'Bad pixel: (J,K) = ', J, K
            VMAP1(IDEP) = BADPIX_VAL
            VMAP2(IDEP) = BADPIX_VAL
            VMAP3(IDEP) = BADPIX_VAL
            VMAP4(IDEP) = BADPIX_VAL
            VMAP5(IDEP) = BADPIX_VAL
            VMAP6(IDEP) = BADPIX_VAL
          END IF
        END DO
      END DO

      RETURN
      END

*-----------------------------------------------------------------------
