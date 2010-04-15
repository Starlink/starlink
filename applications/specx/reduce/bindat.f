C-----------------------------------------------------------------------

      SUBROUTINE BINDAT (NQ, BUF, IFAIL)

C  Program to average data in to bins of width NBIN starting at
C  channel NSTART and amend DFCEN and DFINC appropriately

C   History:
C       6-JUN-2000 (AJC):
C         Replace 'Type *' with 'PRINT *'
C         Unused DFCEN, DFINC
C-

            IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      BUF(*)
      INTEGER   IFAIL

*     Common blocks

      INCLUDE 'STACKCOMM'
      INCLUDE 'FLAGCOMM'

*     Local variables

      INTEGER   I, J, K
      INTEGER   JDEF
      INTEGER   NDAT
      INTEGER   NQ1, NQ2
      INTEGER   NPTSNEW(8)
      INTEGER   NST
      INTEGER   MLEFT
      INTEGER   NLEFT
      INTEGER   NBPTS1
      REAL      CEN
      REAL      OLDCEN

*     Functions

      LOGICAL   DOQUAD
      INTEGER   NTOT

*  Ok, go...

      CALL GEN_GETI4 ('Bin width? (channels)', NBIN, 'I3', NBIN, JDEF)

      IF (NBIN.LT.1) THEN
        IFAIL = 16
        PRINT *, '-- Bin data -- Cannot bin with N =', NBIN,' points!'
        RETURN
      END IF

      CALL QLIM      (NQ, NQ1, NQ2)
      CALL INITNPNEW (NPTSNEW)

      DO NQ = NQ1, NQ2
        IF (DOQUAD(NQ))   THEN

          IF (NPTS(NQ).LE.1) THEN
            IFAIL = 18
            PRINT *, '-- Bin data -- Cannot bin 1-channel data'
            RETURN
          END IF

          NST   = NTOT (NQ-1)
          NLEFT = NPTS (NQ)/2

          IF ( MOD (NPTS(NQ), 2) .EQ. 0 ) THEN    ! # of channels even
            NSTART = MOD (NLEFT, NBIN) + 1
            NBPTS1 = 2 * NLEFT/NBIN

          ELSE                                    ! # of channels odd
            IF ( MOD (NBIN,2) .EQ. 0) THEN        !  .and. NBIN even
              NSTART = 1
              NBPTS1 = NPTS(NQ)/NBIN
            ELSE                                  !  .and. NBIN odd
              MLEFT  = (NLEFT - NBIN/2)/NBIN
              NSTART = MOD (NLEFT-NBIN/2, NBIN) + 1
              NBPTS1 = 2*MLEFT + 1
            END IF
          END IF

          DO J = 1, NBPTS1

            NDAT   = 0
            BUF(J) = 0.
            DO K = 1,NBIN
              I = NST + NSTART - 1 + NBIN*(J-1) + K
              IF (DATA(I).NE.BADPIX_VAL) THEN
                BUF(J) = BUF(J) + DATA(I)
                NDAT   = NDAT   + 1
              END IF
            END DO

            IF (NDAT.NE.0) THEN
              DATA(NST+J) = BUF(J)/FLOAT(NDAT)
            ELSE
              DATA(NST+J) = BADPIX_VAL
            END IF

          END DO

          OLDCEN    = 0.5*(NPTS(NQ)+1)
          CEN       = 0.5*(NBPTS1*NBIN+1)+NSTART-1
          ACHAN     = CEN - OLDCEN

          CALL LSRCOR (LSRFLG, VSL, VES, VTE, VLSR,
     &                 IDATE, ITIME, IUTFLG, RA, DEC,
     &                 JFREST(NQ),  JFCEN(NQ),  LOFREQ(NQ),
     &                 IFFREQ(NQ),  ACHAN,      JFINC(NQ))

          JFINC(NQ)   = JFINC(NQ) * NBIN
          NPTSNEW(NQ) = NBPTS1

        END IF
      END DO

      CALL COMPRESSQ (NPTSNEW)

      RETURN
      END


