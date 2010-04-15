
      SUBROUTINE PERIOD_PHASEBIN(XDATA, YDATA, YERR, NDATA,
     :                           XBIN, YBIN, EBIN, NBIN,
     :                           YARRAY, MXCOL)

C===========================================================================
C Bins data on a given period for PHASE.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_PHASEBIN declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, NBIN, MXCOL, BINCOUNT, I, J
      DOUBLE PRECISION BINWID, CENBIN, MINBIN, MAXBIN
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA), YERR(NDATA)
      DOUBLE PRECISION XBIN(NBIN), YBIN(NBIN), EBIN(NBIN)
      DOUBLE PRECISION YARRAY(NBIN,MXCOL)


C-----------------------------------------------------------------------------
C Bin data in phase bins (if NBIN = 0, leave data unchanged).
C-----------------------------------------------------------------------------

      BINWID = 1.0D0/DFLOAT(NBIN)

      DO 20 I = 1, NBIN
         CENBIN = DFLOAT(I-1)*BINWID
         MINBIN = CENBIN - (BINWID/2.0D0)
         MAXBIN = CENBIN + (BINWID/2.0D0)
         XBIN(I) = CENBIN
         YBIN(I) = 0.0D0
         EBIN(I) = 0.0D0

         BINCOUNT = 0
         DO 10 J = 1, NDATA
            IF ( I.EQ.1 ) THEN
               IF ( XDATA(J).LT.MAXBIN ) THEN
                  YBIN(I) = YBIN(I) + YDATA(J)
                  EBIN(I) = EBIN(I) + YERR(J)
                  BINCOUNT = BINCOUNT + 1
               END IF
               IF ( XDATA(J).GE.MINBIN+1.0D0 ) THEN
                  YBIN(I) = YBIN(I) + YDATA(J)
                  EBIN(I) = EBIN(I) + YERR(J)
                  BINCOUNT = BINCOUNT + 1
               END IF
            ELSE IF ( XDATA(J).GE.MINBIN ) THEN
               IF ( XDATA(J).LT.MAXBIN ) THEN
                  YBIN(I) = YBIN(I) + YDATA(J)
                  EBIN(I) = EBIN(I) + YERR(J)
                  BINCOUNT = BINCOUNT + 1
               END IF
            END IF
  10     CONTINUE

         IF ( BINCOUNT.EQ.0 ) THEN
            YBIN(I) = 0.0D0
            EBIN(I) = 0.0D0
         ELSE
            YBIN(I) = YBIN(I)/DFLOAT(BINCOUNT)
            EBIN(I) = (EBIN(I)/DFLOAT(BINCOUNT))
     :                /DSQRT(DFLOAT(BINCOUNT))
         END IF

  20  CONTINUE

      DO 30 I = 1, NBIN
         YARRAY(I, 1) = XBIN(I)
         YARRAY(I, 2) = YBIN(I)
         YARRAY(I, 3) = EBIN(I)
  30  CONTINUE

      RETURN
      END
