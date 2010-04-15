
      SUBROUTINE PERIOD_PDM(XDATA, YDATA, NDATA, NBIN, WBIN, VARI, PDM,
     :                      MAXPTS, IFAIL)

C===========================================================================
C This routine calculates the PDM statistic of XDATA(NDATA), YDATA(NDATA),
C following the method described by Stellingwerf (1978, APJ, 224, 953).
C Basically, the data is folded on a trial frequency and split up into
C NBIN samples, each of width WBIN. The PDM statistic is then calculated
C by estimating the mean and variance of each sample, calculating the
C overall variance for all of the samples and then dividing this by the
C variance of the whole dataset. If there is an error IFAIL = 1, otherwise
C IFAIL = 0.
C
C Written by K P Duffey @RAL, October 2001, to make use of routine
C  PERIOD_PERFORMPDM (formerly PERIOD_PDM, written by
C                      Vikram Singh Dhillon @LPO 3-March-1993.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE 'CNF_PAR'

C-----------------------------------------------------------------------------
C PERIOD_PDM declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, NBIN, IFAIL, MAXPTS
      INTEGER NPTSPTR
      INTEGER SAMPLEPTR, SVARPTR
      DOUBLE PRECISION XDATA(NDATA), YDATA(NDATA)
      DOUBLE PRECISION WBIN, VARI, PDM

      CALL PERIOD_ALLOC('_INTEGER', NBIN, NPTSPTR)
      CALL PERIOD_ALLOC('_DOUBLE', NDATA*3, SAMPLEPTR)
      CALL PERIOD_ALLOC('_DOUBLE', NBIN, SVARPTR)

C-----------------------------------------------------------------------------
Calculate PDM statistics
C-----------------------------------------------------------------------------

      CALL PERIOD_PERFORMPDM(XDATA, YDATA, NDATA, NBIN, WBIN,
     :                       VARI, PDM, MAXPTS, IFAIL,
     :                       %VAL(CNF_PVAL(NPTSPTR)),
     :                       %VAL(CNF_PVAL(SAMPLEPTR)),
     :                       %VAL(CNF_PVAL(SVARPTR)))


      CALL PERIOD_DEALL(SVARPTR)
      CALL PERIOD_DEALL(SAMPLEPTR)
      CALL PERIOD_DEALL(NPTSPTR)

      RETURN
      END
