*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused JDEF
C-----------------------------------------------------------------------------
C
      SUBROUTINE SET_PLOT_DEVICE (IERR)
C
C subroutine to set up SPECX for a new plot device
C
C Arguments:
C
C      IERR     (integer, returned)  error return -
C                                     0  = success
C                                     21 = unavailable device
C
C sets the following variables passed through by FLAGCOMM
C
C      IDEV     (integer, update)    given old plot device
C                                    returned new plot device
C      TERMINAL (logical, returned)  true if device is a terminal
C
C R. Prestage, JACH, 15th February 1989
C
C based entirely on code in SET-PLOT-DEVICE,
C but implemented to facilitate graphics independence.
C
C "native" MONGO version.

      LOGICAL*1 TT_ERROR
      INTEGER*4 IOLD, INEW, IERR

      INCLUDE   'FLAGCOMM'

      INTEGER*4 JPLOT
      COMMON   /NOKEEP/ JPLOT

      IERR     = 0
      TT_ERROR = .FALSE.

      CALL CLOSE_PLOT (JPLOT, IPSEQ, IDEV)

      IOLD = IDEV
      CALL ASK_PLOT_DEVICE (IOLD, INEW, TERMINAL)
      IDEV = INEW

      PRINT *, 'new plot device has SPECX internal #', inew

      RETURN
      END
