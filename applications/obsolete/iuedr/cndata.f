      SUBROUTINE CNDATA
*+
*  Name:
*     SUBROUTINE CNDATA

*  Description:
*     This routine cancels the current Dataset contents.

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          21-SEP-88     IUEDR Vn. 2.0
*     Martin Clayton     06-SEP-94     IUEDR Vn. 3.1-3

*  Method:
*     Propagate cancellation to all lower levels via structured calls.

*-

*  Implicit:
      IMPLICIT NONE

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMDISP'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'
      INCLUDE 'CMFACE'
      INCLUDE 'CMGEOM'
      INCLUDE 'CMITFC'
      INCLUDE 'CMTEM'

      NODATA = .TRUE.
      NOHEAD = .TRUE.
      NODISP = .TRUE.
      NOFIDS = .TRUE.
      NOFIDT = .TRUE.
      NOROT = .TRUE.
      NOFACE = .TRUE.
      NOGEOM = .TRUE.
      NOITFC = .TRUE.
      NOTEM = .TRUE.
      NOIMA = .TRUE.

      CALL CNSPEC
      CALL CNCOMB
      CALL CNSCAN
      CALL CNLBLS

      END
