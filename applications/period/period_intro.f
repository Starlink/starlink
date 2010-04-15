
      SUBROUTINE PERIOD_INTRO

C=============================================================================
C Routine to show introductory message for PERIOD.
C
C Written by Vikram Singh Dhillon @Sussex 13-June-1992.
C
C
C GJP October 1995
C
C Modified at request of Vik Dhillon
C
C GJP March 1997
C
C New heading for Linux port.
C
C KPD August 2001
C
C Double Precision code implementation.
C
C KPD October 2001
C
C Implementation of Dynamic Memory Allocation.
C
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PERIOD_INTRO declarations.
C-----------------------------------------------------------------------------

      WRITE (*, *) ' '
      WRITE (*, *)
     :            '|**************************************************|'
      WRITE (*, *)
     :            '|**| PERIOD  :>  A time-series analysis package |**|'
      WRITE (*, *)
     :            '|**| Version :>  5.0-2 for UNIX                 |**|'
      WRITE (*, *)
     :            '|**| Date    :>  18-JUN-2003                    |**|'
      WRITE (*, *)
     :            '|**************************************************|'
      WRITE (*, *)

      RETURN
      END
