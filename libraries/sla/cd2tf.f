      SUBROUTINE sla_CD2TF (NDP, DAYS, SIGN, IHMSF)
*+
*     - - - - - -
*      C D 2 T F
*     - - - - - -
*
*  Convert an interval in days into hours, minutes, seconds
*
*  (single precision)
*
*  Given:
*     NDP       int      number of decimal places of seconds
*     DAYS      real     interval in days
*
*  Returned:
*     SIGN      char     '+' or '-'
*     IHMSF     int(4)   hours, minutes, seconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size of
*         DAYS, the format of REAL floating-point numbers on the target
*         machine, and the risk of overflowing IHMSF(4).  For example,
*         on the VAX, for DAYS up to 1.0, the available floating-point
*         precision corresponds roughly to NDP=3.  This is well below
*         the ultimate limit of NDP=9 set by the capacity of the 32-bit
*         integer IHMSF(4).
*
*     3)  The absolute value of DAYS may exceed 1.0.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where DAYS is very nearly 1.0 and rounds up to 24 hours,
*         by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
*
*  Called:  sla_DD2TF
*
*  P.T.Wallace   Starlink   12 December 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      INTEGER NDP
      REAL DAYS
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)



*  Call double precision version
      CALL sla_DD2TF(NDP,DBLE(DAYS),SIGN,IHMSF)

      END
