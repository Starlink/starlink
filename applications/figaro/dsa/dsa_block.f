C+
C                         D S A _ B L O C K
C
C  Routine name:
C     DSA_BLOCK
C
C  Function:
C     Block data routine for the DSA_ package.
C
C  Description:
C     Block data initialisation routine for DSA_ common variables.  The
C     only one in fact set by this routine is the initialisation flag -
C     all the others are then set by DSA_OPEN.
C
C  Language:
C     FORTRAN
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C
C-
C  Common variable details:
C     (<) OPEN_FLAG    (Logical) Inidcates DSA_ system initialised
C
C  History:
C     10th June 1987   Original version.  KS / AAO.
C     21st Aug 1992    Automatic portability modifications
C                      ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992    "INCLUDE" filenames now upper case. KS/AAO
C+
      BLOCK DATA DSA_BLOCK
C
      IMPLICIT NONE
C
C     DSA_ package common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Initialisation
C
      DATA OPEN_FLAG /.FALSE./
C
      END
