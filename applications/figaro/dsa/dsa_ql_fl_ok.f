C+
C             D S A _ Q U A L I T Y _ A N D _ F L A G S _ O K
C
C  Routine name:
C     DSA_QUALITY_AND_FLAGS_OK
C
C  Function:
C     Indicates that a program can handle both types of quality information.
C
C  Description:
C     The original versions of DSA did not allow a file to have both
C     data quality and flagged data values. Later versions removed this
C     restriction. This means that it is now legitimate for a program to
C     call both DSA_USE_QUALITY and DSA_USE_FLAGGED_VALUES and so handle
C     both sets of quality information explicitly. Doing so makes for an
C     efficient but rather complicated program and it is probably better
C     for a program to handle quality information internally using one
C     method or the other but not both. If a program really wants to handle
C     both types of information internally it should call this routine
C     first in order to prevent the DSA system putting out a warning.
C     (DSA puts out the warning in order to catch any older programs that
C     had been coded to call DSA_USE_QUALITY if a file contains quality
C     information and to call DSA_USE_FLAGGED_VALUES if the file contained
C     flagged values, under the assumption that only one of these would in
C     fact be called. If presented with a file containing both types of
C     quality information, such a program may call both routines but - having
C     assumed only one would be called - may then not process the data
C     properly.)
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_QUALITY_AND_FLAGS_OK
C
C  Parameters: None.
C
C  External subroutines / functions used: None.
C
C  Prior requirements:
C     DSA_OPEN should have been called.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 17th February 1995
C-
C  Common variable details:
C     (<) QF_BOTH_OK    (Logical) Indicates that the program can handle files
C                       woith both quality arrays and flagged data values.
C
C  History:
C     17th Feb 1995  Original version.  KS / AAO.
C+
      SUBROUTINE DSA_QUALITY_AND_FLAGS_OK
C
      IMPLICIT NONE
C
C     DSA_ common definition
C
      INCLUDE 'DSA_COMMON'
C
C     Set the common flag used to suppress the warning message.
C
      QF_BOTH_OK = .TRUE.
C
      END
