C+
C                       P A R _ B A T C H
C
C  Routine name:
C     PAR_BATCH
C
C  Function:
C     Enquire the batch mode flag.
C
C  Description:
C     This routine returns the value of the batch mode flag in the
C     parameter system common block. The convention is that the flag
C     is true if the process run in batch mode, the flag is false when
C     the process is interactive.
C
C     The flag value is set by PAR_INIT when the application starts up,
C     it is a simple argument passed to PAR_INIT. It is up to the
C     routine that calls PAR_INIT to work out what the appropriate value
C     should be.
C
C     The Figaro parameter system does not impose a definition of what
C     batch and interactive modes are.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     STATUS=PAR_BATCH()
C
C  Parameters:     (">" input, "<" output)
C     None
C
C  Returns:
C     (<) STATUS   (Logical, function value) Always false.
C
C  Internal description:
C     LOGICAL FUNCTION PAR_BATCH
C
C  Author: Horst Meyerdierks (UoE, Starlink)
C
C  Modifications:
C     12th Aug 1992  HME / UoE, Starlink.  Original version.
C     18th Nov 1992  HME / UoE, Starlink.  Return the batch flag value
C                    rather than always .FALSE.
C-
      LOGICAL FUNCTION PAR_BATCH()
C
      INCLUDE 'PARBLK'
C
      PAR_BATCH=BATCH
C
      END
