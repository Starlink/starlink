C+
C                       P A R _ R D A R Y
C
C  Routine name:
C     PAR_RDARY
C
C  Function:
C     Obtains the value of a Figaro numeric array parameter.
C
C  Description:
C     An application subroutine can call RDARY to obtain the values
C     of a numeric parameter array. RDARY will always return some valid
C     set of values - ie all within VMIN and VMAX, and in agreement with
C     ORDER.
C
C  Language: FORTRAN
C
C  Call:
C     CALL PAR_RDARY(NAME,VMIN,VMAX,ORDER,UNITS,NV,NVMAX,VALS)
C
C  Parameters:     (">" input, "!" modified, "<" output)
C     (>) NAME     (Fixed string, descr) The name of the parameter
C                  array. Should be terminated either by a blank or by
C                  the end of the string.  Case is not significant.
C     (>) VMIN     (Real, ref) The minimum valid value for the
C                  parameters.
C     (>) VMAX     (Real, ref) The maximum valid value for the
C                  parameters.
C     (>) ORDER    (Fixed string, descr) Indicates whether the parameter
C                  values should be in any ascending or descending
C                  order. Only the first character is checked; currently
C                  this can be 'A' for Ascending (ie
C                  VALS(I).LE.VALS(I+1)), or 'D' for Descending (ie
C                  VALS(I).GE.VALS(I+1)).  Any other characters are
C                  treated as 'N' for None (no order).
C                  Case is not significant.
C                  'I' or 'i' is taken as Increasing.
C     (>) UNITS    (Fixes string, descr) The units for the parameters.
C                  Ignored.
C     (>) NV       (Integer, ref) The number of values required.
C     (>) NVMAX    (Integer, ref) Maximum number of values that may be
C                  required for this array - this is needed in case the
C                  array has to be created, in which case its maximum
C                  size must be known.
C     (!) VALS     (Real array (NV), ref) Passed as the reset values for
C                  the parameters, returned containing the parameter
C                  values as obtained by RDARY.
C
C  Internal declaration:
C     SUBROUTINE PAR_RDARY (NAME,VMIN,VMAX,ORDER,UNITS,NV,NVMAX,VALS)
C     INTEGER NV,NVMAX
C     REAL VMIN,VMAX,VALS(NV)
C     CHARACTER*(*) NAME,ORDER,UNITS
C
C  Author: KS: Keith Shortridge (CIT)
C          HME: Horst Meyerdierks (UoE, Starlink)
C
C  History:
C     16th Feb 1984  KS / AAO. Original version brought from CIT.
C     13th May 1986  KS / AAO.  Now gets prompt string from common,
C                    instead of directly from the parameter file.
C     5th Sept 1988  KS / AAO. Support for parameter abort added.
C     29th Aug 1989  KS / AAO. Support to range syntax "n1..n2" added.
C                    Workspace increased to allow 50 values.  Initial
C                    prompt now displays all values.
C     5th Sept 1989  KS / AAO. Fixed bug (introduced with last change?)
C                    that made '\' on command line give reset values and
C                    not defaults.
C     8th Dec  1989  KS / AAO. Comments reformatted.
C     11th Mar 1991  KS / AAO. Added support for repeated input from file.
C     13th Aug 1992  HME: Translate to ADAM PAR call(s).
C     6th  Oct 1992  HME: If values received from parameter system must
C                    be updated, put them back into the parameter system.
C+
      SUBROUTINE PAR_RDARY (NAME,VMIN,VMAX,ORDER,UNITS,NV,NVMAX,VALS)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      INTEGER NV,NVMAX
      REAL VMIN,VMAX,VALS(NV)
      CHARACTER*(*) NAME,ORDER,UNITS
C
      INCLUDE 'PARBLK'
C
C     Local variables
C
      LOGICAL PUTBAK
      INTEGER I
      INTEGER LSTAT
      INTEGER NRET
C
      IF ( ABORT ) RETURN
C
      PUTBAK = .FALSE.
      LSTAT = 0
C
C     Dealings with ADAM parameter system
C
      CALL PAR_DEF1R( NAME, NV, VALS,       LSTAT )
      CALL PAR_GET1R( NAME, NV, VALS, NRET, LSTAT )
      IF ( LSTAT .NE. SAI__OK ) ABORT = .TRUE.
C
C     Bring into range
C
      DO 1 I = 1, NV
         IF ( VALS(I) .GT. VMAX .OR. VALS(I) .LT. VMIN ) THEN
            PUTBAK = .TRUE.
            VALS(I) = MIN( VALS(I), VMAX )
            VALS(I) = MAX( VALS(I), VMIN )
         END IF
 1    CONTINUE
C
C     Bring into order
C
      IF ( ORDER(:1) .EQ. 'A' .OR. ORDER(:1) .EQ. 'a' .OR.
     :     ORDER(:1) .EQ. 'I' .OR. ORDER(:1) .EQ. 'i' ) THEN
         DO 2 I = 1, NV-1
            IF ( VALS(I) .GT. VALS(I+1) ) THEN
               PUTBAK = .TRUE.
               VALS(I+1) = VALS(I)
            END IF
 2       CONTINUE
      ELSE IF ( ORDER(:1) .EQ. 'D' .OR. ORDER(:1) .EQ. 'd' ) THEN
         DO 3 I = 1, NV-1
            IF ( VALS(I) .LT. VALS(I+1) ) THEN
               PUTBAK = .TRUE.
               VALS(I+1) = VALS(I)
            END IF
 3       CONTINUE
      END IF
C
C     If modification was necessary, put the new values back into the
C     parameter system
C
      IF ( PUTBAK ) CALL PAR_PUT1R( NAME, NV, VALS, LSTAT )
C
      END
