C+
C                          P A R _ R D V A L
C
C  Routine name:
C     PAR_RDVAL
C
C  Function:
C     Returns the value of a Figaro numeric parameter.
C
C  Description:
C     An application subroutine can call RDVAL to obtain the value
C     of a numeric parameter.  RDVAL assumes that there has been
C     some command pre-processing performed, probably by PAR_INIT,
C     but in principle this should have been done before the
C     application subroutine was called.  RDVAL will always return
C     some value, and this value will be valid (ie will be within
C     VMAX and VMIN), except if a previous parameter request was
C     aborted. If a previous parameter request was aborted,
C     this routine returns immediately.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_RDVAL (NAME,VMIN,VMAX,RESET,UNITS,VALUE)
C
C  Parameters:      (">" input, "<" output )
C     (>) NAME      (Fixed string, descr) The name of the parameter.
C                   Should be terminated by a blank or the end of the
C                   string.  Case is not significant.
C     (>) VMIN      (Real, ref) The minimum valid value for the parameter.
C     (>) VMAX      (Real, ref) The maximum valid value for the parameter.
C     (>) RESET     (Real, ref) The reset value for the parameter.
C     (>) UNITS     (Fixed string, descr) Ignored.
C     (<) VALUE     (Real, ref) The value of the parameter as obtained.
C
C  Prior requirements:
C     PAR_INIT must have been called (by the main Figaro routine)
C
C  Internal declaration:
C     SUBROUTINE PAR_RDVAL (NAME,VMIN,VMAX,RESET,UNITS,VALUE)
C     CHARACTER*(*) NAME,UNITS
C     REAL VMAX,VMIN,RESET,VALUE
C
C  Author: Keith Shortridge, CIT, AAO
C          Horst Meyerdierks, UoE, Starlink
C
C  Original version:  KS / CIT  15th Feb 1984
C
C  Modified:
C     30th Jan 1985   KS / AAO.  Values outside the legal range now
C                     cause a new value to be prompted for.
C     13th May 1986   KS / AAO.  Now gets prompt string from common,
C                     instead of directly from the parameter file.
C     11th Jun 1986   KS / AAO.  Changed to recognise the FMIN and
C                     FMAX values set when MAX and MIN are specified
C                     in the command line.
C     14th Jul 1986   KS / AAO.  Length of prompt string increased.
C                     ICH_CF replaces ICH_ENCODE for encoding of default
C                     value - allows automatic setting of precision.
C     5th  Sep 1988   KS / AAO.  Support for parameter abort added.
C     8th  Dec 1989   KS / AAO.  Comments reformatted.
C     4th  Mar 1991   KS / AAO.  Added support for repeated values
C                     read from parameter value files.
C     8th  Mar 1991   KS / AAO.  Fixed bug introduced in way common values
C                     are set.
C     13th Aug 1992   HME: Translate into calls to ADAM parameter system.
C     6th  Oct 1992   HME: If value has to be brought into range, put
C                     the changed value back into the parameter system.
C     12th Jan 1994   HME: Use (A)PAR's max/min.
C      1st Feb 1994   HME: Malcolm Currie's PAR reduces this to a single
C                     call.
C-
      SUBROUTINE PAR_RDVAL (NAME,VMIN,VMAX,RESET,UNITS,VALUE)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME,UNITS
      REAL VMAX,VMIN,RESET,VALUE
C
C     Parameter system common -
C
      INCLUDE 'PARBLK'
C
C     Local variables
C
      INTEGER LSTAT
C
      IF ( ABORT ) RETURN
C
      LSTAT = SAI__OK
C
C     Dealings with ADAM parameter system
C
*     CALL PAR_MINR(  NAME, VMIN,  LSTAT )
*     CALL PAR_MAXR(  NAME, VMAX,  LSTAT )
*     CALL PAR_DEF0R( NAME, RESET, LSTAT )
*     CALL PAR_GET0R( NAME, VALUE, LSTAT )
      CALL PAR_GDR0R( NAME, RESET, VMIN, VMAX, .FALSE., VALUE, LSTAT )
      IF ( LSTAT .NE. SAI__OK ) ABORT = .TRUE.
C
C     Bring into range
C
*     IF ( VALUE .LT. VMIN .OR. VALUE .GT. VMAX ) THEN
*        VALUE = MIN( VALUE, VMAX )
*        VALUE = MAX( VALUE, VMIN )
*        CALL PAR_PUT0R( NAME, VALUE, LSTAT )
*     END IF
C
      END
