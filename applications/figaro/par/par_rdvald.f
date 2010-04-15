C+
C                          P A R _ R D V A L D
C
C  Routine name:
C     PAR_RDVALD
C
C  Function:
C     Returns the value of a Figaro double precision parameter.
C
C  Description:
C     An application subroutine can call PAR_RDVALD to obtain the value
C     of a numeric parameter.  PAR_RDVALD assumes that there has been
C     some command pre-processing performed, probably by PAR_INIT,
C     but in principle this should have been done before the
C     application subroutine was called.  PAR_RDVALD will always return
C     some value, and this value will be valid (ie will be within
C     VMAX and VMIN), except if a previous parameter request was
C     aborted.  If a previous parameter request was aborted,
C     this routine returns immediately.
C     This routine was added in 1989 to allow use of double precision
C     parameters.  In principle, it could be used for all numeric
C     parameters (in the same way that PAR_RDVAL is traditionally used
C     for integer values as well as for real) but PAR_RDVAL will
C     probably continue to be used for most real numeric parameters.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_RDVALD (NAME,VMIN,VMAX,RESET,UNITS,VALUE)
C
C  Parameters:      (">" input, "<" output )
C     (>) NAME      (Fixed string, descr) The name of the parameter.
C                   Should be terminated by a blank or the end of the
C                   string.  Case is not significant.
C     (>) VMIN      (Double, ref) The minimum valid value for the parameter.
C     (>) VMAX      (Double, ref) The maximum valid value for the parameter.
C     (>) RESET     (Double, ref) The reset value for the parameter.
C     (>) UNITS     (Fixed string, descr) Ignored.
C     (<) VALUE     (Double, ref) The value of the parameter as obtained.
C
C  Prior requirements:
C     PAR_INIT must have been called (by the main Figaro routine)
C
C  Internal declaration:
C     SUBROUTINE PAR_RDVALD (NAME,VMIN,VMAX,RESET,UNITS,VALUE)
C     CHARACTER*(*) NAME,UNITS
C     DOUBLE PRECISION VMAX,VMIN,RESET,VALUE
C
C  Author: Keith Shortridge, AAO
C          Horst Meyerdierks, UoE, Starlink
C
C  Original version:  KS / AAO 20th Jan 1989, based on PAR_RDVAL.
C
C  Modified:
C     8th  Dec 1989   KS/AAO. Comments reformatted.
C     11th Mar 1991   KS / AAO.  Added support for repeated values
C                     read from parameter value files.
C     13th Aug 1992   HME: Translate into calls to ADAM parameter system.
C     6th  Oct 1992   HME: If value has to be brought into range, put
C                     the changed value back into the parameter system.
C     12th Jan 1994   HME: Use (A)PAR's max/min.
C      1st Feb 1994   HME: Malcolm Currie's PAR reduces this to a single
C                     call.
C-
      SUBROUTINE PAR_RDVALD (NAME,VMIN,VMAX,RESET,UNITS,VALUE)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME,UNITS
      DOUBLE PRECISION VMAX,VMIN,RESET,VALUE
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
*     CALL PAR_MIND(  NAME, VMIN,  LSTAT )
*     CALL PAR_MAXD(  NAME, VMAX,  LSTAT )
*     CALL PAR_DEF0D( NAME, RESET, LSTAT )
*     CALL PAR_GET0D( NAME, VALUE, LSTAT )
      CALL PAR_GDR0D( NAME, RESET, VMIN, VMAX, .FALSE., VALUE, LSTAT )
      IF ( LSTAT .NE. SAI__OK ) ABORT = .TRUE.
C
C     Bring into range
C
*     IF ( VALUE .LT. VMIN .OR. VALUE .GT. VMAX ) THEN
*        VALUE = MIN( VALUE, VMAX )
*        VALUE = MAX( VALUE, VMIN )
*        CALL PAR_PUT0D( NAME, VALUE, LSTAT )
*     END IF
C
      END
