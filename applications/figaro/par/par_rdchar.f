C+
C                       P A R _ R D C H A R
C
C  Routine name:
C     PAR_RDCHAR
C
C  Function:
C     Returns the value of a Figaro character parameter.
C
C  Description:
C     An application subroutine can call RDCHAR to obtain the value
C     of a character parameter.  RDCHAR assumes that there has been
C     some command pre-processing performed, probably by PAR_INIT,
C     but in principle this should have been done before the
C     application subroutine was called.  If a previous parameter
C     request was aborted, this routine will return without action.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL PAR_RDCHAR (NAME,RESET,STRING)
C
C  Parameters:      (">" input, "<" output )
C     (>) NAME      (Fixed string, descr) The name of the parameter.
C                   Should be terminated by a blank or the end of the
C                   string.  Case is not significant.  If '/NOCHECK'
C                   is appended to the parameter name, it is ignored but
C                   only the name itself used.
C     (>) RESET     (Fixed string, descr) The reset value for the
C                   parameter.
C     (<) STRING    (Fixed string, descr) The value of the parameter as
C                   obtained by the rules below.
C
C  Prior requirements:
C     PAR_INIT must have been called (probably by the main Figaro
C     routine).
C
C  Internal declaration:
C     SUBROUTINE PAR_RDCHAR (NAME,RESET,STRING)
C     CHARACTER*(*) NAME,STRING,RESET
C
C  Author: Keith Shortridge, CIT, AAO
C          Horst Meyerdierks, UoE, Starlink
C
C  Original version: KS / CIT 16th Feb 1984
C
C  Modified:
C     12th July 1985.  KS / AAO.  WRUSER, RDUSER finally replaced
C                      by PAR_WRUSER, PAR_RDUSER calls.
C     13th May 1986    KS / AAO.  Now gets prompt string from common,
C                      instead of directly from the parameter file.
C     29th July 1987.  KS / AAO.  Test for non-existent input files
C                      removed.  (This is for use with systems using
C                      FGR_INPUT, where this test gets in the way).
C     5th Sept 1988.   KS / AAO.  Support for parameter abort added.
C     20th Jan 1989.   KS / AAO.  Use of second prompt line when current
C                      string is very long added.
C     8th  Dec 1989.   KS / AAO.  Comments reformatted.
C     3rd  Mar 1991.   KS / AAO.  Added support for repeated values
C                      read from parameter value files.  Test for file
C                      existence (which had assumed a '.DST' file
C                      extension and now gets in the way) removed.
C     13th Aug 1992.   HME / UoE, Starlink.  Translate into calls to
C                      ADAM parameter system.
C-
      SUBROUTINE PAR_RDCHAR (NAME,RESET,STRING)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
C
C     Parameters
C
      CHARACTER*(*) NAME,STRING,RESET
C
C     Parameter system common -
C
      INCLUDE 'PARBLK'
C
C     Local variables
C
      INTEGER LSTAT
      INTEGER I
C
      IF ( ABORT ) RETURN
C
      LSTAT = 0
C
C     Separate the parameter name from qualifier
C
      I = INDEX( NAME, '/' ) - 1
      IF ( I .LT. 0 ) I = LEN( NAME )
C
C     Dealings with ADAM parameter system
C
      CALL PAR_DEF0C( NAME(:I), RESET,  LSTAT )
      CALL PAR_GET0C( NAME(:I), STRING, LSTAT )
      IF ( LSTAT .NE. SAI__OK ) ABORT = .TRUE.
C
      END
