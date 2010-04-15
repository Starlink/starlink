C+
C                        P A R _ Q N U M
C
C  Routine name:
C     PAR_QNUM
C
C  Function:
C     Prompts a Figaro user for a numeric value.
C
C  Description:
C     Prompts the user for a numeric value. This routine uses the ADAM
C     parameters "NUMERIC_VALUE" or "NO_DEF_VALUE". Be sure that these
C     are registered as of type _REAL in the application interface.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     STATUS=PAR_QNUM(PROMPT,VMIN,VMAX,DEFAULT,USEDEF,UNITS,VALUE)
C
C  Parameters:      (">" input, "<" output)
C     (>) PROMPT    (Fixed string, descr) The prompt string for the
C                   number. This should be 'clean' - it should not have
C                   clever control characters, nor should it have
C                   the default value encoded in it.
C     (>) VMIN      (Real, ref) The minimum acceptable value.
C     (>) VMAX      (Real, ref) The maximum acceptable value.
C     (>) DEFAULT   (Real, ref) The default value - ignored if USEDEF
C                   is false.
C     (>) USEDEF    (Logical, ref) True if there is a default value -
C                   ie if a null response is to indicate that the
C                   value of DEFAULT is to be used.
C     (>) UNITS     (Fixed string, descr) The units of the value.
C                   Ignored.
C     (<) VALUE     (Real, ref) The value obtained.  Will lie between
C                   VMIN and VMAX.
C
C  Returns:
C     (<) STATUS    (Logical, function value) True if there actually was
C                   a number entered, false for a null response.  Note
C                   that a null response is always accepted.
C                   Also false if [VMIN,VMAX] was exceeded.
C
C  Author: KS: Keith Shortridge (CIT)
C          HME: Horst Meyerdierks (UoE, Starlink)
C
C  History:
C     17th Jan 1983   KS / CIT.  Original version.
C     30th Jan 1985   KS / AAO.  Response outside range VMIN..VMAX now
C                     causes reprompting.
C     31st May 1989   KS / CIT.  Bug fix found by SNS/CIT implemented
C                     for when DEFAULT and VALUE are the same.
C     8th  Dec 1989   KS / AAO.  Comments reformatted.
C     14th Aug 1992   HME: Translate to ADAM PAR call(s).
C     17th Aug 1992   HME: Don't check ABORT on entry and don't set it
C                     when ADAM PAR returns status.
C
C  Internal declaration:
C     LOGICAL FUNCTION PAR_QNUM(PROMPT,VMIN,VMAX,DEFAULT,USEDEF,
C                                                    UNITS,VALUE)
C     LOGICAL USEDEF
C     REAL VMIN,VMAX,DEFAULT,VALUE
C     CHARACTER*(*) PROMPT,UNITS
C
C-
      LOGICAL FUNCTION PAR_QNUM(PROMPT,VMIN,VMAX,DEFAULT,USEDEF,
     :                                               UNITS,VALUE)
C
      IMPLICIT NONE
C
      INCLUDE 'SAE_PAR'          ! SAE constants
      INCLUDE 'PAR_ERR'          ! (A)PAR error codes
C
C     Parameters
C
      LOGICAL USEDEF
      REAL VMIN,VMAX,DEFAULT,VALUE
      CHARACTER*(*) PROMPT,UNITS
C
C     Local variables
C
      INTEGER LSTAT
      CHARACTER * ( 16 ) PARNAM
C
      LSTAT = 0
      CALL ERR_MARK
C
      PAR_QNUM = .TRUE.
C
C     Dealings with ADAM parameter system
C
      PARNAM = 'NO_DEF_VALUE'
      IF ( USEDEF ) PARNAM = 'NUMERIC_VALUE'
      IF ( USEDEF ) CALL PAR_DEF0R( PARNAM, DEFAULT, LSTAT )
      CALL PAR_CANCL( PARNAM, LSTAT )
      CALL PAR_PROMT( PARNAM, PROMPT,  LSTAT )
      CALL PAR_GET0R( PARNAM, VALUE,   LSTAT )
C
C     Null reply accepted, but .FALSE. returned
C
      IF ( LSTAT .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( LSTAT )
         VALUE = DEFAULT
         PAR_QNUM = .FALSE.
      END IF
C
C     Value is brought into range, .FALSE. returned if VALUE must be
C     changed.
C     The changed value is not stuffed back into the ADAM parameter
C     system, because this parameter is considered transient.
C
      IF ( VALUE .GT. VMAX ) THEN
         VALUE = VMAX
         PAR_QNUM = .FALSE.
      ELSE IF ( VALUE .LT. VMIN ) THEN
         VALUE = VMIN
         PAR_QNUM = .FALSE.
      END IF
C
      CALL ERR_RLSE
C
      END
