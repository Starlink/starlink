      LOGICAL FUNCTION PAR_GIVEN (NAME)
C+
C                        P A R _ G I V E N
C
C  Routine name:
C     PAR_GIVEN
C
C  Function:
C     Determines if a Figaro parameter was specified explicitly.
C
C  Description:
C     In some circumstances a program needs to know whether
C     a particular parameter was specified in the command that
C     invoked the program: it may be that a parameter is not
C     normally used, but must be if it was specified directly.
C     PAR_GIVEN allows the program to find out.  This version is
C     for Figaro running in an ADAM environment.
C
C  Language:
C     FORTRAN
C
C  Call:
C     GIVEN=PAR_GIVEN(NAME)
C
C  Parameters:      (">" input, "<" output)
C     (>) NAME      (Fixed string, descr) The name of the parameter.
C                   Case insignificant, must end either with spaces
C                   or with the end of the string.
C  Returns:
C     (<) GIVEN     (Logical, function value) True if the parameter was
C                   specified explicitly in the invoking command, false
C                   if not.
C
C  External subroutines / functions used -
C     SUBPAR_FINDPAR (ADAM routine) Get index for parameter
C     SUBPAR_GETLOC  (  "     "   ) Get state of parameter
C     ERR_ANNUL
C     ERR_MARK
C     ERR_RLSE
C
C  External variables used:  None.
C
C  Prior requirements:  None.
C
C  Author: Keith Shortridge, CIT
C
C  Date: 18th April 1984
C
C  Internal declaration:
C     LOGICAL FUNCTION PAR_GIVEN(NAME)
C     CHARACTER*(*) NAME
C-
C  Modifications:
C     18th April 1984  KS/CIT Original version.
C     9th   Dec  1986  KS/AAO Modified for use with ADAM.
C     17th  Aug  1992  HME/UoE Annul any status from ADAM PAR.
C     30th  Oct  1992  HME / EIA, Starlink.  Use DAT__SZLOC, initialise
C                      return value also for abort case.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME
C
C     ADAM system parameter values
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
      INCLUDE 'PARBLK'
C
C     Local variables
C
      LOGICAL GIVEN
      INTEGER NUMBER, STATUS
      CHARACTER*(DAT__SZLOC) LOC
C
      PAR_GIVEN=.FALSE.
      IF ( ABORT ) RETURN
C
      CALL ERR_MARK
C
      STATUS=SAI__OK
      CALL SUBPAR_FINDPAR (NAME,NUMBER,STATUS)
      CALL SUBPAR_GETLOC (NUMBER,GIVEN,LOC,STATUS)
      IF (STATUS.EQ.SAI__OK) THEN
         PAR_GIVEN=GIVEN
      ELSE
         PAR_GIVEN=.FALSE.
      END IF
C
      CALL ERR_ANNUL(STATUS)
      CALL ERR_RLSE
C
      END
