C*GRWARN -- issue warning message to user
C+
      SUBROUTINE GRWARN (TEXT)
      CHARACTER*(*) TEXT
C
C Report a warning message on standard output, with prefix "%PGPLOT, ".
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C  8-Nov-1994 [TJP]
C 29-Jul-1999 [BKM]
C   Modify for use with Starlink EMS error system.
C  5-Aug-1999 [BKM]
C   Make error status match GKS based PGPLOT (PGP).
C
C-----------------------------------------------------------------------
      INTEGER  GRPGER, STATUS
      PARAMETER (GRPGER = 233079002) 
C
      IF (TEXT.NE.' ') THEN
          STATUS = GRPGER
          CALL MSG_SETC('MESS', TEXT)
          CALL ERR_REP('GRPGER','%%PGPLOT - ^MESS', STATUS)
*
*   This routine expects that the error message will appear immediately
*   so make this occur!
*
          CALL ERR_FLUSH( STATUS )
      END IF
      END
