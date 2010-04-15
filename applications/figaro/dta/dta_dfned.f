C+
      SUBROUTINE DTA_DFNED (NAME,DFINED,STATUS)
C
C     D T A _ D F N E D
C
C     Returns the state (defined/undefined) of a named data object.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the data object.
C                  This should be in the standard form for
C                  object names, ie with dots separating the
C                  component names and followed by dimension
C                  information (which is ignored by this routine)
C                  in square brackets.  Should end with a blank
C                  or with the end of the string, and case is
C                  unimportant.
C     (<) DFINED   (Logical,ref) True if object contents are defined,
C                  false otherwise.
C     (<) STATUS   (Integer) Returns a status code
C                  0 => OK
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVNAM => Object name is invalid
C                  DTA_INVDIM => Object name has invalid
C                                dimension specifications.
C                  DTA_TOODEEP => MXDIMS is too small.
C                  Lower level routines may pass back other
C                  error codes.
C-
C  History:
C     20 Jul 1995  Original version.  hme / UoE, Starlink.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME
      INTEGER STATUS
      LOGICAL DFINED
C
C     Error codes
C
      INCLUDE 'DTACODES'
C
C     Data structure parameters: used are
C
C     DST_MAXLEV   Maximum number of levels in a name
C     DST_MAXDIM   Maximum number of dimensions in a name
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      LOGICAL FOUND,ISPRIM
      INTEGER EMSTAT,LASTC(DST_MAXLEV),DSDIMS(DST_MAXDIM)
      INTEGER LEVELS,NSDIM,LEV
      CHARACTER FNAME*80
      CHARACTER OBJLOC*(DAT__SZLOC),ENVLOC*(DAT__SZLOC)
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Convert NAME to upper case and analyse it
C
      CALL DTA_TRNAME(NAME,FNAME)
      CALL DTA_SPLITN(FNAME,DST_MAXLEV,DST_MAXDIM,LEVELS,LASTC
     :                                         ,NSDIM,DSDIMS,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Look for object in data structures
C
      CALL DTA_LOCATE(FNAME,LEVELS,LASTC,OBJLOC,FOUND,ENVLOC,
     :                                               LEV,STATUS)
      IF (STATUS.NE.0) GO TO 600
      IF (.NOT.FOUND) THEN
         STATUS=DTA_NOTFND
         GO TO 600
      END IF
C
C     Get the state of the object. A non-primitive is always defined.
C
      CALL DAT_PRIM(OBJLOC,ISPRIM,STATUS)
      IF (ISPRIM) THEN
         CALL DAT_STATE(OBJLOC,DFINED,STATUS)
      ELSE
         DFINED=.TRUE.
      END IF
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END

