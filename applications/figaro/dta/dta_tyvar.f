C+
      SUBROUTINE DTA_TYVAR (NAME,TYPE,STATUS)
C
C     D T A _ T Y V A R
C
C     Returns the type of a named data object.
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
C     (<) TYPE     (Character) Returns with the type of the
C                  data object, in character form.  This will
C                  be in upper case, and will be one of the
C                  primitive data types, or a user-defined
C                  structure name.
C     (<) STATUS   (Integer) Returns a status code
C                  0 => OK
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVNAM => Object name is invalid
C                  DTA_INVDIM => Object name has invalid
C                                dimesnion specifications.
C                  Lower level routines may pass back other
C                  error codes.
C-
C     Subroutines / functions called -
C
C     DTA_SPLITN  (DTA_ package) Analyse object name
C     DTA_LOCATE  ( "    "     ) Look for object in structure
C     DTA_HDSERC  ( "    "     ) Convert HDS error code to DTA code
C     DTA_DTATYP  ( "    "     ) Convert HDS type to DTA type
C     DTA_TRNAME  ( "    "     ) Fold name to upper case and tidy up
C     DAT_TYPE    (HDS   "     ) Get type of component object
C     EMS_BEGIN   (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL   ( "    "     ) Clear current EMS error status.
C     EMS_END     ( "    "     ) End current reporting environment.
C
C                                     KS / CIT  26th Oct 1982
C     Modified:
C
C     13th March 1986.  KS / AAO.  Now uses HDS routines.
C     10th Jan   1992.  KS / AAO.  Syntax of include statements changed to
C                       remove VMS logical names and to use lower case, to
C                       enable compilation on a SUN.
C     24th  Jan  1992.  KS / AAO. Calls to EMS added to control error reporting.
C     12th  Mar  1993.  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME,TYPE
      INTEGER STATUS
C
C     Error codes
C
      INCLUDE 'DTACODES'
C
C     Data structure parameters: used are
C
C     DST_MAXLEV   Maximum number of name components
C     DST_MAXDIM   Maximum number of dimensions
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      LOGICAL FOUND
      INTEGER EMSTAT,LASTC(DST_MAXLEV),DIMS(DST_MAXDIM)
      INTEGER LEVELS,NDIM,LEV,NCHAR
      CHARACTER FNAME*80
      CHARACTER OBJLOC*(DAT__SZLOC),ENVLOC*(DAT__SZLOC)
      CHARACTER HDSTYPE*16
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
     :                                         ,NDIM,DIMS,STATUS)
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
C     Get HDS type for the object, and convert to the corresponding
C     DTA type if such exists.
C
      CALL DAT_TYPE(OBJLOC,HDSTYPE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
      CALL DTA_DTATYP(HDSTYPE,TYPE,NCHAR,STATUS)
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

