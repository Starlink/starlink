C+
      SUBROUTINE DTA_SZVAR (NAME,MXDIM,NDIM,DIMS,STATUS)
C
C     D T A _ S Z V A R
C
C     Returns the dimensions of a named data object.
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
C     (>) MXDIM    (Integer) The maximum number of dimensions
C                  to return - ie the dimension of DIMS.
C     (<) NDIM     (Integer) The number of dimensions of the
C                  object.
C     (<) DIMS     (Integer DIMS(MXDIM)) Array in which the
C                  dimensions of the named object are returned.
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
C     Subroutines / functions called -
C
C     DTA_SPLITN  (DTA_ package) Analyse object name
C     DTA_LOCATE  ( "    "     ) Look for object in structure
C     DTA_HDSERC  ( "    "     ) Convert an HDS error code to a DTA code
C     DTA_DTATYP  ( "    "     ) Get a DTA type, given an HDS type
C     DTA_TRNAME  ( "    "     ) Fold name to upper case and tidy up
C     DAT_SHAPE   (HDS   "     ) Get shape of a component object
C     DAT_TYPE    ( "    "     ) Get type of a component object
C     EMS_BEGIN   (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL   ( "    "     ) Clear current EMS error status.
C     EMS_END     ( "    "     ) End current reporting environment.
C
C                                     KS / CIT  26th Oct 1982
C     Modified:
C
C     13th March 1986.  KS / AAO. Modified to use HDS routines.
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
      CHARACTER*(*) NAME
      INTEGER MXDIM,NDIM,DIMS(MXDIM),STATUS
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
      LOGICAL FOUND
      INTEGER EMSTAT,LASTC(DST_MAXLEV),DSDIMS(DST_MAXDIM)
      INTEGER LEVELS,NSDIM,LEV,I,NCHAR
      CHARACTER FNAME*80
      CHARACTER OBJLOC*(DAT__SZLOC),ENVLOC*(DAT__SZLOC)
      CHARACTER HDSTYPE*16,TYPE*16
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
C     Get the dimension information and object type, just in
C     case it turns out to be a character string.
C
      CALL DAT_SHAPE(OBJLOC,DST_MAXDIM,DSDIMS,NDIM,STATUS)
      CALL DAT_TYPE(OBJLOC,HDSTYPE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
      CALL DTA_DTATYP(HDSTYPE,TYPE,NCHAR,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     If the object is a character string, we use the length as the
C     first dimension, and move the other dimensions up accordingly.
C     Otherwise, just copy the dimension information over.
C
      IF ((TYPE.EQ.'CHAR').AND.(NCHAR.GT.0)) THEN
         NDIM=NDIM+1
         IF (NDIM.GT.MXDIM) THEN
            STATUS=DTA_TOODEEP
            GO TO 600
         END IF
         DO I=NDIM,2,-1
            DIMS(I)=DSDIMS(I-1)
         END DO
         DIMS(1)=NCHAR
      ELSE
         IF (NDIM.GT.MXDIM) THEN
            STATUS=DTA_TOODEEP
            GO TO 600
         END IF
         DO I=1,NDIM
            DIMS(I)=DSDIMS(I)
         END DO
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

