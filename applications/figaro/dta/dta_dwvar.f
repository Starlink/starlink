C+
      SUBROUTINE DTA_DWVAR (NAME,FILE,BLOCK,STATUS)
C
C     D T A _ D W V A R
C
C     Returns the disk location of a named data object.  This allows
C     direct access to the object using either direct disk I/O calls,
C     or direct mapping of the file.  Note that if either of these
C     is attempted, other access to the same data through DTA should
C     not be attempted at the same time.  If the object is sufficiently
C     small that other objects are held in the same disk blocks as it,
C     this sort of access is very unsafe, and an error code is returned.
C     All non-standard I/O to the object should be closed down before
C     accessing it with reading routines such as DTA_RDVARx.
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
C     (<) FILE     (Character) Returns with the full file name of
C                  the file containing the data object.  Note that
C                  this full file name can be quite long.
C     (<) BLOCK    (Integer) Returns the block number in the file
C                  at which the data starts.  The first block is
C                  number one, not zero.
C     (<) STATUS   (Integer) Returns a status code
C                  0 => OK
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVNAM => Object name is invalid
C                  DTA_INVDIM => Object name has invalid
C                                dimension specifications.
C                  DTA_UNSAFE => Object is not isolated from
C                                other data objects.
C                  Lower level routines may pass back other
C                  error codes.
C-
C     Subroutines / functions called -
C
C     DTA_SPLITN  (DTA_ package) Analyse object name
C     DTA_LOCATE  ( "    "     ) Look for object in structure
C     DTA_HDSERC  ( "    "     ) Convert HDS error code to DTA code
C     DTA_TRNAME  ( "    "     ) Fold name to upper case and tidy up
C     DAT_WHERE   (HDS   "     ) Get disk location of object
C     HDS_TRACE   ( "    "     ) Get filename and path for object
C     EMS_BEGIN   (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL   ( "    "     ) Clear current EMS error status.
C     EMS_END     ( "    "     ) End current reporting environment.
C
C                                     KS / AAO 8th Nov 1988
C     Modified:
C
C     2nd Oct 1989.  Temporary (?) fix.  Now checks that the object's
C                    value is defined, and if not, writes to it to
C                    make sure that it is.  KS/AAO.
C     8th Jan 1992.  KS/AAO.  Syntax of include statements changed to remove
C                    VMS logical names and to use lower case, to enable
C                    compilation on a SUN.  Unused variable NCHAR removed.
C     24th Jan 1992. KS / AAO. Calls to EMS added to control error reporting.
C                    EMS status explicitly annulled before call to HDS,
C                    instead of just setting STATUS to zero.
C     12th Mar 1993. HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME,FILE
      INTEGER BLOCK,STATUS
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
      INTEGER LASTC(DST_MAXLEV),DIMS(DST_MAXDIM)
      INTEGER BYTOFF,EMSTAT,LEVELS,NDIM,LEV
      CHARACTER FNAME*80
      CHARACTER OBJLOC*(DAT__SZLOC),ENVLOC*(DAT__SZLOC)
      CHARACTER PATH*16
C
      LOGICAL DEFINED
      INTEGER DEFSTAT
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
C     Get name of file containing object, and file location for object
C     (We annul the status after HDS_TRACE call to clear any character
C     truncation errors)
C
      CALL HDS_TRACE(OBJLOC,LEVELS,PATH,FILE,STATUS)
      CALL EMS_ANNUL(STATUS)
      CALL DAT_WHERE(OBJLOC,BLOCK,BYTOFF,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
      IF (BYTOFF.NE.0) THEN
         STATUS=DTA_UNSAFE
         GO TO 600
      END IF
C
C     Try to make sure object is defined.  Actually doing a write is
C     rather nasty - once HDS gains a call that will allow an object to
C     have its defined flag set, that should be used instead.  (Whether
C     just doing a DTA_DWVAR should define the object is moot, but
C     doing so is pragmatic.)
C
      DEFSTAT=0
      CALL DAT_STATE(OBJLOC,DEFINED,DEFSTAT)
      IF (.NOT.DEFINED) CALL DTA_WRVARB(FNAME,1,0,DEFSTAT)
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

