C+
      SUBROUTINE DTA_CRVAR (NAME,TYPE,STATUS)
C
C     D T A _ C R V A R
C
C     Creates a named data structure variable, of a given
C     type.  The environment of the variable must already
C     exist; that is, the variable must have a name of the
C     form 'environment.object' (environment may have
C     components as well) and there must already exist a
C     structure with the name 'environment'.  The object
C     itself must not already exist.  Dimensional information
C     is obtained from NAME as well - ie name can be of the
C     form 'environment.obj[dimension list]', where the
C     dimension list is a set of integers separated by commas.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME     (Character) The name of the object to be
C                  created.  The form is as described above,
C                  ie a standard data structure name, and
C                  must be terminated either by a blank or by
C                  the end of the string. NAME can be upper or
C                  lower case, or a mixture.
C     (>) TYPE     (Character) The type of the variable. Can be
C                  any of the allowed types (bit,byte,short,
C                  int,long,float,double,char,file), or
C                  any other string.  If it is not one of the
C                  recognised types, it is treated as a user-
C                  defined structure name.  TYPE is case-
C                  independent, and must end with a blank or
C                  the end of the string.  Note that if TYPE
C                  contains characters (such as '-') that would
C                  be rejected by HDS, the actual type used will
C                  have underscores substituted.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK
C                  DTA_NOENV => The environment does not exist
C                  DTA_EXISTS => The object already exists
C                  Lower level routines may generate other error
C                  codes.
C-
C     Subroutines / functions used -
C
C     DTA_SPLITN   (DTA_ package) Analyses a name
C     DTA_LOCATE   (     "      ) Looks for a name in the structure
C     DTA_HDSTYP   (     "      ) Get HDS type corresponding to DTA type
C     DTA_HDSERC   (     "      ) Convert an HDS error code to a DTA one
C     DTA_TYTIDY   (     "      ) Make sure a type is acceptable to HDS
C     DTA_TRNAME   (     "      ) Fold name to upper case and tidy up
C     DAT_NEW      (HDS_ "      ) Create a new structure component
C     EMS_BEGIN    (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL    ( "    "     ) Clear current EMS error status.
C     EMS_END      ( "    "     ) End current reporting environment.
C
C                                      KS / CIT  18th Nov 1982
C     Modified:
C
C     12th Mar 1986.  KS / AAO.  Re-written to use HDS routines.
C     8th  Jan 1992.  KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN.
C     24th Jan 1992.  KS / AAO. Calls to EMS added to control error reporting.
C     12th Mar 1993.  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME,TYPE
      INTEGER STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Data structure parameters - defines the following
C
C     DST_MAXLEV   Maximum number of components in Name
C     DST_MAXDIM      "      "     " dimensions "   "
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables -
C
      LOGICAL FOUND
      INTEGER EMSTAT,INVOKE,LASTC(DST_MAXLEV),DIMS(DST_MAXDIM),LEV
      INTEGER LEVELS,NDIM,I,NCHAR
      CHARACTER FNAME*80
      CHARACTER ENVLOC*(DAT__SZLOC),OBJLOC*(DAT__SZLOC)
      CHARACTER HDSTYPE*16,FTYPE*16
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Convert name to upper case, and analyse it
C
      CALL DTA_TRNAME(NAME,FNAME)
      CALL DTA_SPLITN(FNAME,DST_MAXLEV,DST_MAXDIM,LEVELS,LASTC,
     :                                         NDIM,DIMS,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Look for the name in the existing structure
C
      CALL DTA_LOCATE(FNAME,LEVELS,LASTC,OBJLOC,FOUND,ENVLOC,
     :                                             LEV,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Check that the object doesn't already exist
C
      IF (FOUND) THEN
         STATUS=DTA_EXIST
         GO TO 600
      END IF
C
C     Check that search ended at the correct level.  If it didn't,
C     then the environment doesn't exist.
C
      IF (LEV.NE.LEVELS) THEN
          STATUS=DTA_NOENV
          GO TO 600
      END IF
C
C     It looks OK, so get the HDS object type corresponding to
C     the DTA type specified.  Note the complication that since
C     HDS supports character strings, it has been decided to treat
C     a DTA array CHARS[n,m] of type CHAR, for example, as an
C     HDS array CHARS[m] of type _CHAR*n.  This seems a more natural
C     thing to do, but it complicates things slightly.
C
      FTYPE=TYPE
      INVOKE=ICH_FOLD(FTYPE)
      IF (FTYPE.EQ.'CHAR') THEN
         IF (NDIM.GT.0) THEN
            NCHAR=DIMS(1)
            DO I=2,NDIM
               DIMS(I-1)=DIMS(I)
            END DO
            NDIM=NDIM-1
         ELSE
            NCHAR=0
         END IF
      END IF
      CALL DTA_HDSTYP(FTYPE,NCHAR,HDSTYPE,STATUS)
      IF (STATUS.NE.0) GO TO 600
      CALL DTA_TYTIDY(HDSTYPE)
C
C     Now create the data object.  Note that at this point we
C     don't bother getting a locator, so we can't put it in the
C     name cache.
C
      CALL DAT_NEW(ENVLOC,FNAME(LASTC(LEVELS-1)+2:LASTC(LEVELS)),
     :                             HDSTYPE,NDIM,DIMS,STATUS)
      IF (STATUS.NE.0) CALL DTA_HDSERC(STATUS)
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

