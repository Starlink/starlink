C+
      SUBROUTINE DTA_DLVAR (NAME,STATUS)
C
C     D T A _ D L V A R
C
C     Deletes a named data structure object.  The object should
C     not be a top level name, but it can be a structure - in
C     which case all the objects it contains will also be deleted.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NAME        (Character) The name of the object to be
C                     deleted.  This should not contain any
C                     dimensional specifications (DLVAR cannot
C                     delete a single array element).  Case is
C                     ignored.
C     (<) STATUS      (Integer) Returns a status code.
C                     0 => OK
C                     DTA_DELTOP => NAME is a top level name.
C                     DTA_DELELM => NAME is an array element.
C                     DTA_NOTFND => Object does not exist.
C                     Lower level routines may return other error
C                     codes.
C-
C     Subroutines / functions used -
C
C     DTA_SPLITN    (DTA_ package) Analyse a name's structure.
C     DTA_LOCATE    ( "     "    ) Looks for a name in the structure.
C     DTA_CACHEO    ( "     "    ) Remove an object from name cache.
C     DTA_TRNAME    ( "     "    ) Fold name to upper case and tidy up.
C     DTA_HDSERC    ( "     "    ) Convert HDS error code to DTA code.
C     DAT_ERASE     (HDS_   "    ) Recursive delete of a data object.
C     EMS_BEGIN     (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL     ( "    "     ) Clear current EMS error status.
C     EMS_END       ( "    "     ) End current reporting environment.
C
C                                            KS / CIT 25th May 1984
C     Modified:
C
C     20th Mar 1986  KS / AAO.  Re-written to use HDS routines.
C     4th  Feb 1987  KS / AAO.  Now uses the new, recursive, version
C                    of DAT_ERASE.  (This version will no longer
C                    work with the original BLISS version of HDS).
C     10th Jan 1992  KS / AAO.  Syntax of include statements changed to
C                    remove VMS logical names and to use lower case, to
C                    enable compilation on a SUN.
C     24th Jan 1992  KS / AAO. Calls to EMS added to control error reporting.
C     14th Oct 1992  HME / UoE, Starlink.  Locating one level up is
C                    not as easy as calling DTA_LOCATE with LEVELS-1.
C                    One must make sure that LASTC(LEVELS-1) points
C                    in front of any array index.
C     12th Mar 1993  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER STATUS
      CHARACTER*(*) NAME
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Data structure parameters -  those used are
C
C     DST_MAXLEV Maximum number of components in name
C     DST_MAXDIM   "       "    "  dimensions  "   "
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      LOGICAL FOUND
      INTEGER EMSTAT,LEVELS,LEVEL2,
     :   LASTC(DST_MAXLEV),LASTC2(DST_MAXLEV),
     :   NDIM,NDIM2,DIMS(DST_MAXDIM),DIMS2(DST_MAXDIM),LEV
      CHARACTER ENVLOC*(DAT__SZLOC), OBJLOC*(DAT__SZLOC)
      CHARACTER FNAME*64, OBJNAM*64
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Convert name to upper case and analyse it
C
      CALL DTA_TRNAME(NAME,FNAME)
      CALL DTA_SPLITN(FNAME,DST_MAXLEV,DST_MAXDIM,LEVELS,LASTC,
     :                                         NDIM,DIMS,STATUS)
      IF (STATUS.NE.0) GO TO 600
      IF (LEVELS.EQ.1) THEN
         STATUS=DTA_DELTOP
         GO TO 600
      END IF
      IF (NDIM.NE.0) THEN
         STATUS=DTA_DELELM
         GO TO 600
      END IF
C
C     We analyse the name up to one level higher, because we want to
C     locate the parent (environment) or the object to delete.
C
      CALL DTA_SPLITN(FNAME(:LASTC(LEVELS-1)),
     :   DST_MAXLEV,DST_MAXDIM,LEVEL2,LASTC2,NDIM2,DIMS2,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Look for the environment of the object in the structure.
C     Note, NOT the object itself, because DAT_ERASE needs an
C     environment locator and a component name.
C     We have to use LASTC2 here. One might think that's the same as
C     LASTC, but sometimes it is not. If you want to delete
C     REFNAME.AXIS[3].DATA_ARRAY, then LASTC(2) points to the ']', while
C     LASTC2(2) points in front of the '[', as needed by DTA_LOCATE.
C
      CALL DTA_LOCATE(FNAME,LEVELS-1,LASTC2,OBJLOC,FOUND,ENVLOC,
     :   LEV,STATUS)
      IF (STATUS.NE.0) GO TO 600
      IF (.NOT.FOUND) THEN
         STATUS=DTA_NOTFND
         GO TO 600
      END IF
      ENVLOC=OBJLOC
C
C     Remove any trace of this object from the name cache.
C
      CALL DTA_CACHEO(FNAME,LEVELS,LASTC,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Now we look for the object itself and then delete it, recursively.
C
      OBJNAM=FNAME(LASTC(LEVELS-1)+2:LASTC(LEVELS))
      CALL DAT_ERASE(ENVLOC,OBJNAM,STATUS)
      IF (STATUS.NE.0) CALL DTA_HDSERC(STATUS)
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

