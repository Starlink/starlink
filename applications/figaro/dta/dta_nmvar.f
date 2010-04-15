C+
      SUBROUTINE DTA_NMVAR (ENAME,POS,NAME,STATUS)
C
C     D T A _ N M V A R
C
C     If a structure is specified, this routine will
C     return the name of a specified one of its components.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) ENAME    (Character) The structure name - the full
C                  object name, with all the components.
C                  Terminated by a blank or the end of the
C                  string, and case-insensitive.
C     (>) POS      (Integer) The number of the component
C                  whose name is to be obtained. The first
C                  component is #1, not 0.
C     (<) NAME     (Character) Returns with the component
C                  name, truncated or blank filled, as necessary,
C                  in upper case.
C     (<) STATUS   (Integer) Receives a status code.
C                  0 => OK
C                  DTA_NOTFND => There is no POSth component
C                  Lower level routines may return other error codes
C-
C     Functions / subroutines used -
C
C     DTA_SPLITN   (DTA_ package) Analyse an object name
C     DTA_LOCATE   ( "     "    ) Locate object in data structure
C     DTA_HDSERC   ( "     "    ) Convert HDS error code to a DTA code
C     DTA_TRNAME   ( "     "    ) Fold name to upper case and tidy up
C     DAT_INDEX    (HDS_   "    ) Get Nth component of structure
C     DAT_NAME     ( "     "    ) Get name of component
C     DAT_ANNUL    ( "     "    ) Annul locator
C     EMS_BEGIN    (EMS_   "    ) Start new EMS reporting environment
C     EMS_ANNUL    ( "     "    ) Clear status for current EMS environment
C     EMS_END      ( "     "    ) End current EMS environment
C
C                                   KS / CIT  26th Oct 1982
C     Modified:
C
C     19th March 1986  KS / AAO.  Modified to use HDS routines.
C     2nd  June  1988  KS / AAO.  Modified to support arrays of structures.
C                      (This mod is actually null.  It was inserted and
C                      then removed after it was decided to modify
C                      DTA_LOCATE instead.)
C     10th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.  Unused variables removed.
C     22nd Jan  1992.  KS / AAO. Add EMS calls to prevent storing of bad
C                      status if the object doesn't exist.
C     24th Jan  1992.  KS / AAO. As this is a high level routine, add EMS calls
C                      at start and end of routine.  Really, this duplicates the
C                      previous mod, but keeps the structure right should DTA
C                      be modified to generate its own EMS reports. ASTAT set
C                      to zero before being used as an inherited status value.
C     12th Mar  1993.  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER POS,STATUS
      CHARACTER*(*) NAME,ENAME
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Data structure parameters -  used are -
C
C     DST_MAXLEV    Maximum number of components in a name
C     DST_MAXDIM    Maximum number of dimensions
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      LOGICAL FOUND, GOTLOC
      INTEGER LASTC(DST_MAXLEV),DIMS(DST_MAXDIM),LEVELS,NDIM,LEV
      INTEGER ASTAT,EMSTAT
      CHARACTER FNAME*80
      CHARACTER*(DAT__SZLOC) ENVLOC, LOC, OBJLOC
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Flags indicating use of temporary locators
C
      GOTLOC=.FALSE.
C
C     Convert name to upper case and analyse it
C
      CALL DTA_TRNAME(ENAME,FNAME)
      CALL DTA_SPLITN(FNAME,DST_MAXLEV,DST_MAXDIM,LEVELS,LASTC,
     :                                         NDIM,DIMS,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Look for the environment in the data structure
C
      CALL DTA_LOCATE(FNAME,LEVELS,LASTC,OBJLOC,FOUND,ENVLOC,
     :                                               LEV,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Was it found?
C
      IF (.NOT.FOUND) THEN
         STATUS=DTA_NOENV
         GO TO 600
      END IF
C
C     Get the POSth component.  Note that since DTA_NMVAR is often used
C     as an enquiry routine to determine the number of elements in a
C     structure - and that was the original intention - we don't want
C     an error here to be treated as significant.  Hence the EMS calls.
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
      CALL DAT_INDEX(OBJLOC,POS,LOC,STATUS)
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
      GOTLOC=.TRUE.
C
C     Get the name of the object.
C
      CALL DAT_NAME(LOC,NAME,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
C
  600 CONTINUE
C
C     If we got any temporary locators, we should annul them. Also close
C     down the EMS environment.
C
      ASTAT=0
      IF (GOTLOC) CALL DAT_ANNUL(LOC,ASTAT)
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END

