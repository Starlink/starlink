C+
      SUBROUTINE DTA_LOCATE (NAME,LEVELS,LASTC,OBJLOC,FOUND,
     :                                      ENVLOC,LEV,STATUS)
C
C     D T A _ L O C A T E
C
C     Locates a named object in the data structure.  First it
C     looks in the name cache; if the top level has been opened,
C     there will be a name cache entry, so it will either find
C     the object immediately, or will have a pointer to a higher
C     level environment.  If it gets a higher level match, it
C     has to look down through the structure for the object. Note
C     that if NAME specifies an array element, DTA_LOCATE will
C     ignore this and returns a locator to the array itself and
C     not to the specific element, if the object in question is
C     primitive.  However, if a structure array element is specified,
C     DTA_LOCATE will return a locator to the element itself.  If
C     the array exists but does not contain the specified element,
C     DTA_LOCATE returns an error.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME      (Character) The object name.  This should be
C                   in upper case, and should already have been
C                   analysed by DTA_SPLITN to obtain its level
C                   structure.
C     (>) LEVELS    (Integer) The number of components in the
C                   object name. (As found by DTA_SPLITN).
C     (>) LASTC     (Integer LASTC(LEVELS)) Positions of the
C                   last characters in each of the component names
C                   in NAME.  (As found by DTA_SPLITN).
C     (<) OBJLOC    (Character) Locator for the object, if found.
C     (<) FOUND     (Logical) True if the object was found, false
C                   if it was not.
C     (<) ENVLOC    (Character) Locator for the environment of the
C                   object, if the immediate environment exists.
C                   ENVLOC is only returned if the object itself
C                   was not found.  (Note: any locators activated
C                   by this routine are linked to a group with the
C                   same name as the top level of the object.)
C     (<) LEV       (Integer) The last level searched.  If the
C                   object is found, this will be equal to LEVELS.
C     (<) STATUS    (Integer) Returns a status code.
C                   0 => No errors.  NOTE: this does not imply that
C                        the object was found, merely that the
C                        structure was not illegal and there were no
C                        lower level errors (I/O, for example).
C                   DTA_INVLEV => One of the higher level components
C                                 of NAME is not a structure.
C                   DTA_STRARY => NAME contains a structure array,
C                                 which this system cannot handle.
C                   Lower level routines may return other error codes.
C                   If STATUS is non-zero, the values for FOUND,
C                   OBJLOC, ENVLOC etc. may not be meaningful.
C
C     Subroutines / functions called -
C
C     DTA_CACHEL   (DTA_ package) Look for name in cache
C     DTA_CACHEN   ( "      "   ) Insert name into name cache
C     DTA_DECDIM   ( "      "   ) Decode dimension info in a string
C     DTA_HDSERC   ( "      "   ) Convert HDS error code to DTA code
C     DTA_TYTIDY   ( "      "   ) Get acceptable HDS type name
C     DAT_CELL     (HDS_    "   ) Get locator to element of an array
C     DAT_STRUC    ( "      "   ) Test if an object is a structure
C     DAT_SHAPE    ( "      "   ) Determine shape of object
C     DAT_FIND     ( "      "   ) Locate named component of structure
C     HDS_LINK     ( "      "   ) Link a locator with a given group
C     EMS_BEGIN    (EMS_    "   ) Start a new error context
C     EMS_ANNUL    ( "      "   ) Annul any current bad status
C     EMS_END      ( "      "   ) End the current error context
C
C                                       KS / CIT  3rd March 1983
C     Modified:
C
C     12th Mar 1986  KS / AAO.  Re-written to use HDS routines.
C                    Calling sequence changed: FOUND added, last
C                    object locator no longer returned.
C     8th  May 1986  KS / AAO.  GNAME added to catch otherwise
C                    invalid group names.
C     21st Jun 1988  KS / AAO.  Modified to support structure arrays.
C     8th  Mar 1990  KS / AAO.  Structure array element locators were
C                    being cached with the name of the array not the
C                    element.  Fixed.
C     8th  Jan 1992  KS / AAO.  Syntax of include statements changed to
C                    remove VMS logical names and to use lower case, to
C                    enable compilation on a SUN.
C     24th Jan 1992  KS / AAO. EMS calls added to control error reporting.
C     14th Oct 1992  HME / UoE, Starlink.  Now use (!) EMS_STATUS in
C                    call to DAT_FIND.
C                    Append 'G' to top level name to construct HDS group
C                    name.
C     12th Mar 1993  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters -
C
      LOGICAL FOUND
      CHARACTER*(*) NAME,ENVLOC,OBJLOC
      INTEGER LEVELS,LASTC(LEVELS),LEV,STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     DTA_ system parameters - used is
C
C     DST_MAXDIM    Maximum number of dimensions in an object
C     DST_MAXLEV    Maximum number of structure levels
C
      INCLUDE 'DTASDEF'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables -
C
      LOGICAL ENDED, STRUC
      INTEGER ACTDIM, DIMPTR, DIMS(DST_MAXDIM), EMS_STATUS, I, IEND
      INTEGER IBLANK, LNAME, NAMLEN, NDIM, TDIM, TLAST(DST_MAXLEV)
      CHARACTER ELMLOC*(DAT__SZLOC), LOC*(DAT__SZLOC)
      CHARACTER GNAME*17, OBJNAM*16
C
C     Will have to use name for a group name, so generate a
C     name from it that HDS will accept.  Note: - so long as a top-level
C     name cannot be an array, we don't have to worry about whether
C     or not HDS will accept '[]' characters.
C     Now the group name is constructed from the top level name by
C     appending 'G'. If we use the top level name, that is usually a DSA
C     reference name, which may well be identical to a parameter name.
C     It seems that (A)PAR uses group names equal to parameter names, so
C     if GNAME is a parameter name, an HDS_FLUSH may upset
C     (A)PAR. Appending 'G' makes this less likely.
C     The search for ' ' in GNAME is safe, since the top level name can
C     only be 16 characters, and if GNAME (*17) is finally longer than
C     15 then HDS will fail.
C
      GNAME=NAME(:LASTC(1))
      IBLANK=MAX(1,INDEX(GNAME,' '))
      GNAME(IBLANK:)='G'
      CALL DTA_TYTIDY(GNAME)
C
C     Look up name in name cache
C
      CALL DTA_CACHEL(NAME,LEVELS,LASTC,LEV,LOC,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Is this the object itself? If so, then we're almost there, but
C     we have to allow for the possibility that what've got is an array
C     of structures, of which we actually want a specific element.
C
      ENDED=.FALSE.
      IF (LEV.EQ.LEVELS) THEN
         CALL DAT_STRUC(LOC,STRUC,STATUS)
         IF (STRUC) CALL DAT_SHAPE(LOC,1,TDIM,ACTDIM,STATUS)
         IF (STATUS.NE.0) THEN
            CALL DTA_HDSERC(STATUS)
            GO TO 600
         END IF
         IF (STRUC.AND.(ACTDIM.GT.0)) THEN
C
C           It is a structure array.  Now, we have to look to see if NAME
C           actually contained dimension info for the final element.
C           If so, we get the locator to the element itself.
C
            DIMPTR=INDEX(NAME(LASTC(LEV):),'[')
            IF (DIMPTR.GT.0) THEN
               CALL DTA_DECDIM(NAME,DIMPTR+LASTC(LEV)-1,DST_MAXDIM,
     :                                           IEND,NDIM,DIMS,STATUS)
               IF (STATUS.NE.0) GO TO 600
               CALL DAT_CELL(LOC,NDIM,DIMS,ELMLOC,STATUS)
C
C              And having got it, we have to cache it.
C              It is just possible that the full name has already been
C              cached.  If so, we don't want to cache it twice.  This
C              really is all getting just a bit messy, since it means
C              we want to look up the full name in the cache, which
C              DTA_CACHEL wasn't originally intended for.
C
               DO I=1,LEVELS-1
                  TLAST(I)=LASTC(I)
               END DO
               TLAST(LEVELS)=IEND
               CALL DTA_CACHEL(NAME,LEVELS,LASTC,LEV,LOC,STATUS)
               IF (LEV.NE.LEVELS) THEN
                  CALL DTA_CACHEN(NAME(:IEND),ELMLOC,LEV,STATUS)
                  IF (STATUS.NE.0)  GO TO 600
                  CALL HDS_LINK(ELMLOC,GNAME,STATUS)
                  IF (STATUS.NE.0) THEN
                     CALL DTA_HDSERC(STATUS)
                     GO TO 600
                  END IF
               END IF
               LOC=ELMLOC
            END IF
         END IF
C
C        This is the usual case, where we don't have to worry about
C        structure elements.  (Before structure arrays were supported,
C        this was all that had to be done at this point.  The program
C        has got a lot messier since then, and perhaps could be tidied
C        a little.  Much of this code is duplicated later on at the
C        end of the search down the structures.)
C
         OBJLOC=LOC
         FOUND=.TRUE.
         ENDED=.TRUE.
      END IF
C
C     Or are we going to have to dig around for it...
C
      DO WHILE (.NOT.ENDED)
C
C        We have a record that matches to some level, but not the
C        level we want.  Is this record a structure?
C
         CALL DAT_STRUC(LOC,STRUC,STATUS)
         IF (STATUS.NE.0) THEN
            CALL DTA_HDSERC(STATUS)
            GO TO 600
         END IF
         IF (.NOT.STRUC) THEN
C
C           No, it isn't.  However, it ought to be..
C
            STATUS=DTA_INVLEV
            ENDED=.TRUE.
         ELSE
C
C           It's OK. Make this the environment, go down one level, and
C           look for the next component in the object name.  Note that
C           LEV will always be at least 2 at this point, so LASTC(LEV-1)
C           is safe.  If dimension info is included in the object name,
C           we process it but clip it off when we call DAT_FIND.  Note that
C           the DAT_FIND call may well fail, and so we have to control its
C           error reporting through calls to EMS.
C
            ENVLOC=LOC
            LEV=LEV+1
            OBJNAM=NAME(LASTC(LEV-1)+2:LASTC(LEV))
            DIMPTR=INDEX(OBJNAM,'[')
            IF (DIMPTR.GT.0) THEN
               CALL DTA_DECDIM(OBJNAM,DIMPTR,DST_MAXDIM,IEND,NDIM,DIMS,
     :                                                          STATUS)
               IF (STATUS.NE.0) GO TO 600
               NAMLEN=DIMPTR-1
            ELSE
               NAMLEN=LEN(OBJNAM)
            END IF
            EMS_STATUS=0
            CALL EMS_BEGIN(EMS_STATUS)
            CALL DAT_FIND(ENVLOC,OBJNAM(:NAMLEN),LOC,EMS_STATUS)
            STATUS=EMS_STATUS
            CALL EMS_ANNUL(EMS_STATUS)
            CALL EMS_END(EMS_STATUS)
            IF (STATUS.NE.0) THEN
C
C              The component doesn't exist at this level, so we
C              have to stop here.
C
               FOUND=.FALSE.
               STATUS=0
               ENDED=.TRUE.
            ELSE
C
C              OK, the component exists, so we've got down one more
C              level.  First of all, see if this new component is an
C              array, and whether or not that's what we expected. (If
C              the object name had dimension info included, we check
C              for a structure array - note that dimension info, even
C              if specified in NAME, is not included in OBJNAM for the
C              final level.)
C
               CALL DAT_STRUC(LOC,STRUC,STATUS)
               IF (STRUC.AND.(STATUS.EQ.0)) THEN
                  CALL DAT_SHAPE(LOC,1,TDIM,ACTDIM,STATUS)
               END IF
               IF (STATUS.NE.0) THEN
                  CALL DTA_HDSERC(STATUS)
                  GO TO 600
               END IF
               LNAME=LASTC(LEV)
               IF (STRUC.AND.(ACTDIM.GT.0)) THEN
C
C                 It is a structure array.  Now, what do we want?
C                 If we're at the bottom level, then whether or not we want
C                 the array itself or the element depends on whether
C                 NAME contains dimension information for the final element.
C                 (Note that this is known already, having been determined
C                 by DTA_SPLITN, but is not passed to this routine - because
C                 the calling sequence predates structure array support.
C                 So this has to be found out the hard way.)  Adjust
C                 name length for cached name to include dims.

C
                  IF (LEV.EQ.LEVELS) THEN
                     DIMPTR=INDEX(NAME(LASTC(LEV):),'[')
                     IF (DIMPTR.GT.0) THEN
                        CALL DTA_DECDIM(NAME,DIMPTR+LASTC(LEV)-1,
     :                                DST_MAXDIM,IEND,NDIM,DIMS,STATUS)
                        LNAME=IEND
                        IF (STATUS.NE.0) GO TO 600
                     END IF
                  ELSE
C
C                    If we are at up upper level, we must have been
C                    given dimension information in NAME.  If not, we
C                    have an error.
C
                     IF (DIMPTR.EQ.0) THEN
                        STATUS=DTA_STRARY
                        GO TO 600
                     END IF
                  END IF
C
C                 If it is the specific element we need, we get a locator
C                 to that and throw away the original locator.
C
                  IF (DIMPTR.GT.0) THEN
                     CALL DAT_CELL(LOC,NDIM,DIMS,ELMLOC,STATUS)
                     CALL DAT_ANNUL(LOC,STATUS)
                     IF (STATUS.NE.0) THEN
                        CALL DTA_HDSERC(STATUS)
                        GO TO 600
                     END IF
                     LOC=ELMLOC
                  END IF
               END IF
C
C              Put this component in the name cache, and associate
C              its locator with a group for annulment later.
C
               CALL DTA_CACHEN(NAME(:LNAME),LOC,LEV,STATUS)
               IF (STATUS.NE.0)  GO TO 600
               CALL HDS_LINK(LOC,GNAME,STATUS)
               IF (STATUS.NE.0) THEN
                  CALL DTA_HDSERC(STATUS)
                  GO TO 600
               END IF
C
C              Is this the bottom level for NAME?
C
               IF (LEVELS.EQ.LEV) THEN
C
C                 Yes, so we've got it.  Really got it.
C                 Break out of the search loop.
C
                  OBJLOC=LOC
                  FOUND=.TRUE.
                  ENDED=.TRUE.
               ELSE
C
C                 If it isn't the bottom level, start the outer loop
C                 again, now with LOC one level down.  (This will
C                 happen automatically, so this else clause is null.)
C
               END IF
            END IF
         END IF
      END DO
C
  600 CONTINUE
C
      END

