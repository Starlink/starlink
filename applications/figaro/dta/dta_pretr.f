C+
      SUBROUTINE DTA_PRETR (NAME,NELM,ITYPE,LOC,HDSTYPE,NLDIM,LDIMS,
     :                                         UNDEF,TEMPLOC,STATUS)
C
C     D T A _ P R E T R
C
C     Performs perliminary transfer processing for the WRVARn,
C     RDVARn and MUVARn routines.  Analyses the object name,
C     and locates it in the data structure.  If necessary,
C     this routine will convert the object to a vector and slice
C     it to enable it to return an HDS locator that can be used to
C     access whatever subset (if any) of the object is required.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (>) NAME     (Character) The object name, in the usual
C                  data structure format, with dimension infor-
C                  mation, if any, at the end.  Terminated by
C                  a space or the string end. Case is not
C                  significant.
C     (>) NELM     (Integer) Number of elements to be transfered
C                  This is checked against the number of elements
C                  in the data structure definition for the object.
C     (>) ITYPE    (Integer) The type code for the data to be
C                  transfered from the user's buffer.  This is
C                  checked for validity by this routine.
C     (<) LOC      (Character*(DAT__SZLOC)) The HDS locator to be used for the
C                  data transfer.  If possible, the 'normal' locator
C                  to the data object will be used.  If the transfer
C                  will be to a subset of the data, then a locator to a
C                  slice of the data will be returned.  Any of these
C                  temporary locators will be associated with a group
C                  whose name is the top level name for the object (so
C                  DTA_FCLOSE will annul them), or they can be annuled
C                  explicitly once the transfer is completed.
C     (<) HDSTYPE  (Character) The HDS type code for the user's data
C                  type (ie the HDS type corresponding to ITYPE).
C     (<) NLDIM    (Integer) The number of dimensions of the
C                  data object associated with the locator LOC.
C     (<) LDIMS    (Integer array LDIMS(DST_MAXDIM)) The dimensions
C                  of the data object associated with the locator LOC.
C                  NLDIM and LDIMS should be used in the subsequent
C                  call to DAT_GET/PUT etc.
C     (<) UNDEF    (Logical) True if the data object is undefined, false
C                  otherwise.
C     (<) TEMPLOC  (Logical) True if LOC is a temporary locator generated
C                  for this transfer, false if it is the 'normal' locator
C                  for the object.  The main distinction is that normal
C                  locators are found in the name cache and annuled
C                  automatically, temporary ones need to be annuled
C                  explicitly.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK
C                  DTA_RNGERR => Transfer will exceed the number
C                                of elements in the data structure.
C                  DTA_NOTFND => Object does not exist
C                  DTA_INVTYP => ITYPE is invalid
C                  DTA_CHRCVT => Attempt to convert character data
C                  DTA_INVCHR => Invalid transfer involving character
C                                data (implementation limitation).
C                  DTA_INVDIM => NAME specifies too many dimensions.
C                  Lower level routines may return other error codes
C
C     Subroutines / functions used -
C
C     DTA_SPLITN  (DTA_ package) Analyse object name
C     DTA_LOCATE  ( "     "    ) Locate object in data structure
C     DTA_HDSERC  ( "     "    ) Convert HDS error code to DTA code
C     DTA_DTATYP  ( "     "    ) Get DTA type given HDS type
C     DTA_TRNAME  ( "     "    ) Fold name to upper case and tidy up
C     DTA_TYTIDY  ( "     "    ) Generate acceptable HDS type name
C     DAT_SHAPE   (HDS_   "    ) Get dimensions of object
C     DAT_ANNUL   ( "     "    ) Annul a locator
C     DAT_SLICE   ( "     "    ) Get slice through an object
C     DAT_VEC     ( "     "    ) Convert object to a vector
C     DAT_CELL    ( "     "    ) Get locator to element of array object
C     DAT_TYPE    ( "     "    ) Get type of object
C     DAT_STATE   ( "     "    ) See if object has been defined
C     HDS_LINK    ( "     "    ) Associate locator with group
C
C                                      KS / CIT  27th Oct 1982
C     Modified -
C
C     9th  Jan 1985  KS / AAO.  Code to calculate OFFSET corrected.
C                    Used to work for NDIM<=2, but not if NDIM>2.
C     17th Mar 1986  KS / AAO.  Complete re-write to use HDS
C                    routines.  Calling sequence changed significantly.
C     8th  May 1986  KS / AAO.  GNAME introduced to trap otherwise
C                    invalid HDS type names.
C     19th May 1986  KS / AAO.  Check for use of whole of array
C                    modified to trap case where NELM=array length but
C                    start element is not 1.
C     2nd  Oct 1986  KS / AAO.  Modified to work with new (C) version
C                    of HDS.  Old (Bliss) version of DAT_SLICE treated a
C                    vector of length 1 as scalar, the new version is more
C                    sensible.  Call to DAT_SHAPE added to allow routine to
C                    work with both versions.
C     6th  Feb 1990  KS / AAO.  Modified to allow a little more flexibility
C                    with character string transfers.  Now allows transfers to
C                    span strings.
C     16th Feb 1990 KS / AAO.  If character string is scalar (ie just one
C                    string), don't let transfer try to span strings.  KS/AAO.
C     10th Jan 1992  KS / AAO.  Syntax of include statements changed to
C                    remove VMS logical names and to use lower case, to
C                    enable compilation on a SUN.
C     14th Oct 1992  HME / UoE, Starlink.  Append 'G' to top level name
C                    to construct HDS group name.
C     12th Mar 1993  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Data structure parameters - used are
C
C     DST_MAXDIM   Maximum number of dimensions
C     DST_MAXLEV   Maximum number of levels in names
C
      INCLUDE 'DTASDEF'
C
C     Parameters -
C
      LOGICAL UNDEF, TEMPLOC
      CHARACTER*(*) NAME, LOC, HDSTYPE
      INTEGER NELM, ITYPE, NLDIM, LDIMS(DST_MAXDIM), STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Data type definitions & parameters - used are
C
C     NTYPES   Number of possible data types
C     HTYPE    The HDS types (character)
C
      INCLUDE 'DTATCON'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables
C
      LOGICAL CHAR, FOUND, GOTLOC, STATE
      INTEGER ASTAT, DIMVAL, DIMS(DST_MAXDIM), ELEMNT, I, IBLANK
      INTEGER LASTC(DST_MAXLEV), LEV, LEVELS, NDIM, NCHAR, NTRANS
      INTEGER OBJDIM, OBJDIMS(DST_MAXDIM), OBJELM
      CHARACTER FNAME*80, GNAME*17
      CHARACTER*(DAT__SZLOC) OBJLOC, ENVLOC, LOC2
      CHARACTER TYPE*16, OBJTYPE*16
C
C     Include file containing DATA statements
C
      INCLUDE 'DTATYPES'
C
C     Start by assuming we don't need new locators
C
      TEMPLOC=.FALSE.
C
C     Check ITYPE validity and get corresponding HDS type
C
      IF ((ITYPE.LE.0).OR.(ITYPE.GT.NTYPES)) THEN
         STATUS=DTA_INVTYP
         GO TO 600
      END IF
      HDSTYPE=HTYPE(ITYPE)
C
C     Convert NAME to upper case and analyse it
C
      CALL DTA_TRNAME(NAME,FNAME)
      CALL DTA_SPLITN(FNAME,DST_MAXLEV,DST_MAXDIM,LEVELS,LASTC,
     :                                          NDIM,DIMS,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Look for it in the data structure
C
      CALL DTA_LOCATE(FNAME,LEVELS,LASTC,OBJLOC,FOUND,ENVLOC,
     :                                              LEV,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Does the object exist?
C
      IF (.NOT.FOUND) THEN
         STATUS=DTA_NOTFND
         GO TO 600
      END IF
C
C     It exists, so get its shape (dimensions as stored in the
C     structure) its type, and whether or not it has been defined.
C
      CALL DAT_SHAPE(OBJLOC,DST_MAXDIM,OBJDIMS,OBJDIM,STATUS)
      CALL DAT_TYPE(OBJLOC,OBJTYPE,STATUS)
      CALL DAT_STATE(OBJLOC,STATE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
      UNDEF=.NOT.STATE
      CALL DTA_DTATYP(OBJTYPE,TYPE,NCHAR,STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     If an array element has been specified, see which element,
C     using the structure dimension information (OBJDIMS) and the
C     index numbers for the element (DIMS).  For character transfers,
C     check that the transfer starts at the start of a string (DIMS(1)=1)
C     and then modify DIMS values so that we're talking in terms of
C     strings rather than individual characters.  NTRANS is then the
C     effective number of basic items to transfer.
C
      NTRANS=NELM
      CHAR=TYPE.EQ.'CHAR'
      IF (CHAR) THEN
         IF (OBJDIM.EQ.0) THEN
            NTRANS=1
         ELSE
            NTRANS=(NELM+NCHAR-1)/NCHAR
         END IF
      END IF
      IF (NDIM.EQ.0) THEN
         ELEMNT=1
      ELSE
         IF (OBJDIM.LT.(NDIM-1)) THEN
            STATUS=DTA_INVDIM
            GO TO 600
         END IF
         IF (CHAR) THEN
            IF (DIMS(1).NE.1) THEN
               STATUS=DTA_INVCHR
               GO TO 600
            END IF
            NDIM=NDIM-1
            DO I=1,NDIM
               DIMS(I)=DIMS(I+1)
            END DO
         END IF
         ELEMNT=DIMS(1)
         IF (NDIM.GE.2) THEN
            DIMVAL=1
            DO I=2,NDIM
               DIMVAL=OBJDIMS(I-1)*DIMVAL
               ELEMNT=ELEMNT+(DIMS(I)-1)*DIMVAL
            END DO
         END IF
      END IF
C
C     We don't try to do string to numeric conversions, so check
C     for that.
C
      IF (CHAR) THEN
         IF (ITYPE.NE.TYP_DSCHAR) THEN
            STATUS=DTA_CHRCVT
            GO TO 600
         END IF
      ELSE
         IF (ITYPE.EQ.TYP_DSCHAR) THEN
            STATUS=DTA_CHRCVT
            GO TO 600
         END IF
      END IF
C
C     When we do the transfer, the locator we specify must match the
C     transfer that we want to perform.  If we are transferring a subset
C     of an array, we need a locator to that subset of the array.  So
C     we have to look carefully at the shape of the data object.
C
      IF (OBJDIM.EQ.0) THEN
C
C        Object in the structure is a scalar, and the locator
C        therefore is for the data itself.  Make sure we are
C        attempting a scalar transfer.
C
         IF ((ELEMNT.GT.1).OR.(NTRANS.GT.1)) THEN
            STATUS=DTA_RNGERR
            GO TO 600
         END IF
         NLDIM=0
         LOC=OBJLOC
C
      ELSE
C
C        Object in the structure is n-dimensional.  If the transfer
C        specifies the whole object, we can use it as it stands.
C
         OBJELM=1
         DO I=1,OBJDIM
            OBJELM=OBJELM*OBJDIMS(I)
         END DO
         IF ((OBJELM.EQ.NTRANS).AND.(ELEMNT.EQ.1)) THEN
            LOC=OBJLOC
            NLDIM=OBJDIM
            DO I=1,NLDIM
               LDIMS(I)=OBJDIMS(I)
            END DO
         ELSE
C
C           Transfer does not specify the whole object.  The
C           simplest thing is to turn it into a vector (I abhor
C           the word 'vectorise') and then to slice that vector.
C
            IF ((ELEMNT+NTRANS-1).GT.OBJELM) THEN
               STATUS=DTA_RNGERR
               GO TO 600
            END IF
            CALL DAT_VEC(OBJLOC,LOC2,STATUS)
            GOTLOC=STATUS.EQ.0
C
C     Now the group name is constructed from the top level name by
C     appending 'G'. If we use the top level name, that is usually a DSA
C     reference name, which may well be identical to a parameter name.
C     It seems that (A)PAR uses group names equal to parameter names, so
C     if GNAME is a parameter name, the following HDS_FLUSH may upset
C     (A)PAR. Appending 'G' makes this less likely.
C     The search for ' ' in GNAME is safe, since the top level name can
C     only be 16 characters, and if GNAME (*17) is finally longer than
C     15 then HDS will fail.
C
            GNAME=FNAME(:LASTC(1))
            IBLANK=MAX(1,INDEX(GNAME,' '))
            GNAME(IBLANK:)='G'
            CALL DTA_TYTIDY(GNAME)
            CALL HDS_LINK(LOC2,GNAME,STATUS)
            CALL DAT_SLICE(LOC2,1,ELEMNT,ELEMNT+NTRANS-1,LOC,STATUS)
C
C           The call to DAT_SHAPE makes sure that we treat the new
C           vector the way the HDS version treats it - this differs
C           between the C and BLISS versions.
C
            CALL DAT_SHAPE(LOC,1,LDIMS,NLDIM,STATUS)
            TEMPLOC=STATUS.EQ.0
            ASTAT=0
            IF (GOTLOC) CALL DAT_ANNUL(LOC2,ASTAT)
            IF (STATUS.EQ.0) STATUS=ASTAT
            IF (STATUS.NE.0) THEN
               CALL DTA_HDSERC(STATUS)
               GO TO 600
            END IF
         END IF
      END IF
C
  600 CONTINUE
C
      END

