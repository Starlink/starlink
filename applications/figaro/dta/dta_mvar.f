C+
      SUBROUTINE DTA_MVAR (NAME,NITEM,ITYPE,MODE,POINTER,STATUS)
C
C     D T A _ M V A R
C
C     General routine for obtaining mapped access to an object
C     in the data structure.  This routine returns a pointer
C     to a range of virtual addresses which are mapped onto
C     all or part of the specified data object.  The pointer
C     may be to a range of addresses which have been mapped
C     directly onto the data structure disk file itself, or
C     if conversion or alignment changes were needed, will be to
C     a range of addresses containing the converted or
C     aligned data.  This routine is called by DTA_MUVAR and
C     DTA_MRVAR, the only difference being the value of MODE used.
C
C     Parameters -   (">" input, "<" output )
C
C     (>) NAME     (Character) The object name, in the standard
C                  data structure format.  Should end with a
C                  blank or the string end, and is case
C                  insignificant.
C     (>) NITEM    (Integer) The number of elements to be
C                  read from the data structure.
C     (>) ITYPE    (Integer) The type code for the data to
C                  be read. Should be one of the TYP_DS..
C                  codes (not a conversion code).
C     (>) MODE     (Character) The mapping mode to be used.  Should
C                  be a mode accepted by the HDS routine DAT_MAP,
C                  eg 'READ', 'WRITE', or 'UPDATE'.  Must be in
C                  upper case.
C     (<) POINTER  (Integer) Pointer to the mapped data.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK
C                  DTA_MAXMAP =>  Maximum number of objects have
C                                 already been mapped.
C
C                  Lower level routines may return other error codes
C
C     Common variables used -
C
C     (!) MUSED    Flags for mapping information blocks
C     (!) MUSTART  Addresses of start of mapped data
C     (!) MUNAME   Names of mapped objects
C     (!) MUTYPES  Type code for mapped object
C     (!) MUITEMS  Number of items mapped
C     (!) MULOCS   Locators for mapped items
C     (!) MUMODE   Mode used for mapping of objects
C
C     all in blocks MUINFO and MUCHAR
C
C     Subroutines / functions used -
C
C     DTA_PRETR    (DTA_ package) Preliminary set up for data transfer
C     DTA_HDSERC   ( "      "   ) Convert HDS error code to DTA code
C     DTA_TYTIDY   ( "      "   ) Generate acceptable HDS type name
C     DAT_VALID    (HDS     "   ) See if locator is valid
C     DAT_MAP      ( "      "   ) Map a data object
C     DAT_CLONE    ( "      "   ) Clone a copy of an HDS locator
C     DAT_ANNUL    ( "      "   ) Annul an HDS locator
C     HDS_LINK     ( "      "   ) Link a locator into a group
C     DTA_TRNAME   ( "      "   ) Fold name to upper case and tidy up
C     EMS_BEGIN    (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL    ( "    "     ) Clear current EMS error status.
C     EMS_END      ( "    "     ) End current reporting environment.
C
C                                         KS / AAO  28th March 1986
C
C     This is a new routine written for the HDS-based version of the
C     DTA_ routines, combining the functions of DTA_MUVAR and DTA_MRVAR.
C
C     Modified:
C
C     8th  May 1986  KS / AAO.  GNAME added to trap otherwise invalid
C                    HDS group names.
C     22nd Oct 1986  KS / AAO.  Bug fixed in character mapping.  ITYPE
C                    was being tested against TYP_CHAR instead of TYP_DSCHAR.
C     20th Jan 1992  KS / AAO.  Syntax of include statements changed to
C                    remove VMS logical names and to use lower case, to
C                    enable compilation on a SUN.  DUMMY now explicitly set
C                    to zero before being used as an inherited status value,
C                    and comments added as to the way DTA_CHRPTR works
C                    on a UNIX system. Unused variables SIZE and TMODE removed.
C     24th Jan 1992  KS / AAO. Calls to EMS added to control error reporting.
C     12th Feb 1992  KS / AAO. DTA_CHRPTR changed to DTAZ_CHRPTR to emphasise
C                    system-dependence.
C     14th Oct 1992  HME / UoE, Starlink.  Changed back to DTA_*. The
C                    platform dependence is expressed in the source file
C                    name.
C                    Append 'G' to top level name to construct HDS group
C                    name.
C     12th Mar 1993  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C     2005 May 31    MJC/Starlink Use CNF_PVAL for pointers to mapped
C                    data.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME, MODE
      INTEGER NITEM,ITYPE,POINTER,STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     DTA system parameters.  Used is
C
C     DST_MAXDIM    Maximum number of object dimensions
C
      INCLUDE 'DTASDEF'
C
C     Type definitions & parameters - used are -
C
C     TYP_DSCHAR    type code for character data object
C
      INCLUDE 'DTATCON'
C
C     Mapped data common block
C
C     MUMAX   maximum number of mapped data objects
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTAMAP'
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

C
C     Local variables
C
      LOGICAL TEMPLOC, UNDEF, VALID
      INTEGER DUMMY, EMSTAT, I, IBLANK, IDOT
      INTEGER LDIMS(DST_MAXDIM), NLDIM, OBJ
      CHARACTER FNAME*80, GNAME*17, HDSTYPE*16
      CHARACTER LOC*(DAT__SZLOC), TLOC*(DAT__SZLOC)
C
C     Include file containg DATA statements
C
      INCLUDE 'DTATYPES'
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Make sure name is in upper case for comparison with
C     the names in the common block.
C
      CALL DTA_TRNAME(NAME,FNAME)
C
C     See if we have any map slots left, and see if we already
C     have this object mapped in workspace. If we have, then
C     we can make a quick exit.
C
      OBJ=0
      DO I=1,MUMAX
         IF (MUSED(I)) THEN
            IF ((MUNAME(I).EQ.FNAME).AND.(MUTYPES(I).EQ.ITYPE)
     :          .AND.(MUITEMS(I).EQ.NITEM).AND.(MUMODE(I).EQ.MODE)) THEN
C
C              Have this one already.  Make sure the locator
C              is still valid, and if so use the address we
C              already have for it.  Otherwise, tidy up that slot.
C
               STATUS=0
               CALL DAT_VALID(MULOCS(I),VALID,STATUS)
               IF (STATUS.NE.0) THEN
                  CALL DTA_HDSERC(STATUS)
                  GO TO 600
               END IF
               IF (VALID) THEN
                  POINTER=MUSTART(I)
                  GO TO 600
               ELSE
                  MUSED(I)=.FALSE.
               END IF
            END IF
         ELSE
            OBJ=I
         END IF
      END DO
C
C     Check we have a slot left
C
      IF (OBJ.EQ.0) THEN
         STATUS=DTA_MAXMAP
         GO TO 600
      END IF
C
C     Perform preliminary processing - check NAME, locate it
C     in structures, get suitable locator for transfer.
C
      CALL DTA_PRETR(FNAME,NITEM,ITYPE,LOC,HDSTYPE,NLDIM,LDIMS,
     :                                    UNDEF,TEMPLOC,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Map the data.  Note that we always make sure that we use
C     a temporary locator, so the mapping of the data won't get
C     in the way of access via any original locator, and nor
C     will any annulling of that locator (as it gets uncached, for
C     example) interfere with the mapping.
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
      IF (TEMPLOC) THEN
         TLOC=LOC
      ELSE
         CALL DAT_CLONE(LOC,TLOC,STATUS)
         IDOT=MAX(INDEX(FNAME,'.'),2)
         GNAME=FNAME(:IDOT-1)
         IBLANK=MAX(1,INDEX(GNAME,' '))
         GNAME(IBLANK:)='G'
         CALL DTA_TYTIDY(GNAME)
         CALL HDS_LINK(TLOC,GNAME,STATUS)
      END IF
C
C     An attempt to map undefined data for update will fail, so
C     if that is the case, map for write only.
C
      IF ((MODE.EQ.'UPDATE').AND.UNDEF) THEN
         CALL DAT_MAP(TLOC,HDSTYPE,'WRITE',NLDIM,LDIMS,POINTER,STATUS)
      ELSE
         CALL DAT_MAP(TLOC,HDSTYPE,MODE,NLDIM,LDIMS,POINTER,STATUS)
      END IF
      IF (STATUS.NE.0) THEN
         DUMMY=0
         CALL DAT_ANNUL(TLOC,DUMMY)
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
C
C     Process the pointer for a character object - on a VAX, DAT_MAP returns
C     a pointer to the descriptor, whereas the DTA_ routines expect a
C     pointer to the actual data. Under UNIX, DAT_MAP returns a pointer to
C     the actual data, even for character data. This is handled by having
C     two versions of DTA_CHRPTR - on the VAX it does the necessary processing,
C     while under UNIX it is a null routine.
C
      IF (ITYPE.EQ.TYP_DSCHAR)
     :  CALL DTA_CHRPTR(%VAL( CNF_PVAL(POINTER) ), POINTER)
C
C     Update the common blocks.
C
      MUSED(OBJ)=.TRUE.
      MUNAME(OBJ)=FNAME
      MUITEMS(OBJ)=NITEM
      MULOCS(OBJ)=TLOC
      MUSTART(OBJ)=POINTER
      MUMODE(OBJ)=MODE
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END

