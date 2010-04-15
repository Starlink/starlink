C+
      SUBROUTINE DTA_ASFNAM (ENAME,FILE,EXIST,BLOCKS,TYPE,STATUS)
C
C     D T A _ A S F N A M
C
C     Performs the initial association between a top level
C     data-structure name and a named file.  The file is
C     opened (or created and opened) and all the pointers
C     etc used by the data system are initialised.  This
C     routine has to be called before any other data structure
C     programs can reference part of the specified data
C     structure in that file.
C
C     Parameters - (">" input, "<" output, "!" modified)
C
C     (>) ENAME   (Character) The top level data structure
C                 name - the 'environment' name for the
C                 whole file, if you like.  Should not contain
C                 '.' or '[' and should be terminated either
C                 by a blank or by the end of the string.
C     (>) FILE    (Character) The name of the disk file
C                 containing (or to contain) the data structure.
C                 This should be a full filename. Under VMS, it can
C                 also be a logical name equivalenced to a VMS file.
C     (>) EXIST   (Character) Defines whether the file already
C                 exists or should be created.  It must be
C                 one of 'NEW', 'OLD' or 'UNKNOWN', these having
C                 the same meanings as in a Fortran OPEN
C                 statement:
C                 'NEW' => create a new file
C                 'OLD' => use existing file
C                 'UNKNOWN' => use an existing file if there is
C                              one, otherwise create a new one.
C     (>) BLOCKS  (Integer) Number of disk blocks for the initial
C                 file allocation.  This is not a critical parameter,
C                 since the file will be extended as necessary, but
C                 the system runs more efficiently if this does not
C                 have to happen too often.
C     (>) TYPE    (Character) The structure type for the data.  Should
C                 be terminated either by a blank or the end of the
C                 string.  Case is unimportant.  This is really
C                 just a name that goes into the file.
C     (<) STATUS  (Integer) Returns a status code.
C                 0 => OK
C                 DTA_INVNAM => NAME is invalid
C                 DTA_EXIST  => NAME already exists
C                 DTA_INVPAR => EXIST is not one of three
C                               allowed strings.
C                 Other status codes may be passed down from the
C                 lower level routines called.
C
C     Note -
C
C     All the character parameters may be in upper or lower case.  The file
C     name is subject to some operating system considerations.  Under VMS,
C     file names are all upper case, and lower case characters in file names
C     are treated as upper case.  Also, under VMS, if a new file is created
C     with the same name as an existing file, then a new version of the file
C     is created - the version number being appended to the file name following
C     a semi-colon.  Under UNIX, file names may have mixed case, and upper and
C     lower case versions of the same letter are regarded as different letters.
C     Also under UNIX, files do not normally have version numbers.  However,
C     DTA, for compatability between the VAX and UNIX versions, prefers to
C     treat file names as case-insignificant, and to use version numbers when
C     a new file of the same name as an existing file is created. So, under
C     UNIX, by default all file names are folded to lower case - this being
C     regarded as the natural UNIX case, and version numbers are appended to
C     the file names if necessary, in this case preceded by a comma. The
C     folding to lower case may be prevented by setting the environment
C     variable INHIBIT_FOLDING and the addition of version numbers may be
C     prevented by setting the environment variable INHIBIT_VERSIONS. (These
C     can be set to anything - it's having them set at all that matters.)
C-
C     Subroutines / functions used -
C
C     DTA_ICACHE  (DTA_ package) Initialise name cache
C     DTA_FAINIT  ( "    "     ) Initialise file access routines
C     DTA_CACHEN  ( "    "     ) Set name in name cache
C     DTA_CACHEL  ( "    "     ) Look for name in name cache
C     DTA_TRNAME  ( "    "     ) Fold name to upper case and tidy up.
C     DTA_TYTIDY  ( "    "     ) Make sure a type is a valid HDS type.
C     DTA_FILET   ( "    "     ) Record top level name in common.
C     DTA_HDSERC  ( "    "     ) Convert HDS error code to DTA code.
C     DTA_FILEFOLD( "    "     ) Fold a file name as required for the O/S.
C     DTA_VERSNAME( "    "     ) Deal with version names (system-dependent)
C     HDS_TRACE   (HDS_  "     ) Trace path of object & get filename.
C     EMS_BEGIN   (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL   ( "    "     ) Clear current EMS error status.
C     EMS_END     ( "    "     ) End current reporting environment.
C     ICH_FOLD    (ICH_  "     ) Convert a string to upper case
C
C                                         KS / CIT  9th Nov 1982
C     Modified:
C
C     10th Mar 1986.  KS / AAO.  Modified for use with the HDS
C                     based version of the DTA_ package.  DTA_ICACHE
C                     now has a status parameter, and there is no
C                     longer any need to create the initial dummy record
C                     in a new file.  Locators are now character strings.
C                     TYPE now has to be passed to DTA_FAINIT.
C     26th May 1986.  KS / AAO.  Now records the names of all top-level
C                     structures in common using DTA_FILET.
C     16th Jun 1986.  KS / AAO.  File passed to DTA_FILET is now the
C                     full container file name.
C     7th  Jan 1992.  KS / AAO.  Include file syntax modified to compile
C                     on SUNs. Unused variable I removed.  Call to DTA_ICACHE
C                     moved to start of code - where it should always have
C                     been.
C     24th Jan 1992.  KS / AAO. Calls to EMS added to control error reporting.
C     10th Feb 1991.  KS / AAO. Calls to DTA_VERSNAME and DTA_FILEFOLD
C                     added to handle operating system file name
C                     considerations.
C     12th Feb 1992.  KS / AAO. System-dependent routines now called
C                     DTAZ_ routines.
C     18th Aug 1992.  HME / UoE. Changed back to DTA_*. The platform
C                     dependence is expressed in having several source files
C                     dta_*.?-<mach> and a soft link dta_*.? to
C                     the correct one. The VAX build.com will compile
C                     DTA_*.?-VAX.
C     12th Mar 1993.  HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) ENAME,FILE,EXIST,TYPE
      INTEGER STATUS,BLOCKS
C
C     Error codes
C
      INCLUDE 'DTACODES'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      LOGICAL NEW
      INTEGER EMSTAT,IEND,INVOKE,LEV
      CHARACTER FEXIST*7,FENAME*16
      CHARACTER LOC*(DAT__SZLOC)
      CHARACTER FTYPE*16,PATH*16,CFILE*80
      CHARACTER NFILE*132,FFILE*132
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Ensure name cache is initialised
C
      CALL DTA_ICACHE(STATUS)
      IF (STATUS.NE.0) GO TO 600
C
C     Check environment name validity and get length
C
      IEND=INDEX(ENAME,' ')-1
      IF (IEND.LT.0) IEND=LEN(ENAME)
      IF ((INDEX(ENAME(:IEND),'.').NE.0).OR.
     :    (INDEX(ENAME(:IEND),'[').NE.0))   GO TO 530
      IF (IEND.GT.16) GO TO 530
      CALL DTA_TRNAME(ENAME,FENAME)
C
C     See if we already have NAME (search for it should fail)
C
      CALL DTA_CACHEL(ENAME,1,IEND,LEV,LOC,STATUS)
      IF (STATUS.EQ.0)   GO TO 540
C
C     Name looks OK. Fold EXIST and check its value
C
      FEXIST=EXIST
      INVOKE=ICH_FOLD(FEXIST)
      IF ((FEXIST.NE.'OLD').AND.(FEXIST.NE.'NEW').AND.
     :    (FEXIST.NE.'UNKNOWN')) GO TO 520
C
C     Make sure TYPE is valid
C
      FTYPE=TYPE
      INVOKE=ICH_FOLD(FTYPE)
      CALL DTA_TYTIDY(FTYPE)
C
C     Open the file and initialise file access routines for it, after first
C     modifying it as required by the operating system - version names,
C     folding to lower case, etc.  (DTA_VERSNAME and DTA_FILEFOLD are
C     opearting-system dependent routines.)
C
      NFILE=' '
      FFILE=FILE
      CALL DTA_FILEFOLD(FFILE)
      CALL DTA_VERSNAME(FFILE,FEXIST,NFILE)
      CALL DTA_FAINIT(NFILE,FEXIST,BLOCKS,FTYPE,LOC,NEW,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Stick ENAME in the name cache, at top level
C
      CALL DTA_CACHEN(FENAME,LOC,1,STATUS)
      IF (STATUS.NE.0) GO TO 600
      CALL HDS_TRACE(LOC,LEV,PATH,CFILE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
      CALL DTA_FILET(FENAME,ENAME,CFILE,STATUS)
C
C     Normal end
C
      GO TO 600
C
C     Error returns
C
  520 CONTINUE
      STATUS=DTA_INVPAR
      GO TO 600
  530 CONTINUE
      STATUS=DTA_INVNAM
      GO TO 600
  540 CONTINUE
      STATUS=DTA_EXIST
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

