C+
      SUBROUTINE DTA_FCLOSE (ENAME,STATUS)
C
C     D T A _ F C L O S E
C
C     Closes down the file structure for a particular top
C     level data structure name.  Any mapped data objects in
C     the structure will be unmapped before the file is closed.
C
C     Parameters -    (">" input, "<" output, "!" modified)
C
C     (>) ENAME     (Character) The top level name. Should be
C                   terminated by either a blank or the end
C                   of the string.  Can be either upper or lower
C                   case.
C     (<) STATUS    (Integer) Returns a status code.
C                   0 => OK
C                   DTA_INVNAM => Invalid ENAME.
C                   Other values may be passed from lower level
C                   routines.
C-
C     Subroutines / functions called -
C
C     DTA_SPLITN  (DTA_package)  Analyse name structure
C     DTA_CACHEL  (    "      )  Look up name in name cache
C     DTA_FACLSE  (    "      )  Close file access routines
C     DTA_TRNAME  (    "      )  Fold name to upper case and tidy up
C     DTA_CACHER  (    "      )  Remove a top level name from cache
C     DTA_TYTIDY  (    "      )  Generate a valid HDS type name
C     DTA_FRALL   (    "      )  Unmaps any data objects in a structure
C     HDS_FLUSH   (HDS_package)  Annul a group of locators
C     EMS_BEGIN   (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL   ( "    "     ) Clear current EMS error status.
C     EMS_END     ( "    "     ) End current reporting environment.
C
C                                      KS / CIT  18th Oct 1982
C     Modified:
C
C     11th Mar 1986  KS / AAO Modified for use with the HDS based
C                    version of the DTA package.  Locators are now
C                    character variables.
C     8th  May 1986  KS / AAO  GNAME added to ensure group names
C                    are always valid HDS names.
C     29th May 1986  KS / AAO  DTA_FRALL now used to tidy up any
C                    mapped objects in the file being closed.
C     8th  Jan 1992  KS / AAO.  Syntax of include statements changed to
C                    remove VMS logical names and to use lower case, to
C                    enable compilation on a SUN.
C     24th Jan 1992  KS / AAO. Calls to EMS added to control error reporting.
C     14th Oct 1992  HME / UoE, Starlink.  Append 'G' to top level name
C                    to construct HDS group name.
C     12th Mar 1993 HME / UoE. Changed CHARACTER*15 to *(DAT__SZLOC).
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) ENAME
      INTEGER STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SAE_PAR'
C
C     Local variables -
C
      CHARACTER GNAME*17, UCNAME*16
      CHARACTER LOC*(DAT__SZLOC)
      INTEGER EMSTAT, LEV, LASTC, NDIM, DIM, IBLANK
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Convert ENAME to upper case, check its structure, check
C     it looks like a top level name
C
      CALL DTA_TRNAME(ENAME,UCNAME)
      CALL DTA_SPLITN(ENAME,1,1,LEV,LASTC,NDIM,DIM,STATUS)
      IF (STATUS.NE.0) GO TO 600
      IF ((NDIM.NE.0).OR.(LEV.NE.1).OR.(LASTC.GT.16)) THEN
         STATUS=DTA_INVNAM
         GO TO 600
      END IF
C
C     Look up name in cache.  It must be at top level, and
C     what we want is its HDS locator.
C
      CALL DTA_CACHEL(UCNAME,1,LASTC,LEV,LOC,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Unmap any data objects associated with this file
C
      CALL DTA_FRALL(UCNAME,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Annul any locators associated with this file - these will
C     be in a group with the same name as the top level name.
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
      GNAME=UCNAME
      IBLANK=MAX(1,INDEX(GNAME,' '))
      GNAME(IBLANK:)='G'
      CALL DTA_TYTIDY(GNAME)
      CALL HDS_FLUSH(GNAME,STATUS)
      IF (STATUS.NE.0) THEN
         CALL DTA_HDSERC(STATUS)
         GO TO 600
      END IF
C
C     Close down low level file access routines
C
      CALL DTA_FACLSE(LOC,STATUS)
      IF (STATUS.NE.0)  GO TO 600
C
C     Now remove all trace of this structure from the name cache
C     Note that if this was the only remaining open file, DTA_CACHER
C     will close down HDS and re-initialise the cache.
C
      CALL DTA_CACHER(UCNAME,LASTC,STATUS)
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
      END

