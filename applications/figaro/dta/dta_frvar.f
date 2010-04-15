C+
      SUBROUTINE DTA_FRVAR (NAME,STATUS)
C
C     D T A _ F R V A R
C
C     Used to 'unmap' a data object.  Any workspace used by
C     the system for the mapping of the object is released.
C     If conversion was necessary to map the object in the
C     required form then there will be an intermediate
C     form of the object in that form.  This will be converted
C     back into the form used by the data system and used to
C     update the actual data held in the data system.  If the
C     data had been mapped directly onto the data system files
C     then this will not be necessary.
C
C     Parameters -    (">" input, "<" output,  "!" modified)
C
C     (>) NAME      (Character) The name of the data system
C                   object, exactly as used by the corresponding
C                   call to the mapping routine. Case is not
C                   significant.
C     (<) STATUS    (Integer) Returns a status code
C                   0 => OK
C                   DTA_NOTMAP => Object was not mapped.
C                   Lower level routines may return other error codes.
C-
C     Common variables used -
C
C     (!) MUSED    Flags for mapping information blocks
C     (!) MUNAME   Names of mapped objects
C     (!) MULOCS   HDS locators for mapped objects
C
C     all in blocks MUINFO and MUCHAR
C
C     Subroutines / functions used -
C
C     DAT_ANNUL    (HDS_ package) Annul an HDS locator
C     DTA_HDSERC   (DTA_    "   ) Convert an HDS error to a DTA code
C     DTA_TRNAME   ( "      "   ) Fold name to upper case and tidy up
C     EMS_BEGIN    (EMS_  "     ) Start a new reporting environment.
C     EMS_ANNUL    ( "    "     ) Clear current EMS error status.
C     EMS_END      ( "    "     ) End current reporting environment.
C
C                                         KS / CIT  16th March 1983
C     Modified:
C
C     23rd Sept 1985  KS / AAO.  Now uses DTA_FMTCON instead of STL_FMTCON
C     28th March 1986 KS / AAO.  Re-written for the HDS based version
C                     of the DTA- routines.
C     10th Jan  1992. KS / AAO.  Syntax of include statements changed to
C                     remove VMS logical names and to use lower case, to
C                     enable compilation on a SUN.
C     24th Jan 1992.  KS / AAO. Calls to EMS added to control error reporting.
C                     Setting STATUS=0 before a call to HDS replaced by an
C                     explicit annulling of the status.
C     12th Apr 1993.  HME / UoE, Starlink. For DTAMAP must include SAE_PAR
C                     and DAT_PAR as well.
C+
      IMPLICIT NONE
C
C     Parameters -
C
      CHARACTER*(*) NAME
      INTEGER STATUS
C
C     Error codes -
C
      INCLUDE 'DTACODES'
C
C     Mapped data common block
C
C     MUMAX   maximum number of mapped data objects
C
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'DTAMAP'
C
C     Local variables
C
      INTEGER EMSTAT,I
      CHARACTER FNAME*64
C
C     Set new EMS reporting environment
C
      EMSTAT=0
      CALL EMS_BEGIN(EMSTAT)
C
C     Convert name to upper case
C
      CALL DTA_TRNAME(NAME,FNAME)
C
C     Look for the object in the mapped tables
C
      DO I=1,MUMAX
         IF (MUSED(I)) THEN
            IF (MUNAME(I).EQ.FNAME) THEN
C
C              Found the object.  Clear the used flag, then
C              annul the locator used.  This will have the effect
C              of unmapping the data and performing any necessary
C              conversions.
C
               MUSED(I)=.FALSE.
               CALL EMS_ANNUL(STATUS)
               CALL DAT_ANNUL(MULOCS(I),STATUS)
               IF (STATUS.NE.0) CALL DTA_HDSERC(STATUS)
C
C              Once object found, break out of loop
C
               GO TO 600
            END IF
         END IF
      END DO
C
C     If loop falls through, object wasn't found
C
      STATUS=DTA_NOTMAP
C
C     On way out, clear any EMS errors and revert to previous environment.
C
  600 CONTINUE
      EMSTAT=0
      CALL EMS_ANNUL(EMSTAT)
      CALL EMS_END(EMSTAT)
C
      END

