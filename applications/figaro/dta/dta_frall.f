C+
      SUBROUTINE DTA_FRALL (NAME,STATUS)
C
C     D T A _ F R A L L
C
C     Unmaps all objects in a named structure.  This routine is
C     intended for use by DTA_FCLOSE, rather than for general use,
C     being in the nature of a general tidying up routine.
C
C     Parameters -    (">" input, "<" output,  "!" modified)
C
C     (>) NAME      (Character) The name of the data system
C                   structure. Case is not significant.  Any
C                   data objects in this structure will be unmapped.
C                   If NAME is a single object, this will be
C                   unmapped, although DTA_FCLOSE should really be
C                   used for this purpose.
C     (<) STATUS    (Integer) Returns a status code
C                   0 => OK
C                   Lower level routines may return other error codes.
C                   Note that this routine does not return error status
C                   if no mapped objects match NAME.
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
C     EMS_ANNUL    (EMS_  "     ) Clear current EMS error status.
C
C                                         KS / AAO 26th May 1986
C     Modified:
C
C     8th  Jan 1992.  KS / AAO. Syntax of include file names changed to allow
C                     routine to compile on a SUN.
C     24th Jan 1992.  KS / AAO. With new HDS, setting STATUS=0 before a call
C                     to an HDS routine may not be enough. Replaced by
C                     CALL EMS_ANNUL(STATUS).
C     12th Apr 1993.  HME / UoE, Starlink. For DTAMAP must include SAE_PAR
C                     and DAT_PAR as well.
C
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
      LOGICAL VALID
      INTEGER I, LENGTH
      CHARACTER FNAME*64
C
C     Convert name to upper case
C
      CALL DTA_TRNAME(NAME,FNAME)
      LENGTH=INDEX(FNAME//' ',' ')-1
C
C     Look for any object in the mapped tables that matches the
C     name.
C
      DO I=1,MUMAX
         IF (MUSED(I)) THEN
            IF (MUNAME(I)(:LENGTH).EQ.FNAME) THEN
C
C              Found an object.  Clear the used flag, then
C              annul the locator used.  This will have the effect
C              of unmapping the data and performing any necessary
C              conversions.
C
               MUSED(I)=.FALSE.
               CALL EMS_ANNUL(STATUS)
               CALL DAT_VALID(MULOCS(I),VALID,STATUS)
               IF (VALID) THEN
                  CALL DAT_ANNUL(MULOCS(I),STATUS)
                  IF (STATUS.NE.0) THEN
                     CALL DTA_HDSERC(STATUS)
                     GO TO 600
                  END IF
               END IF
            END IF
         END IF
      END DO
C
C     Exit
C
  600 CONTINUE
C
      END

