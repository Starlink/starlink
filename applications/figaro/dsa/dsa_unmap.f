C+
C                         D S A _ U N M A P
C
C  Routine name:
C     DSA_UNMAP
C
C  Function:
C     Unmaps a previously mapped array
C
C  Description:
C     DSA_UNMAP can be called to explicitly unmap a data array previously
C     mapped by one of the DSA_ mapping routines.  This is usually not
C     necessary, since DSA_CLOSE will reverse the effect of all mappings.
C     Since it is possible for arrays to be multiply mapped, the effect of
C     this routine is not necessarily to completely close down a mapped
C     array; all it really does is cancel a specific mapping request.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_UNMAP (SLOT,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) SLOT     (Integer,ref) The handle value allocated when the
C                  array was originally mapped.
C     (!) STATUS   (Integer,ref) Status value.  If bad status is passed,
C                  this routine returns immediately.
C
C  External subroutines / functions used:
C     CNF_PVAL, ICH_CI, ICH_LEN, DSA_WRUSER, DSA_WRNAME, DSA_FMTCON, DSA_STRUCTC
C     DSA_FREE_WORKSPACE, DSA_REFLAG_DATA, DSA_VARIANCE_TO_ERR,
C     DSA_ERR_TO_VARIANCE, DSA_WRFLUSH
C
C  Prior requirements:
C     DSA_OPEN must have been called to initialise the system.
C
C  Version date: 2nd July 1993
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C           Malcolm J. Currie, Starlink
C-
C  Subroutine / function details:
C     CNF_PVAL           Full pointer to dynamically allocated memory
C     DSA_WRUSER         Output message to user
C     DSA_WRFLUSH        Flush output message buffer to user
C     DSA_WRNAME         Output structure name to user
C     DSA_STRUCTC        Rewrite array into a contracted structure
C     DSA_FREE_WORKSPACE Release work array
C     DSA_REFLAG_DATA    Reset flagged data values removed by DSA_UNFLAG_DATA
C     DSA_VARIANCE_TO_ERR Convert a variance array to an error array
C     DSA_ERR_TO_VARIANCE Convert an error array to a variance array
C     DSA_FMTCON         General array type conversion routine
C     ICH_LEN            Position of last non-blank char in string
C     ICH_CI             Formats an integer into a string
C
C  History:
C     8th July 1987   Original version.  KS / AAO.
C     20th July 1988  Reflagging of unflagged data added, and type
C                     conversion now done through CNV_FMTCON.  KS / AAO.
C     28th June 1989  Check for variance/error processing added.  KS/AAO.
C     8th  Sept 1989  Control of flag value propagation added.  KS/AAO.
C     12th Mar  1990  Support for variance arrays mapped as uncertainty arrays
C                     added.  KS/AAO.
C     27th Apr  1990  Support for some types of SGP38 structured arrays
C                     added. MAP_STRUCT changed to MAP_SCALED. KS/AAO.
C     21st Aug  1992  Replace CNV_ with DSA_FMTCON, which calls VEC_
C                     routines.  HME / UoE, Starlink.
C     31st Aug  1992  Added use of DSA_WRFLUSH. KS/AAO
C      1st Sep  1992  Usused variable declarations removed. KS/AAO.
C     24th Feb  1993  Arrays are now passed to DSA_FMTCON as arrays instead
C                     of by pointer. KS/AAO.
C      2nd Jul  1993  Now closes down any workspace used to hold flagged
C                     data information. KS/AAO.
C     2005 May 31     Use CNF_PVAL for pointers to mapped data. MJC
C     2005 June 3     Replace DYNAMIC_MEMORY with %VAL(CNF_PVAL(ADDRESS))
C                     contruct for 64-bit addressing.  MJC / Starlink
C
C  Note:
C     This version can handle the following types of SGP38 structured
C     arrays: SIMPLE, COMPLEX (when it operates only on the .REAL array)
C     SCALED.  It cannot handle arrays with origins other than 1.0.  It
C     can also handle the original Figaro scaled type 'CSTRUCT' (which
C     DSA__STRUCT_ARRAY treats as a 'SCALED' array)
C+
      SUBROUTINE DSA_UNMAP (SLOT,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER SLOT, STATUS
C
C     Functions used
C
      INTEGER ICH_LEN
      CHARACTER ICH_CI*8
C
C     DSA_ system common definition
C
      INCLUDE 'DSA_COMMON'
C
C     DSA_ system error codes
C
      INCLUDE 'DSA_ERRORS'
C
C     Local variables
C
      INTEGER   ERR_ADDR               ! Address of error data array
      INTEGER   FLAGS_ADDR             ! Address of flag data array
      INTEGER   I                      ! Loop index
      INTEGER   MAP_SLOT               ! Map table slot for base array
      INTEGER   NBAD                   ! Number of conversion errors
      INTEGER   NELM                   ! Number of elements in mapped array
      CHARACTER NUMBER*8               ! Used to format numbers
      INTEGER   OBJ_TYPE_CODE          ! Integer code for base array type
      CHARACTER TYPE*16                ! Type of mapped array
      INTEGER   TYPE_CODE              ! Integer code for workspace type
      LOGICAL   VALID                  ! Indicates map slot # valid & in use
      INTEGER   VAR_ADDR               ! Address of variance array
      INTEGER   WORK_SLOT              ! Work table slot used for mapping
C
C     DSA_ system type definitions.  Defines MAX_TYPES, TYPE_NAMES,
C                                    TYPE_SIZE, FMTCON_CODE
C
      INCLUDE 'DSA_TYPES'
C
C     Return immediately if bad status passed
C
      IF (STATUS.NE.0) RETURN
C
C     Make sure the slot is in fact in use
C
      VALID=(SLOT.GE.1).AND.(SLOT.LE.MAX_MAP_CALLS)
      IF (VALID) VALID=MAP_CALL_USED(SLOT)
      IF (.NOT.VALID) THEN
         CALL DSA_WRUSER (
     :       'Attempt to release an invalid or unused map call slot (#')
         NUMBER=ICH_CI(SLOT)
         CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
         CALL DSA_WRUSER('). Probable programming error.')
         CALL DSA_WRFLUSH
         STATUS=DSA__MPSLNV
         GO TO 500             ! Error exit
      END IF
C
C     Get the map table entry corresponding to this map call.
C
      MAP_SLOT=MAP_CALL_SLOT(SLOT)
C
C     See if what has been mapped is a variance array that was generated
C     by processing an error array.  If so, and if the mapping was for
C     write or update, the first thing we have to do is reverse the
C     processing.  Similarly, if it was an error array generated by
C     processing an error array (the sign of the work slot tells us which
C     processing was performed).
C
      IF ((MAP_CALL_VSLOT(SLOT).NE.0).AND.
     :          (MAP_CALL_MODE(SLOT).NE.'R')) THEN
         IF (MAP_CALL_VSLOT(SLOT).LT.0) THEN
C
C           This is a variance array mapped as an error array.
C
            MAP_CALL_VSLOT(SLOT)=-MAP_CALL_VSLOT(SLOT)
            ERR_ADDR=WORK_POINTER(MAP_CALL_VSLOT(SLOT))
            NELM=MAP_SIZE(MAP_SLOT)
            IF (MAP_CALL_WORK(SLOT).NE.0) THEN
               TYPE=WORK_TYPE(MAP_CALL_WORK(SLOT))
               VAR_ADDR=WORK_POINTER(MAP_CALL_WORK(SLOT))
            ELSE
               TYPE=MAP_TYPE(MAP_SLOT)
               VAR_ADDR=MAP_POINTER(MAP_SLOT)
            END IF
            CALL DSA_ERR_TO_VARIANCE (NELM,TYPE,ERR_ADDR,VAR_ADDR,NBAD)
         ELSE
C
C           This is an error array mapped as a variance array.
C
            VAR_ADDR=WORK_POINTER(MAP_CALL_VSLOT(SLOT))
            NELM=MAP_SIZE(MAP_SLOT)
            IF (MAP_CALL_WORK(SLOT).NE.0) THEN
               TYPE=WORK_TYPE(MAP_CALL_WORK(SLOT))
               ERR_ADDR=WORK_POINTER(MAP_CALL_WORK(SLOT))
            ELSE
               TYPE=MAP_TYPE(MAP_SLOT)
               ERR_ADDR=MAP_POINTER(MAP_SLOT)
            END IF
            CALL DSA_VARIANCE_TO_ERR (NELM,TYPE,VAR_ADDR,ERR_ADDR,NBAD)
         END IF
C
         IF (NBAD.NE.0) THEN
            CALL DSA_WRUSER('Note: ')
            NUMBER=ICH_CI(NBAD)
            CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
            CALL DSA_WRUSER(' numeric error(s) occurred converting ')
            CALL DSA_WRNAME(MAP_ACTNAM(MAP_SLOT))
            CALL DSA_WRUSER(' between a variance and a standard '//
     :                                     'deviation error array.')
            CALL DSA_WRFLUSH
         END IF
         CALL DSA_FREE_WORKSPACE (MAP_CALL_VSLOT(SLOT),STATUS)
      END IF
C
C     This routine doesn't actually free any mapped arrays, or any
C     workspace versions of arrays, on the principle that they might
C     be wanted again and it's better to use the memory and keep them
C     than to have to take the overhead of having to map and/or
C     convert them again.  What it does do is force an update of
C     the base array from any workspace arrays mapped for write/update,
C     and decrements the various reference counters so that an (as
C     yet unwritten) garbage-collector might be able to recover unused
C     space if necessary.  Until then, we just assume that DSA_CLOSE
C     will release everything in one go.
C
C     See if the mapping had in fact been to a workspace copy of the data.
C
      WORK_SLOT=MAP_CALL_WORK(SLOT)
      IF (WORK_SLOT.NE.0) THEN
C
C        If the mapping was readonly, we don't have to do very much.
C        There are some 'update' mappings which do not have a real
C        (ie on disk) array associated with them - quality arrays
C        which are being handled using flagged data.  We don't
C        update them either - how can we?
C
         IF ((MAP_CALL_MODE(SLOT).NE.'R').AND.(MAP_SLOT.GT.0)) THEN
C
C           However, if it was write/update, we update the main array
C           from the workspace version.  This has two stages.  First,
C           we may have to reflag data in the array, then we need to
C           do the actual update.  However, the very first thing we
C           have to do is to sort out the type code (for FMTCON) and
C           the number of elements involved.  (It might be an idea to
C           have saved these in common, actually.)
C
            DO I=1,MAX_TYPES
               IF (WORK_TYPE(WORK_SLOT).EQ.TYPE_NAMES(I)) THEN
                  TYPE_CODE=I
                  GO TO 360       ! Break out of I loop
               END IF
            END DO
  360       CONTINUE
            NELM=WORK_BYTES(WORK_SLOT)/TYPE_SIZE(TYPE_CODE)
            OBJ_TYPE_CODE=MAP_CODE(MAP_SLOT)
C
C           Now, has the data been unflagged?
C
            IF (MAP_CALL_FSLOT(SLOT).NE.0) THEN
C
C              Yes, it has been unflagged.  So we need to reflag it.
C
               FLAGS_ADDR=WORK_POINTER(MAP_CALL_FSLOT(SLOT))
               CALL DSA_REFLAG_DATA(NELM,WORK_TYPE(WORK_SLOT),
     :                  %VAL( CNF_PVAL(WORK_POINTER(WORK_SLOT) ) ),
     :                  %VAL( CNF_PVAL(FLAGS_ADDR) ), STATUS)
            END IF
C
C           Now we can get on with updating the base data array.
C           For a primitive array, all we have to do is a format
C           conversion.
C
            IF (.NOT.MAP_SCALED(MAP_SLOT)) THEN
               CALL DSA_FMTCON(WORK_PROP(WORK_SLOT),
     :            FMTCON_CODE(TYPE_CODE),FMTCON_CODE(OBJ_TYPE_CODE),
     :            %VAL(CNF_PVAL(WORK_POINTER(WORK_SLOT))),
     :            %VAL(CNF_PVAL(MAP_POINTER(MAP_SLOT))),
     :            NELM,NBAD)
               IF (NBAD.NE.0) THEN
                  CALL DSA_WRUSER('Note: ')
                  NUMBER=ICH_CI(NBAD)
                  CALL DSA_WRUSER(NUMBER(:ICH_LEN(NUMBER)))
                  CALL DSA_WRUSER(
     :                        ' numeric error(s) occurred converting ')
                  CALL DSA_WRNAME(MAP_ACTNAM(MAP_SLOT))
                  CALL DSA_WRUSER(' back from ')
                  CALL DSA_WRUSER(WORK_TYPE(WORK_SLOT)
     :                                (:ICH_LEN(WORK_TYPE(WORK_SLOT))))
                  CALL DSA_WRUSER(' to ')
                  CALL DSA_WRUSER(MAP_TYPE(MAP_SLOT)
     :                                  (:ICH_LEN(MAP_TYPE(MAP_SLOT))))
                  CALL DSA_WRUSER('.')
                  CALL DSA_WRFLUSH
               END IF
            ELSE
C
C              Here, we deal with the possibility of a scaled array.
C              We assume this is always based on a short array (something
C              DSA_MAP_ARRAY should have made sure of).
C
               CALL DSA_STRUCTC (MAP_NAMES(MAP_SLOT),NELM,OBJ_TYPE_CODE,
     :                                  MAP_POINTER(MAP_SLOT),TYPE_CODE,
     :                                   WORK_POINTER(WORK_SLOT),STATUS)
            END IF
         END IF
      ELSE
C
C        If the mapping was directly to the base array, then any changes
C        will already be reflected in it.  The only exception would be if
C        the array had been unflagged, but DSA_MAP_ARRAY always unflags
C        into work arrays.  So we don't have to do anything here.
C
      END IF
C
C     If workspace had been allocated to record flagged values, release
C     that at this point.
C
      IF (MAP_CALL_FSLOT(SLOT).NE.0) THEN
         CALL DSA_FREE_WORKSPACE(MAP_CALL_FSLOT(SLOT),STATUS)
      END IF
C
C     Close down the map call slot, and decrement the reference counter
C     for the basic mapped object.
C
      MAP_CALL_USED(SLOT)=.FALSE.
      MAP_COUNT(MAP_SLOT)=MAP_COUNT(MAP_SLOT)-1
C
C     Exit
C
  500 CONTINUE
C
      END
