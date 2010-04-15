C+
      SUBROUTINE QUAL2FLAG
C
C                              QUAL2FLAG
C
C  Description:
C     This is a Figaro program that removes the quality array from a
C     Figaro data file. If the quality array indicated that any of
C     the data array elements were bad, then those elements of the
C     main data array are set to the 'flagged' or 'magic' value. Note
C     that this replaces the previous value in the data array, which is
C     therefore left - so this process can remove information from the
C     file, which is why Figaro tends to prefer the use of quality
C     arrays rather than flagged data arrays.
C
C  Command parameters:
C
C     INPUT  (Character) The name of the structure containing the data.
C
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for INPUT.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C-
C  History:
C     12th Feb 1995  KS / AAO.  Original version.
C     27th Nov 1995  KS / AAO.  Added call to DSA_QUALITY_AND_FLAGS_OK.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_LEN
      CHARACTER ICH_CI*16
C
C     Local variables
C
      INTEGER   DIMS(10)    ! The sizes of the data's dimensions
      REAL      FBAD        ! Flag value for 'FLOAT' data
      INTEGER   IGNORE      ! Used for status we don't care about
      INTEGER   NDIM        ! The number of dimensions in the data
      INTEGER   NELM        ! The total number of elements in the data
      INTEGER   NFLAGGED    ! Total number of flagged values on exit
      INTEGER   NFWERE      ! Original number of flagged values
      INTEGER   NQUAL       ! Number of non-zero quality values
      CHARACTER NUMBER*16   ! Used to format an integer
      INTEGER   OUTELM      ! Dynamic memory pointer to output data
      INTEGER   QPTR        ! Dynamic pointer to output quality array
      INTEGER   QSLOT       ! Slot number used to map quality array
      LOGICAL   QUAL        ! True if image has a quality array
      INTEGER   SLOT        ! Slot number used for mapping
      INTEGER   STATUS      ! Running status for DSA_ routines
C
C     Initial values
C
      QUAL=.FALSE.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Indicate that handling both quality and flagged values is OK,
C     although it's unusual for a Figaro routine.
C
      CALL DSA_QUALITY_AND_FLAGS_OK
C
C     Get input name
C
      CALL DSA_INPUT ('INPUT','INPUT',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('INPUT',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get output structure name and open it, copying input structure
C     into it if the two are not the same.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','INPUT',0,0,STATUS)
C
C     If the main data array had no quality array anyway, there's nothing for
C     us to do.
C
      CALL DSA_SEEK_QUALITY ('OUTPUT',QUAL,STATUS)
      IF (STATUS.NE.0) GO TO 500  ! Error exit
      IF (.NOT.QUAL) THEN
         CALL PAR_WRUSER (
     :         'Input data had no associated quality array',IGNORE)
         GO TO 500                ! Exit now, all done
      END IF
C
C     We indicate our intention to use both quality and flagged values
C     in handling the data - this ensures that we get both arrays quite
C     unchanged by the DSA routines (apart from any necessary type
C     conversion).
C
      CALL DSA_USE_QUALITY ('OUTPUT',STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('OUTPUT',STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FBAD,STATUS)
C
C     Map main output data array.
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OUTELM,SLOT,STATUS)
C
C     Map the data quality array
C
      CALL DSA_MAP_QUALITY('OUTPUT','READ','BYTE',QPTR,QSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Flag those data elements indicated by the quality array.
C
      CALL QUAL2FLAG_DO (%VAL(CNF_PVAL(OUTELM)),NELM,
     :                   %VAL(CNF_PVAL(QPTR)),FBAD,NQUAL,NFWERE,
     :                   NFLAGGED)
C
C     Report on what happened
C
      IF (NFWERE.GT.0) THEN
         NUMBER=ICH_CI(NFWERE)
         CALL PAR_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :     ' data array elements were already flagged.',IGNORE)
      END IF
      IF (NQUAL.EQ.0) THEN
         CALL PAR_WRUSER(
     :      'Quality array showed all elements as good.'//
     :                        ' No new elements flagged.',IGNORE)
      ELSE
         NUMBER=ICH_CI(NQUAL)
         CALL PAR_WRUSER('Quality array showed '//
     :      NUMBER(:ICH_LEN(NUMBER))//' data elements as bad.',IGNORE)
         IF (NFWERE.EQ.NFLAGGED) THEN
            CALL PAR_WRUSER('All of these were already flagged.'//
     :                       ' No new elements flagged.',IGNORE)
         ELSE
            NUMBER=ICH_CI(NFLAGGED-NFWERE)
            CALL PAR_WRUSER(NUMBER(:ICH_LEN(NUMBER))//
     :         ' of these were not already flagged, and '//
     :                         'have now been flagged.',IGNORE)
         END IF
      END IF
C
C     Set the file to indicate it has flagged values if indeed it has.
C
      IF (NFLAGGED.GT.0) THEN
         NUMBER=ICH_CI(NFLAGGED)
         CALL PAR_WRUSER('Data array now has '//
     :       NUMBER(:ICH_LEN(NUMBER))//' data elements flagged',IGNORE)
         CALL DSA_SET_FLAGGED_VALUES ('OUTPUT',.TRUE.,STATUS)
      ELSE
         CALL DSA_SET_FLAGGED_VALUES ('OUTPUT',.FALSE.,STATUS)
      END IF
C
C     In any case, we no longer need the quality array. Close down all
C     the mappings and delete it.
C
      CALL DSA_UNMAP(SLOT,STATUS)
      CALL DSA_UNMAP(QSLOT,STATUS)
      CALL DSA_DELETE_QUALITY('OUTPUT',STATUS)
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END


      SUBROUTINE QUAL2FLAG_DO (ARRAY,NELM,FLAGS,FBAD,
     :                                      NQUAL,NFWERE,NFLAGGED)
C
C     Q U A L 2 F L A G _ D O
C
C     Utility routine for QUAL2FLAG. This is the routine that actually
C     sets the flagged values in the NELM elements of ARRAY. FBAD
C     is the flag value the routine used. If an element of FLAGS is
C     non-zero the corresponding element of ARARY is set to FBAD.
C     NQUAL is the number of elements of FLAGS that were non-zero.
C     NFWERE is the number of elements of the data arrray that were
C     originally flagged, and NFLAGGED is the number of data array
C     elements that were flagged when this routine returns - note that
C     some data elements with corresponding FLAGS elements set may have
C     originally been flagged.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM,NQUAL,NFWERE,NFLAGGED
      REAL ARRAY(NELM),FBAD
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER I                 ! Loop index through all elements of array
C
      NFLAGGED=0
      NQUAL=0
      NFWERE=0
      DO I=1,NELM
         IF (ARRAY(I).EQ.FBAD) NFWERE=NFWERE+1
         IF (FLAGS(I).NE.0) THEN
            NQUAL=NQUAL+1
            IF (ARRAY(I).NE.FBAD) THEN
               ARRAY(I)=FBAD
               NFLAGGED=NFLAGGED+1
            END IF
         END IF
      END DO
      NFLAGGED=NFLAGGED+NFWERE
C
      END
