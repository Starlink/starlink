C+
C                   D S A _ _ D E L E T E _ A X I S
C
C  Routine name:
C     DSA__DELETE_AXIS
C
C  Function:
C     Deletes a complete axis substructure from a data structure.
C
C  Description:
C    Given the reference slot number in the DSA common tables for an
C    already open structure, this routine completely deletes a specified
C    axis substructure from the open structure.  No error messages are
C    output if this fails (there may not even be such an axis substructure)
C    and the DTA error code from the delete call is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA__DELETE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) REF_SLOT       (Integer,ref) The number of the reference slot
C                        for the structure in the internal common tables.
C     (>) AXIS           (Integer,ref) The number of the axis in question.
C     (<) DTA_STATUS     (Integer,ref) The DTA status code from the delete
C                        operation.
C
C  External variables used:
C     Only common variables internal to the DSA routines.
C
C  External subroutines / functions used:  DTA_DLVAR.
C
C  Prior requirements:
C     The structure must have been opened already and REF_SLOT must
C     be valid.  AXIS should not be more than the maximum allowed by
C     the DSA routines.  None of this is tested by this routine.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DTA_DLVAR         Deletes a data object.
C
C  Common variable details:
C     (>) OBJ_LEN       (Integer array) Number of chars in each OBJ_NAME.
C     (>) OBJ_NAMES     (String array) Name (as recognised by DTA_) of data
C                       object corresponding to reference name.
C     (>) NDF_FORMAT    (Logical array) Indicates structure format is Starlink's
C                       NDF format (described in SGP38).  If false, format is
C                       original Figaro format (DST files).
C  History:
C     11th Dec  1989.   Original version.  KS / AAO.
C     7th March 1990.   Support for NDF format added.  KS/AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C
C  Note:
C     This version supports both the original Figaro data structures
C     and Starlink's NDF format.
C+
      SUBROUTINE DSA__DELETE_AXIS (REF_SLOT,AXIS,DTA_STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER REF_SLOT, AXIS, DTA_STATUS
C
C     DSA system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER AXIS_STRUCT*32        ! DTA name of axis structure
      INTEGER   DIMS(10)              ! Dimensions of axis structure
      INTEGER   I                     ! Loop axis through dimensions
      INTEGER   IMAX                  ! Highest non-empty axis structure
      INTEGER   LENGTH                ! Number of chars in AXIS_STRUCT
      CHARACTER NAME*16               ! Name of item in axis structure
      INTEGER   NDIM                  ! Number of axis structure dimensions
      INTEGER   NM_STATUS             ! Status returned by DTA_NMVAR
C
C     Names of axis data structures in the original Figaro format.
C
      CHARACTER AXIS_NAMES*6
      DATA AXIS_NAMES/'XYTUVW'/
C
      IF (NDF_FORMAT(REF_SLOT)) THEN
C
C        NDF format has the axis data as an array of structures, .AXIS[ndim].
C        First, generate the name of the axis structure.
C
         AXIS_STRUCT=OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.AXIS['//
     :                                        CHAR(ICHAR('0')+AXIS)//']'
         LENGTH=OBJ_LEN(REF_SLOT)+8
C
C        Get the size of the axis structure itself (name without [n]).
C        If it only has one element and this is it, we can delete the
C        whole thing in one go.  If we can't get the size at all, it
C        doesn't exist.
C
         CALL DTA_SZVAR (AXIS_STRUCT(:LENGTH-3),10,NDIM,DIMS,DTA_STATUS)
         IF (DTA_STATUS.EQ.0) THEN
            IF ((DIMS(1).EQ.1).AND.(AXIS.EQ.1)) THEN
               CALL DTA_DLVAR (AXIS_STRUCT(:LENGTH-3),DTA_STATUS)
            ELSE
C
C              This is doing it the hard way.  It's not the only element
C              of the structure, so we have to delete everything in it
C              one by one in order to clear it out.
C
               NM_STATUS=0
               DO WHILE (NM_STATUS.EQ.0)
                  CALL DTA_NMVAR (AXIS_STRUCT,1,NAME,NM_STATUS)
                  IF (NM_STATUS.EQ.0) THEN
                     CALL DTA_DLVAR (AXIS_STRUCT(:LENGTH)//'.'//NAME,
     :                                                      DTA_STATUS)
                     IF (DTA_STATUS.NE.0) GO TO 500   ! Error exit
                  END IF
               END DO
C
C              Now, if it was the highest element in the structure,
C              we can resize the structure to loose it completely.
C              We can even be cleverer than that, and trim off any
C              high numbered elements that turn out to be empty.
C
               IMAX=0
               DO I=DIMS(1),1,-1
                  AXIS_STRUCT(LENGTH-1:LENGTH-1)=CHAR(ICHAR('0')+I)
                  CALL DTA_NMVAR (AXIS_STRUCT,1,NAME,DTA_STATUS)
                  IF (DTA_STATUS.EQ.0) THEN
                     IMAX=I
                     GO TO 320      ! Element not empty, break out of I loop
                  END IF
               END DO
  320          CONTINUE
               DTA_STATUS=0
               IF (IMAX.EQ.0) THEN
                  CALL DTA_DLVAR (AXIS_STRUCT(:LENGTH-3),DTA_STATUS)
               ELSE IF (IMAX.LT.DIMS(1)) THEN
                  CALL DTA_RNVAR (AXIS_STRUCT(:LENGTH-3),
     :                                 AXIS_STRUCT(:LENGTH),DTA_STATUS)
               END IF
            END IF
         END IF
      ELSE
C
C        Original Figaro format has each axis in a separate structure,
C        which can be deleted all in one go.
C
         CALL DTA_DLVAR(OBJ_NAMES(REF_SLOT)(:OBJ_LEN(REF_SLOT))//'.'//
     :                               AXIS_NAMES(AXIS:AXIS),DTA_STATUS)
      END IF
C
C     Exit
C
  500 CONTINUE
C
      END
