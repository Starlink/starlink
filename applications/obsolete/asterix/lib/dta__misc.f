*+DTA_COPYSLICEB   Copies a slice of one BYTE array to another and reorders
      SUBROUTINE DTA_COPYSLICEB(IDIM1, IDIM2, IDIM3, IDIM4,
     &                          DATA_IN, AMIN, AMAX, ORD, ODIM1,
     &                          ODIM2, ODIM3, ODIM4, DATA_OUT)
*    Description :
*      Takes a mapped input BYTE array of upto 4 dimensions and copies
*     a subset of it into another array. The input and output data are
*     taken to be 4 dimensional, and so DIM and ODIM must each contain
*     4 values. This routine also allows the order of the dimensions of
*     the output array to be changed for efficiency. This is determined
*     by the array ORD.
*        e.g. If         ORD(1)=3
*                        ORD(2)=1
*                        ORD(3)=2
*                        ORD(4)=4
*             then the output array would have the third dimension of
*            the old array as its most rapidly changing dimension.
*
*        N.B. The output dimensions ODIM must take into account the reordering
*            of the output data array.
*    Method :
*      At present this uses four DO loops
*      It would be neater to use DAT_SLICE, but this only works for upto
*      three dimensions at the moment.
*    Restrictions :
*      A maximum of four dimensional data may be used.
*    Author:
*         R.D.Saxton
*    History :
*     May 6 1988 original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Parameters :
      INTEGER MAXDIM
      PARAMETER (MAXDIM=4)
*    Import :
      INTEGER IDIM1,IDIM2,IDIM3,IDIM4          ! Dimensions of input array
      BYTE DATA_IN(IDIM1,IDIM2,IDIM3,IDIM4)    ! Input data array
      INTEGER AMIN(MAXDIM)                     ! Lower values for each dimension
      INTEGER AMAX(MAXDIM)                     ! Upper values for each dimension
      INTEGER ORD(MAXDIM)                      ! Order of Dims. of output aray
      INTEGER ODIM1,ODIM2,ODIM3,ODIM4          ! Dimensions of output aray
*    Import-Export :
*    Export :
      BYTE DATA_OUT(ODIM1,ODIM2,ODIM3,ODIM4)   ! Output data array
*    Local Constants :
*    Local variables :
      INTEGER  POS(MAXDIM)                     ! Position in output array
      INTEGER  LP1,LP2,LP3,LP4                 ! Variables for LP.
*    Data :
*-
* Fortran doesn't allow the use of array variables as LP counters
*  hence this inelegant code.
      DO LP4=AMIN(4),AMAX(4)
         POS(4)=LP4-AMIN(4)+1
*
        DO LP3=AMIN(3),AMAX(3)
           POS(3)=LP3-AMIN(3)+1
*
           DO LP2=AMIN(2),AMAX(2)
              POS(2)=LP2-AMIN(2)+1
*
              DO LP1=AMIN(1),AMAX(1)
                 POS(1)=LP1-AMIN(1)+1
*
* Write output data in correct order
*
                 DATA_OUT( POS(ORD(1)), POS(ORD(2)), POS(ORD(3)),
     &                                                POS(ORD(4)) )
     &                      =    DATA_IN( LP1,LP2,LP3,LP4 )
*
              ENDDO
*
           ENDDO
*
        ENDDO
*
      ENDDO
*
      END
*+DTA_COPYSLICER   Copies a slice of one real array to another and reorders
      SUBROUTINE DTA_COPYSLICER(IDIM1, IDIM2, IDIM3, IDIM4, DATA_IN,
     &                  AMIN, AMAX, ORD, ODIM1, ODIM2, ODIM3, ODIM4,
     &                                                      DATA_OUT)
*    Description :
*      Takes a real mapped input array of upto 4 dimensions and copies
*     a subset of it into another array. The input and output data are
*     taken to be 4 dimensional, and so DIM and ODIM must each contain
*     4 values. This routine also allows the order of the dimensions of
*     the output array to be changed for efficiency. This is determined
*     by the array ORD.
*        e.g. If         ORD(1)=3
*                        ORD(2)=1
*                        ORD(3)=2
*                        ORD(4)=4
*             then the output array would have the third dimension of
*            the old array as its most rapidly changing dimension.
*
*        N.B. The output dimensions ODIM must take into account the reordering
*            of the output data array.
*    Method :
*      At present this uses four DO loops
*      It would be neater to use DAT_SLICE, but this only works for upto
*      three dimensions at the moment.
*    Restrictions :
*      A maximum of four dimensional data may be used.
*    Author:
*         R.D.Saxton
*    History :
*     May 6 1988 original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Parameters :
      INTEGER MAXDIM
      PARAMETER (MAXDIM=4)
*    Import :
      INTEGER IDIM1, IDIM2, IDIM3, IDIM4       ! Dimensions of input array
      REAL DATA_IN(IDIM1,IDIM2,IDIM3,IDIM4)    ! Input data array
      INTEGER AMIN(MAXDIM)                     ! Lower values for each dimension
      INTEGER AMAX(MAXDIM)                     ! Upper values for each dimension
      INTEGER ORD(MAXDIM)                      ! Order of Dims. of output aray
      INTEGER ODIM1, ODIM2, ODIM3, ODIM4       ! Dimensions of output aray
*    Import-Export :
*    Export :
      REAL DATA_OUT(ODIM1,ODIM2,ODIM3,ODIM4)   ! Output data array
*    Local Constants :
*    Local variables :
      INTEGER  POS(MAXDIM)                     ! Position in output array
      INTEGER  LP1,LP2,LP3,LP4                 ! Variables for LP.
*    Data :
*-
* Fortran doesn't allow the use of array variables as LP counters
*  hence this inelegant code.
      DO LP4=AMIN(4),AMAX(4)
         POS(4)=LP4-AMIN(4)+1
*
        DO LP3=AMIN(3),AMAX(3)
           POS(3)=LP3-AMIN(3)+1
*
           DO LP2=AMIN(2),AMAX(2)
              POS(2)=LP2-AMIN(2)+1
*
              DO LP1=AMIN(1),AMAX(1)
                 POS(1)=LP1-AMIN(1)+1
*
* Write output data in correct order
*
                 DATA_OUT( POS(ORD(1)), POS(ORD(2)), POS(ORD(3)),
     &                                                POS(ORD(4)) )
     &                      =    DATA_IN( LP1,LP2,LP3,LP4 )
*
              ENDDO
*
           ENDDO
*
        ENDDO
*
      ENDDO
*
      END
*+  DTA_WRISPACED - Write components of a spaced array to an HDS object
      SUBROUTINE DTA_WRISPACED(LOC,ORIGIN,DIM,SCALE,BASE,STATUS)
* Description :
*        This routine writes a spaced array.
* Method :
*        The elements of a spaced array as defined by SGP38 are written
*      into the structrure.
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
* History :
*     10 Apr 88: original (LTVAD::RDS)
*     10 May 88: New header (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
* Import :
      CHARACTER*(DAT__SZLOC) LOC           !Locator of HDS file or structure
      INTEGER ORIGIN                       !Pixel no of first data point.
      INTEGER DIM                          !Number of elements in ARRAY
      REAL SCALE                           !Scale value for array
      REAL BASE                            !Value of 1st element
* Export :
* Status :
      INTEGER STATUS
* Function declarations :
* Local constants :
* Local variables :
      CHARACTER*(DAT__SZLOC) DLOC          !Locator of Data array structure
* Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Create data array structure
      CALL DAT_NEW(LOC,'DATA_ARRAY','ARRAY',0,0,STATUS)
      CALL DAT_FIND(LOC,'DATA_ARRAY',DLOC,STATUS)
*
* Write in elements of the spaced array
      CALL HDX_PUTC(DLOC,'VARIANT',1,'SPACED',STATUS)
      CALL HDX_PUTI(DLOC,'ORIGIN',1,ORIGIN,STATUS)
      CALL HDX_PUTI(DLOC,'DIMENSION',1,DIM,STATUS)
      CALL HDX_PUTR(DLOC,'BASE',1,BASE,STATUS)
      CALL HDX_PUTR(DLOC,'SCALE',1,SCALE,STATUS)
*
      CALL DAT_ANNUL(DLOC,STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from DTA_WRISPACED',STATUS)
      ENDIF
*
      END
*+DTA_WRITESLICEB   Copies a slice of one BYTE array to another and reorders
      SUBROUTINE DTA_WRITESLICEB(IDIM1,IDIM2,IDIM3,IDIM4,DATA_IN,
     &                           AMIN,AMAX,ORD,ODIM1,ODIM2,ODIM3,
     &                                             ODIM4,DATA_OUT)
*    Description :
*      Takes a mapped input BYTE array of 4 dimensions and copies
*     it into a slice of a larger array. The input and output data are
*     taken to be 4 dimensional, and so DIM and ODIM must each contain
*     4 values. This routine also allows the order of the dimensions of
*     the input array relative to the output array to be specified.
*    Method :
*      At present this uses four DO loops
*      It would be neater to use DAT_SLICE, but this only works for upto
*      three dimensions at the moment.
*    Restrictions :
*      A maximum of four dimensional data may be used.
*    Author:
*         R.D.Saxton
*    History :
*     May 6 1988 original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Parameters :
      INTEGER MAXDIM
      PARAMETER (MAXDIM=4)
*    Import :
      INTEGER IDIM1,IDIM2,IDIM3,IDIM4          ! Dimensions of input array
      BYTE DATA_IN(IDIM1,IDIM2,IDIM3,IDIM4)    ! Input data array
      INTEGER AMIN(MAXDIM)                     ! Lower values for each dimension
*                                                of the output array
      INTEGER AMAX(MAXDIM)                     ! Upper values for each dimension
      INTEGER ORD(MAXDIM)                      ! Order of Dims. of input aray
      INTEGER ODIM1,ODIM2,ODIM3,ODIM4          ! Dimensions of output aray
*    Import-Export :
      BYTE DATA_OUT(ODIM1,ODIM2,ODIM3,ODIM4)   ! Output data array
*    Export :
*    Local Constants :
*    Local variables :
      INTEGER  POS(MAXDIM)                     ! Position in input array
      INTEGER  LP1,LP2,LP3,LP4                 ! Variables for LP.
*    Data :
*-
      DO LP4=AMIN(4),AMAX(4)
         POS(4)=LP4-AMIN(4)+1
*
        DO LP3=AMIN(3),AMAX(3)
           POS(3)=LP3-AMIN(3)+1
*
           DO LP2=AMIN(2),AMAX(2)
              POS(2)=LP2-AMIN(2)+1
*
              DO LP1=AMIN(1),AMAX(1)
                 POS(1)=LP1-AMIN(1)+1
*
* Write output data in correct order
*
                 DATA_OUT( LP1,LP2,LP3,LP4 ) =
     &            DATA_IN( POS(ORD(1)), POS(ORD(2)), POS(ORD(3)),
     &                                                POS(ORD(4)) )
*
              ENDDO
*
           ENDDO
*
        ENDDO
*
      ENDDO
*
      END
*+DTA_WRITESLICER   Copies a slice of one real array to another and reorders
      SUBROUTINE DTA_WRITESLICER(IDIM1, IDIM2, IDIM3, IDIM4,
     &                           DATA_IN, AMIN, AMAX, ORD, ODIM1,
     &                           ODIM2, ODIM3, ODIM4, DATA_OUT)
*    Description :
*      Takes a REAL mapped input array of 4 dimensions and copies
*     it into a slice of a larger array. The input and output data are
*     taken to be 4 dimensional, and so DIM and ODIM must each contain
*     4 values. This routine also allows the order of the dimensions of
*     the input array relative to the output array to be specified.
*    Method :
*      At present this uses four DO loops
*      It would be neater to use DAT_SLICE, but this only works for upto
*      three dimensions at the moment.
*    Restrictions :
*      A maximum of four dimensional data may be used.
*    Author:
*         R.D.Saxton
*    History :
*     May 6 1988 original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Parameters :
      INTEGER MAXDIM
      PARAMETER (MAXDIM=4)
*    Import :
      INTEGER IDIM1, IDIM2, IDIM3, IDIM4       ! Dimensions of input array
      REAL DATA_IN(IDIM1, IDIM2, IDIM3, IDIM4) ! Input data array
      INTEGER AMIN(MAXDIM)                     ! Lower values for each dimension
*                                                of the output array
      INTEGER AMAX(MAXDIM)                     ! Upper values for each dimension
      INTEGER ORD(MAXDIM)                      ! Order of Dims. of input aray
      INTEGER ODIM1,ODIM2,ODIM3,ODIM4          ! Dimensions of output aray
*    Import-Export :
      REAL DATA_OUT(ODIM1,ODIM2,ODIM3,ODIM4)   ! Output data array
*    Export :
*    Local Constants :
*    Local variables :
      INTEGER  POS(MAXDIM)                     ! Position in input array
      INTEGER  LP1,LP2,LP3,LP4                 ! Variables for LP.
*    Data :
*-
      DO LP4=AMIN(4),AMAX(4)
         POS(4)=LP4-AMIN(4)+1
*
        DO LP3=AMIN(3),AMAX(3)
           POS(3)=LP3-AMIN(3)+1
*
           DO LP2=AMIN(2),AMAX(2)
              POS(2)=LP2-AMIN(2)+1
*
              DO LP1=AMIN(1),AMAX(1)
                 POS(1)=LP1-AMIN(1)+1
*
* Write output data in correct order
*
                 DATA_OUT( LP1,LP2,LP3,LP4 ) =
     &            DATA_IN( POS(ORD(1)), POS(ORD(2)), POS(ORD(3)),
     &                                                POS(ORD(4)) )
*
              ENDDO
*
           ENDDO
*
        ENDDO
*
      ENDDO
*
      END
