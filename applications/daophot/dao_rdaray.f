**==rdaray.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************

      SUBROUTINE RDARAY(ENVIRO,LX,LY,MX,MY,NX,FUNC,IFLAG)

*+
*  Name :
*     RDARAY
*
*  Purpose :
*     Read a rectangular sub-array from an NDF's DATA_ARRAY
*
*  Invocation :
*     CALL RDARAY( ENVIRO, LX, LY, MX, MY, NX, FUNC, IFLAG )
*
*  Description :
*     This is a re-written version of the original Daophot RDARAY
*     routine which accessed Figaro-style data structures.  This version
*     uses NDF data files. The present version is written to the
*     specification of the original Figaro version.
*
*  Arguments :
*     ENVIRO = CHARACTER*(*) (Given)
*        The "environment" from which data are to be read.  This
*        routine assumes that only the values "DATA" and "COPY" will
*        be used.  It reports an error if any other value is
*        supplied.
*     LX,LY = INTEGER (Given and Returned)
*        The desired coordinates in the DATA_ARRAY of the "first"
*        corner of the subarray to be extracted (i.e. the smaller
*        value of X and smaller value of Y).  The values will be
*        changed on output if the input values would run beyond the
*        bounds of the DATA_ARRAY.  Note, however, that Daophot
*        sometimes passes write-protected arguments here, so they
*        are only written to if actually necessary.
*     MX,MY = INTEGER (Given and Returned)
*        The desired number of columns and rows in the subarray.
*        The values will be changed on output if the input values
*        would run beyond the bounds of the DATA_ARRAY.  Note,
*        however, that Daophot sometimes passes write-protected
*        arguments here, so they are only written to if actually
*        necessary.
*     NX = INTEGER (Given)
*        The first dimension size of the FUNC array.
*     FUNC = REAL(NX,*) (Returned)
*        Array to receive the sub-array data values.  The "first"
*        value (corresponding to location (LX,LY) in the DATA_ARRAY)
*        is returned in FUNC(1,1).
*     IFLAG = INTEGER (Returned)
*        The Daophot error flag.  A zero value is returned if there
*        is no error, otherwise this version of the routine returns
*        an HDS error code.
*
*  Algorithm :
*     Adjust the LX,LY,MX & MY values to lie inside the DATA_ARRAY
*     bounds.  Return if this cannot be done.
*     Select the identifier of the appropriate "environment" file.
*     For each row of the sub-array, obtain the appropriate NDF
*     section from the DATA_ARRAY.
*     Map the data from the DATA_ARRAY and read it into the appropriate
*     location in the FUNC array.
*     When all the data have been transferred, report any error.
*     Set the Daophot status flag appropriately.
*
*  Deficiencies :
*     <description of any deficiencies>
*
*  Bugs :
*     <description of any "bugs" which have not been fixed>
*
*  Authors :
*     RFWS: R.F. Warren-Smith (Durham University)
*     NE: Nick Eaton (Durham University)
*     MBT: Mark Taylor (STARLINK)
*
*  History :
*     19-MAY-1988 (RFWS):
*        Original version derived from Daophot RDARAY routine.
*      6-DEC-1991 (NE):
*        NDF version derived from HDS version.
*     19-FEB-1992 (NE):
*        Unix version.
*      8-JUN-2000 (MBT):
*        Fixed for NDFs which do not start at (1,1).
*     10-JUL-2000 (MBT):
*        Fixed bug in ERR_REP call.
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Global variables :
*   ...Daophot picture size:
      INTEGER NCOL
      INTEGER NROW
      COMMON /SIZE  / NCOL , NROW

      INCLUDE 'ndf_cmn'         ! Common block for NDF information
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Arguments Given :
      CHARACTER*(*) ENVIRO      ! Environment whose data is to be read
      INTEGER LX                ! X coordinate of first sub-array corner
      INTEGER LY                ! Y coordinate of first sub-array corner
      INTEGER MX                ! Number of sub-array columns
      INTEGER MY                ! Number of sub_array rows
      INTEGER NX                ! First dimension of FUNC array

*  Arguments Returned :
      REAL FUNC(NX,*)           ! Array to hold sub-array data values

*  Status :
      INTEGER IFLAG             ! Daophot status variable

*  Local variables :
      INTEGER STATUS            ! HDS error status
      INTEGER J                 ! Loop counter for data rows
      INTEGER LBND(2)           ! Lower bounds of NDF
      INTEGER NELE              ! Number of data elements read
      INTEGER OFFS(2)           ! Offset value of pixel indices from array
      INTEGER DIML(2)           ! Lower bounds on data array slice
      INTEGER DIMU(2)           ! Upper bounds on data array slice
      INTEGER IERR
      INTEGER IPNTR
      INTEGER NDF_ISEL
      INTEGER NDF_ISECT
      INTEGER NDIM              ! Number of dimensions of NDF
      INTEGER NERR
      INTEGER UBND(2)           ! Upper bounds of NDF
*.

*   Initialise the HDS status variable.
      STATUS = SAI__OK

*   Adjust the sub-array bounds so that it lies within the DATA_ARRAY.
      MX = LX + MX - 1  ! Upper limit in X
      MY = LY + MY - 1  ! Upper limit in Y
      IF ( LX.LT.1 ) LX = 1
      IF ( LY.LT.1 ) LY = 1
      IF ( MX.GT.NCOL ) MX = NCOL
      IF ( MY.GT.NROW ) MY = NROW
      MX = MX - LX + 1  ! Number of pixels in X
      MY = MY - LY + 1  ! Number of pixels in Y

*   Return if the sub-array lies completely outside the main array.
      IF ( (MX.LE.0) .OR. (MY.LE.0) ) RETURN

*   Select the identifier associated with the specified environment.
*   ..."DATA" environment:
      IF ( ENVIRO.EQ.'DATA' ) THEN
         NDF_ISEL = NDF_IDATA

*   ..."COPY" environment:
      ELSE IF ( ENVIRO.EQ.'COPY' ) THEN
         NDF_ISEL = NDF_ICOPY

*   ...environment not known, so report an error:
      ELSE
         CALL TBLANK
         STATUS = SAI__ERROR
         CALL MSG_SETC('ENV',ENVIRO)
         CALL ERR_REP(' ','RDARAY - unknown environment: ^ENV',STATUS)
      END IF

*   Get bounds of NDF.
      CALL NDF_BOUND(NDF_ISEL,2,LBND,UBND,NDIM,STATUS)
      OFFS(1) = LBND(1) - 1
      OFFS(2) = LBND(2) - 1

*   Loop to read each line of the sub-array from the DATA_ARRAY.
      DO 100 J = 1 , MY

*   Locate the required slice of the DATA_ARRAY.
         DIML(1) = OFFS(1) + LX
         DIMU(1) = OFFS(1) + LX + MX - 1
         DIML(2) = OFFS(2) + LY + J - 1
         DIMU(2) = DIML(2)
         CALL NDF_SECT(NDF_ISEL,2,DIML,DIMU,NDF_ISECT,STATUS)

*   Obtain a pointer to the mapped DATA_ARRAY component
         CALL NDF_MAP(NDF_ISECT,'DATA','_REAL','READ',IPNTR,NELE,STATUS)

*   Read the data into the given array
         CALL VEC_RTOR(.FALSE.,MX,%VAL(CNF_PVAL(IPNTR)),
     :                 FUNC(1,J),IERR,NERR,STATUS)

*   Unmap the data and annul the identifier
         CALL NDF_UNMAP(NDF_ISECT,'DATA',STATUS)
         CALL NDF_ANNUL(NDF_ISECT,STATUS)
 100  CONTINUE

*   Report any errors.
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL TBLANK
         CALL ERR_REP(' ','RDARAY - error reading data',STATUS)
      END IF

*   Set the Daophot status flag.
      IF ( STATUS.EQ.SAI__OK ) THEN
         IFLAG = 0
      ELSE
         IFLAG = STATUS
      END IF

*   Exit routine.
      END
