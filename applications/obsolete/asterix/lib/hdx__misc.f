*+  HDX_OPENHDSNEW - Opens new HDS file in UPDATE mode
      SUBROUTINE HDX_OPENHDSNEW(PAR_NAME, TYPE, FNAME, LOC,STATUS)
* Description :
*        Obtains a file name from the parameter system and opens an
*        HDS file of that name.
*
* Environment parameters :
*        PAR_NAME         '_CHAR'         Dummy name of parameter for filename
*
* Deficiencies :
* Bugs :
* Authors :
*       R.D.Saxton Feb 1988 adapted from Clive Page	1987 Feb 26
* History :
*      MAY 10 1988   New header    (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
* Import :
      CHARACTER*(*) FNAME          !input: file-name, default extn is .SDF
      CHARACTER*(*) PAR_NAME       !input: parameter name for datafile
      CHARACTER*(*) TYPE           !input: type of dataset being created.
* Export :
      CHARACTER*(DAT__SZLOC) LOC   !output: locator to top-level object.
* Status :
      INTEGER STATUS
*-

* Check status
        IF (STATUS .NE. SAI__OK) RETURN
*
* Allow upto 100 components in new file
        CALL HDS_TUNE('NCOMP', 100, STATUS)
*
* Ask for filename jump back to here if unsatisfactory input.
*
100	CONTINUE
*
	CALL PAR_GET0C(PAR_NAME, FNAME, STATUS)
*
	CALL HDS_NEW(FNAME, 'DATASET', TYPE, 0, 0, LOC, STATUS)
*
	IF (STATUS .NE. SAI__OK) THEN
*
             IF (STATUS .NE. PAR__ABORT) THEN
*
		CALL ERR_REP(' ','OPENHDS_NEW error: cannot open file',
     &                                                    STATUS)
                CALL ERR_FLUSH(STATUS)
		CALL PAR_CANCL(PAR_NAME,STATUS)
*
		GO TO 100
*
             ENDIF
*
	END IF
*
        IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ','from HDX_OPENHDSNEW',STATUS)
        ENDIF
*
	END
*+  HDX_OPENHDSOLD - Opens existing HDS file in UPDATE mode
	SUBROUTINE HDX_OPENHDSOLD(PAR_NAME, FNAME, LOC,STATUS)
*
* Description:
*        Obtains a file name from the parameter system and opens an
*        HDS file of that name.
*
*-Author:
*       R.D.Saxton Feb 1988 adapted from Clive Page	1987 Feb 26
*
* Parameters:
*       PAR_NAME                     Dummy name of parameter for filename
* Global:
        IMPLICIT NONE
        INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'
        INCLUDE 'PAR_ERR'
*
* Import :
	CHARACTER*(*) FNAME          !input: file-name, default extn is .SDF
        CHARACTER*(*) PAR_NAME       !input: parameter name for datafile
* Export :
	CHARACTER*(DAT__SZLOC) LOC   !output: locator to top-level object.
*
        INTEGER STATUS
*
        IF (STATUS .NE. SAI__OK) RETURN
*
100	CONTINUE
*
	CALL PAR_GET0C(PAR_NAME, FNAME, STATUS)
*
	CALL HDS_OPEN(FNAME, 'UPDATE', LOC, STATUS)
*
	IF (STATUS .NE. SAI__OK) THEN
*
             IF (STATUS .NE. PAR__ABORT) THEN
*
		CALL ERR_REP(' ','HDX_OPEN error: cannot open file',
     &                                                    STATUS)
                CALL ERR_FLUSH(STATUS)
		CALL PAR_CANCL(PAR_NAME,STATUS)
*
		GO TO 100
*
             ENDIF
*
	END IF
	END
*+  HDX_SAMESHAPE - returns .TRUE. if objects are same shape
      LOGICAL FUNCTION HDX_SAMESHAPE(ALOC,BLOC)

*    Description :
*     Returns a true value if two objects are of same shape.  .FALSE.
*     returned if either locator invalid.
*    Author :
*             (BHVAD::RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) ALOC,BLOC  ! locators to data objects
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      LOGICAL AVALID,BVALID             ! whether locators valid
      LOGICAL SAME                      ! temporary value of function
      INTEGER NADIMS,ADIMS(DAT__MXDIM)  ! dimensionality and dimensions
      INTEGER NBDIMS,BDIMS(DAT__MXDIM)  ! of two data objects being compared
      INTEGER IDIM                      ! index to dimensions
*    Internal References :
*    Local data :
*-
      STATUS=SAI__OK
      SAME=.FALSE.
*  check validity of locators
      CALL DAT_VALID(ALOC,AVALID,STATUS)
      CALL DAT_VALID(BLOC,BVALID,STATUS)
      IF (AVALID.AND.BVALID) THEN
*  get dimensions of two objects
        CALL DAT_SHAPE(ALOC,DAT__MXDIM,ADIMS,NADIMS,STATUS)
        CALL DAT_SHAPE(BLOC,DAT__MXDIM,BDIMS,NBDIMS,STATUS)

*  must be same number of dimensions
        SAME=(NADIMS.EQ.NBDIMS)
        IDIM=1

*  each dimension must be same size
        DO WHILE (SAME.AND.IDIM.LE.NADIMS)
          SAME=(ADIMS(IDIM).EQ.BDIMS(IDIM))
          IDIM=IDIM+1
        ENDDO
      ENDIF
      HDX_SAMESHAPE=SAME
      END
