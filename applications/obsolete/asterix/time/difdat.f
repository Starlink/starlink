*+   DIFDAT - Difference data set
      SUBROUTINE DIFDAT( STATUS )
*
*    Description :
*
*     DIFDAT differences a time series dataset of intensity values,differen-
*     cing successive values in order to remove any low frequency variation.
*     It can deal with primitive datasets,assuming a constant timescale,as
*     well as structures containing data values at irregular intervals and
*     quality information.
*
*    Environment parameters :
*
*
*
*
*
*    Method :
*     The program outputs a difference dataset and a corresponding timescale
*     the time for each point being the average of the twopoints that were
*     differenced to get the difference value.
*
*    Deficiences :
*
*    Authors :
*
*     S.R.Duck   (BHVAD::SRD)
*     G.R.Mellor (LTVAD::GRM)
*
*    History :
*
*     09 Mar 90 : V1.2-0  Original (BHVAD::SRD)
*     28 Jun 90 : V1.2-1  Corrected (BHVAD::SRD)
*      8 Nov 91 : V1.2-2  Corrected (LTVAD::GRM)
*     20 Apr 95 : V1.8-0  Updated data interfaces (DJA)
*     12 Dec 1995 V2.0-0 ADi port (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS ! status return variable.
*
*    Local variables :
*
      INTEGER			IFID			! Input dataset id
      INTEGER 			NDIM, DIMS(ADI__MXDIM)	! Input dimensions
      INTEGER			OFID			! Output dataset id

	INTEGER DPTR
	INTEGER ODPTR
	INTEGER NOP
	INTEGER AXPTR
	INTEGER QPTR
	INTEGER NBAD
	INTEGER OAXPTR
	INTEGER VPTR
	INTEGER OVPTR
	INTEGER NDAT

	LOGICAL INPRIM
	LOGICAL VOK
	LOGICAL OK

*    Version :
	CHARACTER*30 VERSION
	PARAMETER  (VERSION='DIFDAT Version 2.0-0')
*-

*  Version
      CALL MSG_PRNT( VERSION )

*  Initialise
      CALL AST_INIT()

*  Dataset access
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )

*  Check 1-D
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( NDIM .NE. 1 ) THEN
	STATUS=SAI__ERROR
	CALL ERR_REP( ' ', 'Dataset is not 1-D', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Input and output dimensions
      NDAT = DIMS(1)

*  Map input data
      CALL BDI_MAPR( IFID, 'Data', 'READ', DPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Primitive dataset?
      CALL ADI_DERVD( IFID, 'Array', INPRIM, STATUS )
      IF ( INPRIM ) THEN

        CALL BDI_LINK( 'BinDS', 1, NDAT - 1, 'REAL', OFID, STATUS )

*    Set flags
	CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )
        SPARR(1) = 0.0
        SPARR(1) = 1.0
        CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
        CALL BDI_MAPR( OFID,'WRITE',OVPTR,STATUS )
        IF(STATUS.NE.SAI__OK)GOTO 99
        CALL DIFDAT_CALCP( NDAT, %VAL(DPTR), %VAL(ODPTR) )
        CALL MATH_POISSVAR( %VAL(ODPTR), NDAT, 1, %VAL(OVPTR), STATUS )

*  Non-primitive
      ELSE

*    Check axis data
        CALL BDI_AXCHK( IFID, 1, 'Data', OK, STATUS )
	IF ( .NOT. OK ) THEN
	  STATUS=SAI__ERROR
          CALL MSG_PRNT( 'Bad axis, values will assume regularly'/
     :                   /' spaced' )
	END IF
	IF(STATUS.NE.SAI__OK)GOTO 99

*    Map axis data
        CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', AXPTR, STATUS )

*    Quality there?
	CALL BDI_CHK( IFID, 'Quality', OK, STATUS )
	IF ( OK )THEN
	  CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', QPTR, STATUS )
          CALL ARR_NBAD( NDAT, %VAL(QPTR), NBAD, STATUS )
          IF ( NBAD .GT. 0 ) THEN
            CALL MSG_SETI( 'NBAD', NBAD )
            CALL MSG_PRNT( '^NBAD bad quality points present' )
	  END IF
	ELSE
          NBAD=0
	END IF
	NOP = NDAT - 1 - NBAD

*    Link output
        CALL BDI_LINK( 'BinDS', 1, NOP, 'REAL', OFID, STATUS )

*    Variance
        CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
	IF ( VOK ) THEN
	  CALL BDI_MAPR( IFID, 'Variance', 'READ', VPTR, STATUS )
	ELSE
	  CALL DYN_MAPR( 1, NDAT, VPTR, STATUS )
          CALL MATH_POISSVAR( %VAL(DPTR), NDAT, 1, %VAL(VPTR), STATUS )
	END IF

*    Map output arrays
        CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )
        CALL BDI_MAPR( OFID, 'Variance', 'WRITE', OVPTR, STATUS )
        CALL BDI_AXMAPR( OFID, 1, 'Data', 'WRITE', OAXPTR, STATUS )

*    Copy stuff
        CALL BDI_AXCOPY( IFID, 1, 'Label,Units', OFID, 1, STATUS )
        CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )
        CALL UDI_COPANC( IFID, ' ', OFID, STATUS )

*    Perform differencing
        IF ( OK ) THEN
	  CALL DIFDAT_CALCS(NDAT,NOP,%VAL(DPTR),%VAL(QPTR),%VAL(AXPTR),
     :     %VAL(VPTR),%VAL(ODPTR),%VAL(OAXPTR),%VAL(OVPTR))
	ELSE
	  CALL DIFDAT_CALNQ(NDAT,NOP,%VAL(DPTR),%VAL(AXPTR),%VAL(VPTR),
     :     %VAL(ODPTR),%VAL(OAXPTR),%VAL(OVPTR))
        END IF

      END IF

*  History
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD(OFID,VERSION,STATUS)

*  Tidy up
  99  CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+   DIFDAT_CALCP-difference calculation for primitive dataset
	SUBROUTINE DIFDAT_CALCP(NDAT,DATA,DIFF)
*    Description :
*     This subroutine outputs an array of differences between neighbouring
*     points in the input array.
*    History :
*     22-NOV-88:  original (BHVAD::SRD)
*    Type definitions :
	IMPLICIT NONE
*    Import :

	INTEGER NDAT
	REAL DATA(NDAT)
*    Import-Export :
*
*    Export :
	REAL DIFF(NDAT-1)
*    Local constants :
*
*    Local variables :
	INTEGER I
*-
	DO I=1,NDAT-1
	   DIFF(I)=DATA(I+1)-DATA(I)
	ENDDO
	END


*+   DIFDAT_CALCS-Difference calculation for structured dataset.
	SUBROUTINE DIFDAT_CALCS(NDAT,NOP,DAT,QUAL,AX,VARI,DIFF,DIFFAX,VARO)
*    Description :
*     This subroutine outputs an array of differences between neighbouring good
*     data points. It also outputs the corresponding axis value for each
*     difference value.
*    History :
*     22-NOV-88: original (BHVAD::SRD)
*    Type definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NDAT
	INTEGER NOP
	REAL DAT(NDAT)
	REAL AX(NDAT)
	REAL VARI(NDAT)
	LOGICAL QUAL(NDAT)
*    Import-Export :
*    Export :
	REAL DIFF(NOP)
	REAL DIFFAX(NOP)
	REAL VARO(NOP)
*    Local variables :
	INTEGER NIP
	INTEGER NOPT
	INTEGER I
	INTEGER ILAST
*
*
	NOPT=1
	NIP=1
	DO WHILE(.NOT.QUAL(NIP))
	   NIP=NIP+1
	ENDDO
	   ILAST=NIP
	   DO I=NIP+1,NDAT
	      IF (QUAL(I))THEN
	         DIFF(NOPT)=(DAT(I)-DAT(ILAST)/(((AX(NDAT)-AX(1))/NDAT)/
     :	(AX(I)-AX(ILAST))))
	         VARO(NOPT)=VARI(ILAST)+VARI(I)
		 ILAST=I
		 DIFFAX(NOPT)=(AX(ILAST)+AX(I))/2
		 NOPT=NOPT+1
	      ENDIF
	   ENDDO
	END




*+   DIFDAT_CALNQ-Difference calculations for axis but no quality.
	SUBROUTINE DIFDAT_CALNQ(NDAT,NOP,DAT,AX,VARI,DIFF,DIFFAX,VARO)
*    Description :
*     This subroutine outputs an array of differences between neighbouring data
*     points.It also outputs the corresponding axis value for each difference
*     value.
*    History :
*     20-DEC-88: origina; (BHVAD::SRD)
*    Type definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NDAT
	INTEGER NOP
	REAL DAT(NDAT)
	REAL AX(NDAT)
	REAL VARI(NDAT)
*    Import-Export :
*    Export :
	REAL DIFF(NOP)
	REAL DIFFAX(NOP)
	REAL VARO(NOP)
*    Local constants :
*    Local variables :
	INTEGER I
*
*
	DO I=1,NOP
	   DIFF(I)=DAT(I+1)-DAT(I)
	   DIFFAX(I)=(AX(I)+AX(I+1))/2
	   VARO(I)=VARI(I)+VARI(I+1)
	ENDDO

	END
