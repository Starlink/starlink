*+  SFIT_OPFILES - Write output file list to fit output stream
      SUBROUTINE SFIT_OPFILES( FSTAT, NDS, OBDAT, MODEL, OCI, STATUS )
*
*    Description :
*
*     Writes a description of the fit dataset and fit model objects to an
*     output stream.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan(JET-X,University of Birmingham)
*
*    History :
*
*     21 Jul 94 : Original (DJA)
*     25 Jul 94 : Converted to use AIO system (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER			FSTAT			! Fit statistic
      INTEGER			NDS			! Number of datasets
      RECORD /DATASET/    	OBDAT(NDS)		! Observed datasets
      RECORD /MODEL_SPEC/ 	MODEL			! Model specification
      INTEGER			OCI			! AIO stream id
*
*    Local variables :
*
      CHARACTER*132 		FILE			! Filename
      CHARACTER*80		PATH			! Path to object

      INTEGER			I			! Loop over datasets
      INTEGER			NLEV			! Trace info
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Write list of files
      CALL AIO_WRITE( OCI, 'Fitting to datasets:', STATUS )
      DO I = 1, NDS
        CALL MSG_SETC( 'DFILE', OBDAT(I).DATNAME )
        CALL AIO_IWRITE( OCI, 21, '^DFILE', STATUS )
        IF ( FSTAT .EQ. FIT__LOGL ) THEN
          IF ( OBDAT(I).BLOC .NE. DAT__NOLOC ) THEN
            CALL HDS_TRACE( OBDAT(I).BLOC, NLEV, PATH, FILE, STATUS )
          ELSE
            FILE = '** Unable to find bgnd - assumed negligible **'
          END IF
          CALL MSG_SETC( 'BFILE', FILE )
          CALL AIO_IWRITE( OCI, 14, 'bgnd : ^BFILE', STATUS )
        END IF
        CALL AIO_BLNK( OCI, STATUS )
      END DO

*    Write model prescription
      CALL MSG_SETC( 'MODEL', MODEL.SPEC )
      CALL AIO_WRITE( OCI, 'Using model  : ^MODEL', STATUS )
      CALL HDS_TRACE( MODEL.MLOC, NLEV, PATH, FILE, STATUS )
      CALL MSG_SETC( 'MFILE', FILE )
      CALL AIO_WRITE( OCI, 'From dataset : ^MFILE', STATUS )

*    Terminate with anouther blank
      CALL AIO_BLNK( OCI, STATUS )

      END
