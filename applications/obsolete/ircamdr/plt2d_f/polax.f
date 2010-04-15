	SUBROUTINE POLAX( STATUS)

* Plots the polarization vector diagram from users P,Th images

* History
* 27-Jul-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
* 02-Sep-1994 Removed unused variables flagged by UNIX compiler (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'SAE_PAR'
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

	INTEGER IST, ISTAT, STATUS
	INTEGER IDIMS_1( 2), IDIMS_2( 2)
	INTEGER PNTRIQP, PNTRIUT
	INTEGER POLDEN
	INTEGER LOCIQP, LOCIUT
        INTEGER NDIMS, NELEMENTS

	REAL THETACORR, VECSLEN

	CHARACTER*80 QUORPT, POLANN, POLTITLE


*      check input status on entry

	IF( STATUS .NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'Error, on entry to POLAX ...', STATUS )
	  RETURN

	END IF

*      set all local status variables to OK

	IST = 0
	ISTAT = 0

*      request the Q,U OR P,TH data images

	CALL PAR_GET0C( 'QUORPT', QUORPT, STATUS)

	CALL NDF_ASSOC( 'INPICQP', 'READ', LOCIQP, STATUS)
	CALL NDF_ASSOC( 'INPICUT', 'READ', LOCIUT, STATUS)
	IF ( STATUS .NE. SAI__OK) then
          CALL ERR_REP('ERR',
     :                 'Error, after NDF_assoc in polax ...',
     :                  STATUS )
	  RETURN
	END IF

*      map in the two input images

	CALL NDF_MAP( LOCIQP, 'Data', '_REAL', 'READ', PNTRIQP,
     :                NELEMENTS, STATUS)
	CALL NDF_MAP( LOCIUT, 'Data', '_REAL', 'READ', PNTRIUT,
     :                NELEMENTS, STATUS)

	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR',
     :                 'Error, after NDF_map in polax ...',
     :                  STATUS )
	  return
	end if

        CALL NDF_DIM( LOCIQP, 2, IDIMS_1, NDIMS, STATUS)
        CALL NDF_DIM( LOCIUT, 2, IDIMS_2, NDIMS, STATUS)

	IF( IDIMS_1( 1) .NE. IDIMS_2( 1) .OR.
     :	    IDIMS_1( 2) .NE. IDIMS_2( 2)) THEN

           CALL MSG_OUT('ERR',
     :        'Error, Q,U or P,TH data_array dimension mismatch ...',
     :                  STATUS )
	  RETURN

	END IF

*      get the instrumental polarization position angle from user

	CALL PAR_GET0R( 'POLPOS', THETACORR, STATUS)

*      get the vector density factor from user

	CALL PAR_GET0I( 'POLDEN', POLDEN, STATUS)

*      get the title for the top of the plot

	CALL PAR_GET0C( 'POLTITLE', POLTITLE, STATUS)

*      get the vector length from user

	CALL PAR_GET0R( 'POLVEC', VECSLEN, STATUS)

*      get option to plot (or not) the annotation around plot

	CALL PAR_GET0C( 'POLANN', POLANN, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR',
     :                 'Error, after par_gets in polax ...',
     :                  STATUS )
	  return
	end if

*      perform the plotting

	CALL POLAX_PLOT( QUORPT, IDIMS_1( 1), IDIMS_1( 2), %VAL( PNTRIQP),
     :	                 %VAL( PNTRIUT), THETACORR, POLANN, POLTITLE,
     :	                 POLDEN, VECSLEN, STATUS)

*      release all input images

	CALL NDF_ANNUL( LOCIQP, STATUS)
	CALL NDF_ANNUL( LOCIUT, STATUS)
	if( status .ne. sai__ok) then
          CALL ERR_REP('ERR',
     :                 'Error, after NDF_annuls in polax ...',
     :                  STATUS )
	  return
	end if

	END
