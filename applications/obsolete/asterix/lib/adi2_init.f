	subroutine adi2_init( status )

        INTEGER         STATUS
        INTEGER         ID
        EXTERNAL        ADI2_OPEN
        EXTERNAL        ADI2_FCREAT
        EXTERNAL        ADI2_FTRACE
        EXTERNAL        ADI2_FCOMIT
        EXTERNAL        ADI2_FCLOSE
        EXTERNAL        ADI2_NEWLNK_ARR

        CALL ADI_DEFREP( 'FITS', ID, STATUS )


	CALL ADI_DEFCLS( 'FITSfile', 'FileObject',
     :                   'STRUC PRIMARY,STRUC EXTENSIONS', DID,
     :                   STATUS )

        CALL ADI_DEFMTH( 'NewLink(Array,FITSfile)', ADI2_NEWLNK_ARR,
     :                   did, STATUS )

        CALL ADI_DEFRCB( ID, 'CreatRtn', ADI2_FCREAT, STATUS )
        CALL ADI_DEFRCB( ID, 'CloseRtn', ADI2_FCLOSE, STATUS )
        CALL ADI_DEFRCB( ID, 'ComitRtn', ADI2_FCOMIT, STATUS )
        CALL ADI_DEFRCB( ID, 'OpenRtn', ADI2_OPEN, STATUS )

        CALL ADI_DEFMTH( 'FileTrace(FITSfile)', ADI2_FTRACE, DID,
     :                   STATUS )

        END
