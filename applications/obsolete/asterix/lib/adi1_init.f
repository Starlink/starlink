	subroutine adi1_init( status )

        INTEGER         STATUS
	INTEGER ID,DID
        EXTERNAL        ADI1_FCREAT
        EXTERNAL        ADI1_OPEN
        EXTERNAL        ADI1_NATRL

        CALL ADI_DEFREP( 'HDS', ID, STATUS )
        CALL ADI_DEFRCB( ID, 'OPEN_RTN', ADI1_OPEN, STATUS )
        CALL ADI_DEFRCB( ID, 'CREAT_RTN', ADI1_FCREAT, STATUS )

	CALL ADI_DEFCLS( 'HDSlocator', ' ', 'Locator', DID,
     :                   STATUS )

	CALL ADI_DEFCLS( 'HDSfile', 'FileObject,HDSlocator', 'x', DID,
     :                   STATUS )

c        CALL ADI_DEFRCB( ID, 'NATRL_RTN', ADI1_NATRL, STATUS )

        END
