	subroutine adi1_init( status )

        INTEGER         STATUS
	INTEGER ID,DID
        EXTERNAL        ADI1_FCREAT
        EXTERNAL        ADI1_FTRACE
        EXTERNAL        ADI1_OPEN
        EXTERNAL        ADI1_NATRL

        CALL ADI_DEFREP( 'HDS', ID, STATUS )
        CALL ADI_DEFRCB( ID, 'OpenRtn', ADI1_OPEN, STATUS )
        CALL ADI_DEFRCB( ID, 'CreatRtn', ADI1_FCREAT, STATUS )

	CALL ADI_DEFCLS( 'HDSlocator', ' ', 'Locator', DID,
     :                   STATUS )

	CALL ADI_DEFCLS( 'HDSfile', 'FileObject,HDSlocator', ' ', DID,
     :                   STATUS )

        CALL ADI_DEFMTH( 'FileTrace(HDSlocator)', ADI1_FTRACE, DID,
     :                   STATUS )

c        CALL ADI_DEFRCB( ID, 'NatrlRtn', ADI1_NATRL, STATUS )

        END
