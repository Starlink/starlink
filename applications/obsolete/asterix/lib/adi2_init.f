	subroutine adi2_init( status )

        INTEGER         STATUS
        INTEGER         ID
        EXTERNAL        ADI2_OPEN

        CALL ADI_DEFREP( 'FITS', ID, STATUS )

c        CALL ADI_DEFRCB( ID, 'OPEN_RTN', ADI2_OPEN, STATUS )

        END
