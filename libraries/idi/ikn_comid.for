*.......................................................................
*   Common block for storing things by identifier

*   Array storage for the QIO channel numbers
      INTEGER * 2 ACHAN( MAXID )

*   Array storage for the device open/close flags
      INTEGER ONOFF( MAXID )

*   Flag to indicate that the device is to be opened as it stands, i.e.
*   the context will be read from the device, rather than from the
*   context file. Also no context file will be written on closedown.
*   This flag is stored in this common block so that it will be
*   initialised by the IDI_DATID block data.
      INTEGER CLRFG

*   Cache of ROI identifiers. Note IIRINR assigns the identifiers
*   but there is no way of deassigning them. Therefore they cannot
*   be saved with the context and have to be re-initialised on
*   each execution.
      INTEGER CROIID( 0 : MAXROI - 1 )

      COMMON / IKN_COMID / ACHAN, ONOFF, CLRFG, CROIID

      SAVE / IKN_COMID /

