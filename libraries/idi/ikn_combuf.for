*.......................................................................
*   Common block for buffer I/O storage
*
*   History
*    March 1990  Added double buffering for synchronous QIOs
*    March 1991  Renamed IKN_* from IDI_*
*   endhistory

*   QIO channel number
      INTEGER * 2 QCHAN

*   Flag for indicating output buffer in use
      LOGICAL WRITEF

*   Number of bytes in buffers
      INTEGER BUFLN1
      INTEGER BUFLN2

*   Buffer for I/O
      BYTE BUFF1( MAXBUF )
      BYTE BUFF2( MAXBUF )

*   Buffer number in use
      INTEGER BUFNUM

*   QIO completion flag
      LOGICAL LQIO

*   QIO status block
      INTEGER * 2 IOSB( 4 )

*   QIO event flag
      INTEGER NEVF

*   Flags for reversing bytes
      INTEGER REVRS1
      INTEGER REVRS2

      COMMON / IKN_COMBUF / QCHAN, WRITEF, BUFLN1, BUFLN2, BUFF1, BUFF2,
     :                      BUFNUM, LQIO, IOSB, NEVF, REVRS1, REVRS2

      SAVE / IKN_COMBUF /

