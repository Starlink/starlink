*.......................................................................
*   Common block for remebering positions of things

*   Memory x position, in screen coords
      INTEGER CMEMX( 0 : MAXMEM - 1 )

*   Memory y position, in screen coords
      INTEGER CMEMY( 0 : MAXMEM - 1 )

*   Memory zoom factors
      INTEGER CMEMZ( 0 : MAXMEM - 1 )

*   Cursor x position, in screen coords
      INTEGER CURX( 0 : MAXCUR - 1 )

*   Cursor y position, in screen coords
      INTEGER CURY( 0 : MAXCUR - 1 )

*   ROI minimum x position, in screen coords
      INTEGER CROIXL( 0 : MAXROI - 1 )

*   ROI maximum x position, in screen coords
      INTEGER CROIXH( 0 : MAXROI - 1 )

*   ROI minimum y position, in screen coords
      INTEGER CROIYL( 0 : MAXROI - 1 )

*   ROI maximum y position, in screen coords
      INTEGER CROIYH( 0 : MAXROI - 1 )

*   Screen scrolling position in x
      INTEGER CSCROX( 0 : MAXMEM - 1 )

*   Screen scrolling position in y
      INTEGER CSCROY( 0 : MAXMEM - 1 )

*   Locator displacement
      INTEGER CLOCXY( 2 )

*   Constant scrolling offsets ( e.g. to get full Ikon screen )
      INTEGER CSCROF( 2 )

      COMMON / IKN_COMPOS / CMEMX, CMEMY, CMEMZ, CURX, CURY,
     :                      CROIXL, CROIXH, CROIYL, CROIYH,
     :                      CSCROX, CSCROY, CLOCXY, CSCROF

      SAVE / IKN_COMPOS /

