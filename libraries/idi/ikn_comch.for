*.......................................................................
*   Common block storage for device characteristics

*   Current display identifier
      INTEGER CURRID

*   Implementation level
      INTEGER CIMPLE

*   Number of available configurations
      INTEGER CONNUM

*   Selected configuration number
      INTEGER CONFIG

*   Physical size of display
      INTEGER CNPIX( 0 : 1 )

*   Display depth ( number of bits in DAC )
      INTEGER CDISDE

*   Maximum depth of VLUT's
      INTEGER CLUTDE

*   Number of VLUT's
      INTEGER CNLUT

*   Number of ITT's per image memory
      INTEGER CNITT

*   Zoom range
      INTEGER CZOOMR( 0 : 1 )

*   Memory visible ( logical )
      INTEGER CMEMVI( 0 : MAXMEM - 1 )

*   List of memories in currently selected configuration
      INTEGER CONMEM( 0 : MAXMEM - 1 )

*   Depth of memories ( in bits )
      INTEGER CMEMDE( 0 : MAXMEM - 1 )

*   List of current VLUT bindings
      INTEGER CLUTBI( 0 : MAXMEM - 1 )

*   List of current display path bindings
      INTEGER CDISBI( 0 : MAXMEM - 1 )

*   List of current ITT bindings
      INTEGER CITTBI( 0 : MAXMEM - 1 )

*   Maximum dimensions of transfer window in x
      INTEGER CTWDIX( 0 : MAXMEM - 1 )

*   Maximum dimensions of transfer window in y
      INTEGER CTWDIY( 0 : MAXMEM - 1 )

*   Transfer window x sizes
      INTEGER CTWSIX( 0 : MAXMEM - 1 )

*   Transfer window y sizes
      INTEGER CTWSIY( 0 : MAXMEM - 1 )

*   Transfer window x offsets
      INTEGER CTWOFX( 0 : MAXMEM - 1 )

*   Transfer window y offsets
      INTEGER CTWOFY( 0 : MAXMEM - 1 )

*   Depth of transfer windows in bits
      INTEGER CTWDE( 0 : MAXMEM - 1 )

*   Transfer window load direction
      INTEGER CTWDIR( 0 : MAXMEM - 1 )

*   Number of available device cursors
      INTEGER CURN

*   Array of cursor shapes
      INTEGER CURASH( 0 : MAXCUR - 1 )

*   Number of cursor shapes
      INTEGER CURNSH

*   List of current cursor bindings
      INTEGER CURBI( 0 : MAXCUR - 1 )

*   List of current cursor shapes
      INTEGER CURSHA( 0 : MAXCUR - 1 )

*   List of current cursor colours
      INTEGER CURCOL( 0 : MAXCUR - 1 )

*   List of current cursor visibilities
      INTEGER CURVIS( 0 : MAXCUR - 1 )

*   Number of locators
      INTEGER CNLOC

*   Number of real evaluators
      INTEGER CNREVA

*   Number of integer evaluators
      INTEGER CNIEVA

*   Number of logical evaluators ( switches )
      INTEGER CNLEVA

*   Number of character evaluators
      INTEGER CNCEVA

*   Number of triggers
      INTEGER CNTRIG

*   Region of interest ( ROI ) implemented ( logical )
      INTEGER CANROI

*   Number of device ROI's
      INTEGER CNROI

*   List of current ROI bindings
      INTEGER CROIBI( 0 : MAXROI - 1 )

*   List of current ROI marker colours
      INTEGER CROICO( 0 : MAXROI - 1 )

*   List of current ROI visibilities
      INTEGER CROIVI( 0 : MAXROI - 1 )

*   Blink implemented ( logical )
      INTEGER CANBLI

*   Blink period for each memory
      REAL CBLINK( 0 : MAXMEM - 1 )

*   Trigger number to increase blink rate ( IIMBLM )
      INTEGER CBLINS

*   Trigger number to decrease blink rate ( IIMBLM )
      INTEGER CBLDES

*   Trigger number to stop blink ( IIMBLM )
      INTEGER CBLSTP

*   Split screen implemented ( logical )
      INTEGER CANSPL

*   Split screen x memory offsets
      INTEGER CSPXOF( 0 : MAXMEM - 1 )

*   Split screen y memory offsets
      INTEGER CSPYOF( 0 : MAXMEM - 1 )

*   Split screen ( x, y ) address
      INTEGER CSPLXY( 0 : 1 )

*   Split screen enabled ( logical )
      INTEGER CSPLON

*   Intensity bar implemented ( logical )
      INTEGER CANINT

*   Intensity bar visible ( logical )
      INTEGER CINTVI( 0 : MAXMEM - 1 )

*   Snapshot implemented ( logical )
      INTEGER CANSNA

*   Escape function implemented ( logical )
      INTEGER CANESC

*   Diagnostic routine implemented ( logical )
      INTEGER CANDIA

*   Dynamic configuration implemented ( logical )
      INTEGER CANDYN

      COMMON / IKN_COMCH / CURRID, CIMPLE, CONNUM, CONFIG, CNPIX,
     :                     CDISDE, CLUTDE, CNLUT, CNITT, CZOOMR,
     :                     CMEMVI, CONMEM, CMEMDE, CLUTBI, CDISBI,
     :                     CITTBI, CTWDIX, CTWDIY, CTWSIX, CTWSIY,
     :                     CTWOFX, CTWOFY, CTWDE, CTWDIR, CURN,
     :                     CURASH, CURNSH, CURBI, CURSHA, CURCOL,
     :                     CURVIS, CNLOC, CNREVA, CNIEVA, CNLEVA,
     :                     CNCEVA, CNTRIG, CANROI, CNROI, CROIBI,
     :                     CROICO, CROIVI, CANBLI, CBLINK, CBLINS,
     :                     CBLDES, CBLSTP, CANSPL, CSPXOF, CSPYOF,
     :                     CSPLXY, CSPLON, CANINT, CINTVI, CANSNA,
     :                     CANESC, CANDIA, CANDYN

      SAVE / IKN_COMCH /

