(new-routine "IDI_ANNUL"
             "Annul the given display identifier"
             '("DISPID" "STATUS"))

(new-routine "IDI_ASSOC"
             "Open IDI in the ADAM environment"
             '("PNAME" "ACMODE" "DISPID" "STATUS"))

(new-routine "IDI_CANCL"
             "Cancel the ADAM device parameter"
             '("PNAME" "STATUS"))

(new-routine "IDI_CLRFG"
             "Set clear flag"
             '("IFLAG"))

(new-routine "IICINC"
             "Initialize Cursor"
             '("DISPID" "MEMID" "NUMCUR" "SHAPE" "COLOR" "XC" "YC" "STATUS"))

(new-routine "IICRCP"
             "Read Cursor Position"
             '("DISPID" "INMID" "NUMCUR" "XC" "YC" "OUTMID" "STATUS"))

(new-routine "IICSCV"
             "Set Cursor Visibility"
             '("DISPID" "NUMCUR" "LVIS" "STATUS"))

(new-routine "IICWCP"
             "Write Cursor Position"
             '("DISPID" "MEMID" "NUMCUR" "XC" "YC" "STATUS"))

(new-routine "IIDAMY"
             "Allocate Memory"
             '("DISPID" "XSIZE" "YSIZE" "MEMDEP" "MEMTYP" "MEMID" "STATUS"))

(new-routine "IIDCLO"
             "Close Display"
             '("DISPID" "STATUS"))

(new-routine "IIDENC"
             "Enable configuration"
             '("DISPID" "STATUS"))

(new-routine "IIDERR"
             "Get Error"
             '("STATUS" "MESSAG" "MESLEN"))

(new-routine "IIDIAG"
             "Diagnostic Routine"
             '("DISPID" "OUTID" "STATUS"))

(new-routine "IIDOPN"
             "Open Display"
             '("DEVNAM" "DISPID" "STATUS"))

(new-routine "IIDQCI"
             "Query Capabilities Integer"
             '("DISPID" "CAPID" "NARR" "OUTARR" "NOUT" "STATUS"))

(new-routine "IIDQCR"
             "Query Capabilities Real"
             '("DISPID" "CAPID" "NARR" "OUTARR" "NOUT" "STATUS"))

(new-routine "IIDQDC"
             "Query Defined Configuration"
             '("DISPID" "NCONF" "MEMTYP" "NMEMAX" "MODCON" "MEMID" "MEMSIX" "MEMSIY" "MEMDEP" "ITTDEP" "NMEM" "STATUS"))

(new-routine "IIDQDV"
             "Query Device Characteristics"
             '("DISPID" "NCONF" "XSIZE" "YSIZE" "DEPTH" "NVLUT" "NITT" "NCURS" "STATUS"))

(new-routine "IIDRLC"
             "Release Configuration"
             '("DISPID" "NCONF" "STATUS"))

(new-routine "IIDRST"
             "Reset Display"
             '("DISPID" "STATUS"))

(new-routine "IIDSDP"
             "Select Display Path"
             '("DISPID" "MEMID" "NMEM" "LUTLIS" "ITTLIS" "STATUS"))

(new-routine "IIDSEL"
             "Select Configuration"
             '("DISPID" "NCONF" "STATUS"))

(new-routine "IIDSNP"
             "Create Snapshot"
             '("DISPID" "CMODE" "NPIX" "XSTART" "YSTART" "DEPTH" "PACK" "IMAGE" "STATUS"))

(new-routine "IIDSSS"
             "Set Split Screen"
             '("DISPID" "MEMID" "XOFF" "YOFF" "SPLIT" "XSPLIT" "YSPLIT" "STATUS"))

(new-routine "IIDSTC"
             "Stop Configuration"
             '("DISPID" "NCONF" "STATUS"))

(new-routine "IIDUPD"
             "Update Display"
             '("DISPID" "STATUS"))

(new-routine "IIEGEP"
             "Get Escape Parameter"
             '("PARAM" "SLEN" "STRING" "STATUS"))

(new-routine "IIEPEP"
             "Put Escape Parameter"
             '("PARAM" "SLEN" "STRING" "STATUS"))

(new-routine "IIGPLY"
             "Polyline"
             '("DISPID" "MEMID" "X" "Y" "NXY" "COLOR" "LSTYLE" "STATUS"))

(new-routine "IIGTXT"
             "Plot text"
             '("DISPID" "MEMID" "TEXT" "XPOS" "YPOS" "TPATH" "TANGLE" "COLOR" "TSIZE" "STATUS"))

(new-routine "IIIEIW"
             "Execute interaction and wait"
             '("DISPID" "TRIGS" "STATUS"))

(new-routine "IIIENI"
             "Enable interaction"
             '("DISPID" "INTTY" "INTID" "OBJTY" "OBJID" "INTOP" "EXTRN" "STATUS"))

(new-routine "IIIGIE"
             "Get Integer Evaluator"
             '("DISPID" "NEVAL" "IVALUE" "STATUS"))

(new-routine "IIIGLD"
             "Get Locator Displacement"
             '("DISPID" "LOCNUM" "DX" "DY" "STATUS"))

(new-routine "IIIGLE"
             "Get Logical Evaluator"
             '("DISPID" "NEVAL" "LVALUE" "STATUS"))

(new-routine "IIIGRE"
             "Get Real Evaluator"
             '("DISPID" "NEVAL" "RVALUE" "STATUS"))

(new-routine "IIIGSE"
             "Get String Evaluator"
             '("DISPID" "NEVAL" "STRING" "SLEN" "STATUS"))

(new-routine "IIIQID"
             "Query Interactor Description"
             '("DISPID" "INTTY" "INTID" "MESSAG" "MESLEN" "STATUS"))

(new-routine "IIISTI"
             "Stop Interactive Input"
             '("DISPID" "STATUS"))

(new-routine "IILRIT"
             "Read Intensity Transformation Table"
             '("DISPID" "MEMID" "ITTNUM" "START" "NENT" "ITT" "STATUS"))

(new-routine "IILRLT"
             "Read Video Look Up Table"
             '("DISPID" "LUTNUM" "START" "NENT" "VLUT" "STATUS"))

(new-routine "IILSBV"
             "Set Intensity Bar Visibility"
             '("DISPID" "MEMID" "LVIS" "STATUS"))

(new-routine "IILWIT"
             "Write Intensity Transformation Table"
             '("DISPID" "MEMID" "ITTNUM" "START" "NENT" "ITT" "STATUS"))

(new-routine "IILWLT"
             "Write Video Look Up Table"
             '("DISPID" "LUTNUM" "START" "NENT" "VLUT" "STATUS"))

(new-routine "IIMBLM"
             "Blink Memories"
             '("DISPID" "MEMID" "NMEM" "BLINKS" "STATUS"))

(new-routine "IIMCMY"
             "Clear Memory"
             '("DISPID" "MEMID" "NMEM" "BACK" "STATUS"))

(new-routine "IIMEBM"
             "Define External Bitmap"
             '("DISPID" "BMDSCR" "BMTYPE" "XSIZE" "YSIZE" "STATUS"))

(new-routine "IIMRMY"
             "Read Memory"
             '("DISPID" "MEMID" "NPIX" "XSTART" "YSTART" "DEPTH" "PACK" "ITTON" "IMAGE" "STATUS"))

(new-routine "IIMSLT"
             "Select Memory Look up Tables"
             '("DISPID" "MEMID" "LUTNUM" "ITTNUM" "STATUS"))

(new-routine "IIMSMV"
             "Set Memory Visibility"
             '("DISPID" "MEMID" "NMEM" "LVIS" "STATUS"))

(new-routine "IIMSTW"
             "Set Transfer Window"
             '("DISPID" "MEMID" "DIRECN" "XSIZE" "YSIZE" "DEPTH" "XOFF" "YOFF" "STATUS"))

(new-routine "IIMWMY"
             "Write Memory"
             '("DISPID" "MEMID" "IMAGE" "NPIX" "DEPTH" "PACK" "XSTART" "YSTART" "STATUS"))

(new-routine "IIRINR"
             "Initialize Rectangular Region of Interest"
             '("DISPID" "MEMID" "ROICOL" "XMIN" "YMIN" "XMAX" "YMAX" "ROIID" "STATUS"))

(new-routine "IIRRRI"
             "Read Rectangular Region of Interest"
             '("DISPID" "INMID" "ROIID" "XMIN" "YMIN" "XMAX" "YMAX" "OUTMID" "STATUS"))

(new-routine "IIRSRV"
             "Set Visibility Rectangular Region of Interest"
             '("DISPID" "ROIID" "LVIS" "STATUS"))

(new-routine "IIRWRI"
             "Write Rectangular Region of Interest"
             '("DISPID" "MEMID" "ROIID" "XMIN" "YMIN" "XMAX" "YMAX" "STATUS"))

(new-routine "IIZRSZ"
             "Read Memory Scroll and Zoom"
             '("DISPID" "MEMID" "XOFF" "YOFF" "ZOOMF" "STATUS"))

(new-routine "IIZRZP"
             "Read Display Zoom and Pan"
             '("DISPID" "XOFF" "YOFF" "ZOOMF" "STATUS"))

(new-routine "IIZWSC"
             "Write Memory Scroll"
             '("DISPID" "MEMID" "NMEM" "XOFF" "YOFF" "STATUS"))

(new-routine "IIZWZM"
             "Write Memory Zoom"
             '("DISPID" "MEMID" "NMEM" "ZOOMF" "STATUS"))

(new-routine "IIZWZP"
             "Write Display Zoom and Pan"
             '("DISPID" "XOFF" "YOFF" "ZOOMF" "STATUS"))
