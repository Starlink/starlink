(new-routine "AGD_ACTIV"
             "Initialise IDI"
             '("STATUS"))

(new-routine "AGD_ASSOC"
             "Associate a device with AGI and IDI"
             '("PARAM" "ACMODE" "PNAME" "MEMID" "PICID" "DISPID" "XSIZE" "YSIZE" "XOFF" "YOFF" "STATUS"))

(new-routine "AGD_DEACT"
             "Close down IDI"
             '("STATUS"))

(new-routine "AGD_DEASS"
             "Deassociate a device from AGI and IDI"
             '("PARAM" "PARCAN" "STATUS"))

(new-routine "AGD_NWIND"
             "Define an IDI window from the current picture"
             '("MEMID" "DISPID" "XSIZE" "YSIZE" "XOFF" "YOFF" "STATUS"))

(new-routine "AGD_SWIND"
             "Save an IDI window in the database"
             '("DISPID" "MEMID" "XSIZE" "YSIZE" "XOFF" "YOFF" "PNAME" "COMENT" "WX1" "WX2" "WY1" "WY2" "PICID" "STATUS"))

(new-routine "AGI_ANNUL"
             "Annul the given picture identifier"
             '("PICID" "STATUS"))

(new-routine "AGI_ASSOC"
             "Associate an AGI device with an ADAM parameter"
             '("PARAM" "ACMODE" "PICID" "STATUS"))

(new-routine "AGI_BEGIN"
             "Mark the beginning of a new AGI scope"
             ( ))

(new-routine "AGI_CANCL"
             "Cancel the ADAM device parameter"
             '("PARAM" "STATUS"))

(new-routine "AGI_CLOSE"
             "Close AGI in non-ADAM environments"
             '("STATUS"))

(new-routine "AGI_END"
             "Mark the end of an AGI scope"
             '("PICID" "STATUS"))

(new-routine "AGI_GTREF"
             "Get a reference object from a picture"
             '("PICID" "MODE" "DATREF" "STATUS"))

(new-routine "AGI_IBASE"
             "Inquire base picture for current device"
             '("PICID" "STATUS"))

(new-routine "AGI_ICOM"
             "Inquire comment for the current picture"
             '("COMENT" "STATUS"))

(new-routine "AGI_ICURP"
             "Inquire the current picture"
             '("PICID" "STATUS"))

(new-routine "AGI_ILAB"
             "Inquire label of a picture"
             '("PICID" "LABEL" "STATUS"))

(new-routine "AGI_IMORE"
             "Inquire if a MORE structure exists"
             '("PICID" "LMORE" "STATUS"))

(new-routine "AGI_INAME"
             "Inquire name of the current picture"
             '("PNAME" "STATUS"))

(new-routine "AGI_IPOBS"
             "Is current picture obscured by another?"
             '("PICID" "LOBS" "STATUS"))

(new-routine "AGI_ISAMD"
             "Inquire if pictures are on same device"
             '("PICID" "LSAME" "STATUS"))

(new-routine "AGI_ISAMP"
             "Inquire if two pictures are the same"
             '("PICID" "LSAME" "STATUS"))

(new-routine "AGI_ITOBS"
             "Inquire if test points are obscured"
             '("NXY" "X" "Y" "LTOBS" "STATUS"))

(new-routine "AGI_IWOCO"
             "Inquire world coordinates of current picture"
             '("WX1" "WX2" "WY1" "WY2" "STATUS"))

(new-routine "AGI_MORE"
             "Return an HDS locator to a MORE structure"
             '("PICID" "ACMODE" "MORLOC" "STATUS"))

(new-routine "AGI_NUPIC"
             "Create a new picture in the database"
             '("WX1" "WX2" "WY1" "WY2" "PNAME" "COMENT" "NEWX1" "NEWX2" "NEWY1" "NEWY2" "PICID" "STATUS"))

(new-routine "AGI_OPEN"
             "Open an AGI device in a non-ADAM environment"
             '("WKNAME" "ACMODE" "PICID" "STATUS"))

(new-routine "AGI_PDEL"
             "Delete all the pictures on the current device"
             '("STATUS"))

(new-routine "AGI_PTREF"
             "Store a reference object in a picture"
             '("DATREF" "PICID" "STATUS"))

(new-routine "AGI_RCF"
             "Recall first picture of specified name"
             '("PNAME" "PICID" "STATUS"))

(new-routine "AGI_RCFP"
             "Recall first picture embracing a position"
             '("PNAME" "X" "Y" "PICID" "STATUS"))

(new-routine "AGI_RCL"
             "Recall last piture of specified name"
             '("PNAME" "PICID" "STATUS"))

(new-routine "AGI_RCLP"
             "Recall last picture embracing a position"
             '("PNAME" "X" "Y" "PICID" "STATUS"))

(new-routine "AGI_RCP"
             "Recall preceding picture of specified name"
             '("PNAME" "PSTART" "PICID" "STATUS"))

(new-routine "AGI_RCPP"
             "Recall preceding picture embracing a position"
             '("PNAME" "PSTART" "X" "Y" "PICID" "STATUS"))

(new-routine "AGI_RCS"
             "Recall succeeding picture of specified name"
             '("PNAME" "PSTART" "PICID" "STATUS"))

(new-routine "AGI_RCSP"
             "Recall succeeding picture embracing a position"
             '("PNAME" "PSTART" "X" "Y" "PICID" "STATUS"))

(new-routine "AGI_SELP"
             "Select the given picture as the current one"
             '("PICID" "STATUS"))

(new-routine "AGI_SLAB"
             "Store label in picture"
             '("PICID" "LABEL" "STATUS"))

(new-routine "AGI_SROOT"
             "Select the root picture for searching"
             '("STATUS"))

(new-routine "AGI_TCOPY"
             "Copy a transformation structure to the database"
             '("TRNLOC" "PICID" "STATUS"))

(new-routine "AGI_TDDTW"
             "Transform double precision data to world coordinates"
             '("PICID" "NXY" "DX" "DY" "WX" "WY" "STATUS"))

(new-routine "AGI_TDTOW"
             "Transform data to world coordinates"
             '("PICID" "NXY" "DX" "DY" "WX" "WY" "STATUS"))

(new-routine "AGI_TNEW"
             "Store a transformation in the database"
             '("NCD" "NCW" "DTOW" "WTOD" "PICID" "STATUS"))

(new-routine "AGI_TWTDD"
             "Transform double precision world to data coordinates"
             '("PICID" "NXY" "WX" "WY" "DX" "DY" "STATUS"))

(new-routine "AGI_TWTOD"
             "Transform world to data coordinates"
             '("PICID" "NXY" "WX" "WY" "DX" "DY" "STATUS"))

(new-routine "AGP_ACTIV"
             "Initialise PGPLOT"
             '("STATUS"))

(new-routine "AGP_ASSOC"
             "Associate a device with AGI and PGPLOT"
             '("PARAM" "ACMODE" "PNAME" "BORDER" "PICID" "STATUS"))

(new-routine "AGP_DEACT"
             "Close down PGPLOT"
             '("STATUS"))

(new-routine "AGP_DEASS"
             "Deassociate a device from AGI and PGPLOT"
             '("PARAM" "PARCAN" "STATUS"))

(new-routine "AGP_NVIEW"
             "Create a new PGPLOT viewport from the current picture"
             '("STATUS"))

(new-routine "AGP_SVIEW"
             "Save the current PGPLOT viewport in the database"
             '("PICNAM" "COMENT" "PICID" "STATUS"))

(new-routine "AGS_ACTIV"
             "Initialise SGS"
             '("STATUS"))

(new-routine "AGS_ASSOC"
             "Associate a device with AGI and SGS"
             '("PARAM" "ACMODE" "PNAME" "PICID" "NEWZON" "STATUS"))

(new-routine "AGS_DEACT"
             "Close down SGS"
             '("STATUS"))

(new-routine "AGS_DEASS"
             "Deassociate a device from AGI and SGS"
             '("PARAM" "PARCAN" "STATUS"))

(new-routine "AGS_NZONE"
             "Create a new SGS zone from the current picture"
             '("NEWZON" "STATUS"))

(new-routine "AGS_SZONE"
             "Save the current SGS zone in the database"
             '("PNAME" "COMENT" "PICID" "STATUS"))
