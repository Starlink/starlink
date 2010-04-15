(new-routine "SGS_ANNUL"
             "Close graphics workstation without cancelling parameter"
             '("IZONID" "STATUS"))

(new-routine "SGS_APOLY"
             "Append a new line to a polyline"
             '("X" "Y"))

(new-routine "SGS_ARC"
             "Draw arc of a circle"
             '("X" "Y" "R" "THETA1" "THETA2"))

(new-routine "SGS_ASSOC"
             "Associate graphics workstation with parameter and open it"
             '("PARAM" "MODE" "IZONID" "STATUS"))

(new-routine "SGS_ATEXT"
             "Append a field to the text buffer"
             '("STRING"))

(new-routine "SGS_ATXB"
             "Append to text with blanks"
             '("STRING" "NSPACE"))

(new-routine "SGS_ATXI"
             "Append to text an integer"
             '("I" "NFI"))

(new-routine "SGS_ATXL"
             "Append to text left justified"
             '("STRING"))

(new-routine "SGS_ATXR"
             "Append to text a real number"
             '("R" "NFI" "NDP"))

(new-routine "SGS_BOX"
             "Draw a box"
             '("X1" "X2" "Y1" "Y2"))

(new-routine "SGS_BPOLY"
             "Begin a polyline"
             '("X" "Y"))

(new-routine "SGS_BTEXT"
             "Begin a new text string"
             '("X" "Y"))

(new-routine "SGS_BZNDC"
             "Set a base zone extent in NDC"
             '("X1" "X2" "Y1" "Y2" "POS" "STATUS"))

(new-routine "SGS_CANCL"
             "Close graphics workstation and cancel parameter"
             '("PARAM" "STATUS"))

(new-routine "SGS_CIRCL"
             "Draw circle"
             '("X" "Y" "R"))

(new-routine "SGS_CLOSE"
             "Close all graphics (including GKS)"
             ( ))

(new-routine "SGS_CLRBL"
             "Clear block"
             '("X1" "X2" "Y1" "Y2"))

(new-routine "SGS_CLRFG"
             "Set clear screen flag"
             '("IFLAG"))

(new-routine "SGS_CLRZ"
             "Clear zone"
             ( ))

(new-routine "SGS_CLSWK"
             "Close workstation"
             '("IZONID" "STATUS"))

(new-routine "SGS_CUVIS"
             "Set cursor visibility"
             '("VIS"))

(new-routine "SGS_DEACT"
             "Deactivate ADAM SGS after use by an application"
             '("STATUS"))

(new-routine "SGS_DEFCH"
             "Define valid choice keys"
             '("CHOSTR"))

(new-routine "SGS_DISCU"
             "Disable sample cursor"
             ( ))

(new-routine "SGS_ENSCU"
             "Enable sample cursor"
             ( ))

(new-routine "SGS_FLUSH"
             "Flush buffers"
             ( ))

(new-routine "SGS_ICUAV"
             "Inquire cursor availability"
             '("AVAIL"))

(new-routine "SGS_ICURW"
             "Inquire current workstation"
             '("IWKID"))

(new-routine "SGS_ICURZ"
             "Inquire current zone"
             '("IZONID"))

(new-routine "SGS_IDUN"
             "Inquire device units"
             '("DXW" "DYW"))

(new-routine "SGS_INCHO"
             "Inquire number of choices"
             '("NCHOIC" "N"))

(new-routine "SGS_INIT"
             "Initialise SGS and (if necessary) open GKS"
             '("LUN" "STATUS"))

(new-routine "SGS_IPEN"
             "Inquire pen number"
             '("NPEN"))

(new-routine "SGS_IPLXY"
             "Inquire polyline x & y"
             '("X" "Y"))

(new-routine "SGS_ISLER"
             "Inquire selective erase capability"
             '("BLKER"))

(new-routine "SGS_ITXA"
             "Inquire text attributes"
             '("NF" "NPR" "HT" "AR" "XU" "YU" "SP" "TXJ"))

(new-routine "SGS_ITXB"
             "Inquire text buffer"
             '("X" "Y" "N" "DX" "DY"))

(new-routine "SGS_IZONE"
             "Inquire zone attributes"
             '("X1" "X2" "Y1" "Y2" "XM" "YM"))

(new-routine "SGS_LINE"
             "Begin polyline with a single line"
             '("X1" "Y1" "X2" "Y2"))

(new-routine "SGS_MARK"
             "Draw marker"
             '("X" "Y" "MTYPE"))

(new-routine "SGS_MARKL"
             "Draw marker at end of polyline"
             '("MTYPE"))

(new-routine "SGS_OPEN"
             "Open graphics (including GKS if necessary)"
             '("WKSTN" "IZONID" "STATUS"))

(new-routine "SGS_OPNWK"
             "Open workstation"
             '("WKSTN" "IZONID" "STATUS"))

(new-routine "SGS_OPOLY"
             "Output buffered polyline"
             ( ))

(new-routine "SGS_OTEXT"
             "Output buffered text"
             ( ))

(new-routine "SGS_RELZ"
             "Release zone"
             '("IZONID"))

(new-routine "SGS_REQCH"
             "Request choice"
             '("N"))

(new-routine "SGS_REQCU"
             "Request cursor position"
             '("X" "Y" "N"))

(new-routine "SGS_SAMCU"
             "Sample cursor"
             '("X" "Y"))

(new-routine "SGS_SARTX"
             "Set aspect ratio of text"
             '("R"))

(new-routine "SGS_SELCH"
             "Select choice device"
             '("NCHDEV"))

(new-routine "SGS_SELZ"
             "Select zone"
             '("IZONID" "STATUS"))

(new-routine "SGS_SETCU"
             "Set cursor position"
             '("X" "Y"))

(new-routine "SGS_SFONT"
             "Set font of text"
             '("NF"))

(new-routine "SGS_SHTX"
             "Set height of text"
             '("HT"))

(new-routine "SGS_SPEN"
             "Select pen"
             '("NPEN"))

(new-routine "SGS_SPREC"
             "Set precision of text"
             '("NPR"))

(new-routine "SGS_SSPTX"
             "Set spacing of text"
             '("SP"))

(new-routine "SGS_STXJ"
             "Set text justification"
             '("TXJ"))

(new-routine "SGS_SUPTX"
             "Set up vector of text"
             '("XU" "YU"))

(new-routine "SGS_SW"
             "Set window"
             '("X1" "X2" "Y1" "Y2" "STATUS"))

(new-routine "SGS_TPZ"
             "Transform position to new zone"
             '("IZIN" "XIN" "YIN" "IZOUT" "XOUT" "YOUT" "STATUS"))

(new-routine "SGS_TX"
             "Begin a new text string with a string"
             '("X" "Y" "STRING"))

(new-routine "SGS_TXI"
             "Begin text with an integer"
             '("X" "Y" "I" "NFI"))

(new-routine "SGS_TXR"
             "Begin new text with a real"
             '("X" "Y" "R" "NFI" "NDP"))

(new-routine "SGS_WIDEN"
             "Translate SGS workstation name to GKS"
             '("WKSTN" "ITYPE" "ICONID" "STATUS"))

(new-routine "SGS_WLIST"
             "List available workstations"
             '("LUN"))

(new-routine "SGS_WNAME"
             "Generate list of workstation names"
             '("ACTROU" "IARG" "STATUS"))

(new-routine "SGS_ZONE"
             "Create a zone"
             '("X1" "X2" "Y1" "Y2" "IZONID" "STATUS"))

(new-routine "SGS_ZPART"
             "Partition a zone"
             '("NX" "NY" "IZONES" "STATUS"))

(new-routine "SGS_ZSHAP"
             "Create a zone of a given shape"
             '("AR" "POS" "IZONID" "STATUS"))

(new-routine "SGS_ZSIZE"
             "Create a zone of a given size"
             '("XM" "YM" "POS" "IZONID" "STATUS"))
