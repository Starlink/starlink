(new-routine "TRN_ANNUL"
             "Annul compiled mapping"
             '("ID" "STATUS"))

(new-routine "TRN_APND"
             "Append transformation"
             '("LOCTR1" "LOCTR2" "STATUS"))

(new-routine "TRN_CLOSE"
             "Close the TRANSFORM facility"
             '("STATUS"))

(new-routine "TRN_COMP"
             "Compile transformation"
             '("LOCTR" "FORWD" "ID" "STATUS"))

(new-routine "TRN_ERR"
             "TRN__ error code definitions (include file)"
             ( ))

(new-routine "TRN_GTCL"
             "Get classification"
             '("LOCTR" "FORWD" "CLASS" "STATUS"))

(new-routine "TRN_GTCLC"
             "Get compiled classification"
             '("ID" "CLASS" "STATUS"))

(new-routine "TRN_GTNV"
             "Get numbers of variables"
             '("LOCTR" "NVIN" "NVOUT" "STATUS"))

(new-routine "TRN_GTNVC"
             "Get numbers of compiled variables"
             '("ID" "NVIN" "NVOUT" "STATUS"))

(new-routine "TRN_INV"
             "Invert transformation"
             '("LOCTR" "STATUS"))

(new-routine "TRN_JOIN"
             "Concatenate transformations"
             '("LOCTR1" "LOCTR2" "ELOC" "NAME" "LOCTR" "STATUS"))

(new-routine "TRN_NEW"
             "Create new transformation"
             '("NVIN" "NVOUT" "FOR" "INV" "PREC" "COMM" "ELOC" "NAME" "LOCTR" "STATUS"))

(new-routine "TRN_PAR"
             "TRN__ symbolic constant definitions (include file)"
             ( ))

(new-routine "TRN_PRFX"
             "Prefix transformation"
             '("LOCTR1" "LOCTR2" "STATUS"))

(new-routine "TRN_PTCL"
             "Put classification"
             '("CLASS" "LOCTR" "STATUS"))

(new-routine "TRN_STOK"
             "Substitute text for a token"
             '("TOKEN" "TVALUE" "TEXT" "NSUBS" "STATUS"))

(new-routine "TRN_STOKD"
             "Substitute a DOUBLE PRECISION value for a token"
             '("TOKEN" "DVALUE" "TEXT" "NSUBS" "STATUS"))

(new-routine "TRN_STOKI"
             "Substitute a INTEGER value for a token"
             '("TOKEN" "IVALUE" "TEXT" "NSUBS" "STATUS"))

(new-routine "TRN_STOKR"
             "Substitute a REAL value for a token"
             '("TOKEN" "RVALUE" "TEXT" "NSUBS" "STATUS"))

(new-routine "TRN_TR1D"
             "Transform 1-dimensional DOUBLE PRECISION coordinate data"
             '("BAD" "NX" "DXIN" "ID" "DXOUT" "STATUS"))

(new-routine "TRN_TR1I"
             "Transform 1-dimensional INTEGER coordinate data"
             '("BAD" "NX" "IXIN" "ID" "IXOUT" "STATUS"))

(new-routine "TRN_TR1R"
             "Transform 1-dimensional REAL coordinate data"
             '("BAD" "NX" "RXIN" "ID" "RXOUT" "STATUS"))

(new-routine "TRN_TR2D"
             "Transform 2-dimensional DOUBLE PRECISION coordinate data"
             '("BAD" "NXY" "DXIN" "DYIN" "ID" "DXOUT" "DYOUT" "STATUS"))

(new-routine "TRN_TR2I"
             "Transform 2-dimensional INTEGER coordinate data"
             '("BAD" "NXY" "IXIN" "IYIN" "ID" "IXOUT" "IYOUT" "STATUS"))

(new-routine "TRN_TR2R"
             "Transform 2-dimensional REAL coordinate data"
             '("BAD" "NXY" "RXIN" "RYIN" "ID" "RXOUT" "RYOUT" "STATUS"))

(new-routine "TRN_TRND"
             "Transform general DOUBLE PRECISION coordinate data"
             '("BAD" "ND1" "NCIN" "NDAT" "DDATA" "ID" "NR1" "NCOUT" "DRESLT" "STATUS"))

(new-routine "TRN_TRNI"
             "Transform general INTEGER coordinate data"
             '("BAD" "ND1" "NCIN" "NDAT" "IDATA" "ID" "NR1" "NCOUT" "IRESLT" "STATUS"))

(new-routine "TRN_TRNR"
             "Transform general REAL coordinate data"
             '("BAD" "ND1" "NCIN" "NDAT" "RDATA" "ID" "NR1" "NCOUT" "RRESLT" "STATUS"))

(new-routine "TRN__CLSIN"
             "Classification information invalid (error code)"
             ( ))

(new-routine "TRN__CMPER"
             "Compilation error (error code)"
             ( ))

(new-routine "TRN__CMTOF"
             "Compiled mapping table overflow (error code)"
             ( ))

(new-routine "TRN__CONDT"
             "'Constant scale factor' classification (symbolic constant)"
             ( ))

(new-routine "TRN__CONIN"
             "Constant syntax invalid (error code)"
             ( ))

(new-routine "TRN__DELIN"
             "Delimiting comma invalid (error code)"
             ( ))

(new-routine "TRN__DIAG"
             "'Preserves axes' classification (symbolic constant)"
             ( ))

(new-routine "TRN__DIMIN"
             "Dimensions invalid (error code)"
             ( ))

(new-routine "TRN__DSTIN"
             "Definition status invalid (error code)"
             ( ))

(new-routine "TRN__DUVAR"
             "Duplicate variable name (error code)"
             ( ))

(new-routine "TRN__EXPUD"
             "Expression undefined (error code)"
             ( ))

(new-routine "TRN__ICDIR"
             "Incompatible transformation directions (error code)"
             ( ))

(new-routine "TRN__INDEP"
             "'Preserves axis independence' classification (symbolic constant)"
             ( ))

(new-routine "TRN__ISOT"
             "'Preserves angles and shapes' classification (symbolic constant)"
             ( ))

(new-routine "TRN__LIN"
             "'Preserves straight lines' classification (symbolic constant)"
             ( ))

(new-routine "TRN__MAPUD"
             "Mapping undefined (error code)"
             ( ))

(new-routine "TRN__MIDIN"
             "Compiled mapping identifier invalid (error code)"
             ( ))

(new-routine "TRN__MIOPA"
             "Missing or invalid operand (error code)"
             ( ))

(new-routine "TRN__MIOPR"
             "Missing or invalid operator (error code)"
             ( ))

(new-routine "TRN__MISVN"
             "Missing variable name (error code)"
             ( ))

(new-routine "TRN__MLPAR"
             "Missing left parenthesis (error code)"
             ( ))

(new-routine "TRN__MRPAR"
             "Missing right parenthesis (error code)"
             ( ))

(new-routine "TRN__MXCLS"
             "Max. size of classification array (symbolic constant)"
             ( ))

(new-routine "TRN__NDCMM"
             "Number of data coordinates mis-matched (error code)"
             ( ))

(new-routine "TRN__NEGDT"
             "'Reflection is present' classification (symbolic constant)"
             ( ))

(new-routine "TRN__NMVMM"
             "Number of module variables mis-matched (error code)"
             ( ))

(new-routine "TRN__NOID"
             "Null identifier value (symbolic constant)"
             ( ))

(new-routine "TRN__NTVMM"
             "Number of transformation variables mis-matched (error code)"
             ( ))

(new-routine "TRN__NVRIN"
             "Number of variables invalid (error code)"
             ( ))

(new-routine "TRN__OPCIN"
             "Operation code invalid (error code)"
             ( ))

(new-routine "TRN__POSDT"
             "'Reflection is absent' classification (symbolic constant)"
             ( ))

(new-routine "TRN__PRCIN"
             "Precision invalid (error code)"
             ( ))

(new-routine "TRN__SZPRC"
             "Size of a precision string (symbolic constant)"
             ( ))

(new-routine "TRN__TOKIN"
             "Token name invalid (error code)"
             ( ))

(new-routine "TRN__TRNUD"
             "Transformation undefined (error code)"
             ( ))

(new-routine "TRN__TRUNC"
             "Character string truncated (error code)"
             ( ))

(new-routine "TRN__TYPIN"
             "Type invalid (error code)"
             ( ))

(new-routine "TRN__UNIDT"
             "'Volumes/areas preserved' classification (symbolic constant)"
             ( ))

(new-routine "TRN__VARIN"
             "Variable name invalid (error code)"
             ( ))

(new-routine "TRN__VARUD"
             "Variable name undefined (error code)"
             ( ))

(new-routine "TRN__VERMM"
             "Software version mis-match (error code)"
             ( ))

(new-routine "TRN__WRNFA"
             "Wrong number of function arguments (error code)"
             ( ))
