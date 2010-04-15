(new-routine "CMP_ERR"
             "CMP__ error code definitions (include file)"
             ( ))

(new-routine "CMP_GET0C"
             "Read primitive scalar component as CHARACTER"
             '("LOC" "NAME" "CVALUE" "STATUS"))

(new-routine "CMP_GET0D"
             "Read primitive scalar component as DOUBLE PRECISION"
             '("LOC" "NAME" "DVALUE" "STATUS"))

(new-routine "CMP_GET0I"
             "Read primitive scalar component as INTEGER"
             '("LOC" "NAME" "IVALUE" "STATUS"))

(new-routine "CMP_GET0L"
             "Read primitive scalar component as LOGICAL"
             '("LOC" "NAME" "LVALUE" "STATUS"))

(new-routine "CMP_GET0R"
             "Read primitive scalar component as REAL"
             '("LOC" "NAME" "RVALUE" "STATUS"))

(new-routine "CMP_GET1C"
             "Read primitive vector component as CHARACTER"
             '("LOC" "NAME" "ELX" "CVALUE" "EL" "STATUS"))

(new-routine "CMP_GET1D"
             "Read primitive vector component as DOUBLE PRECISION"
             '("LOC" "NAME" "ELX" "DVALUE" "EL" "STATUS"))

(new-routine "CMP_GET1I"
             "Read primitive vector component as INTEGER"
             '("LOC" "NAME" "ELX" "IVALUE" "EL" "STATUS"))

(new-routine "CMP_GET1L"
             "Read primitive vector component as LOGICAL"
             '("LOC" "NAME" "ELX" "LVALUE" "EL" "STATUS"))

(new-routine "CMP_GET1R"
             "Read primitive vector component as REAL"
             '("LOC" "NAME" "ELX" "RVALUE" "EL" "STATUS"))

(new-routine "CMP_GETNC"
             "Read primitive array component as CHARACTER "
             '("LOC" "NAME" "NDIM" "DIMX" "CVALUE" "DIM" "STATUS"))

(new-routine "CMP_GETND"
             "Read primitive array component as DOUBLE PRECISION "
             '("LOC" "NAME" "NDIM" "DIMX" "DVALUE" "DIM" "STATUS"))

(new-routine "CMP_GETNI"
             "Read primitive array component as INTEGER "
             '("LOC" "NAME" "NDIM" "DIMX" "IVALUE" "DIM" "STATUS"))

(new-routine "CMP_GETNL"
             "Read primitive array component as LOGICAL "
             '("LOC" "NAME" "NDIM" "DIMX" "LVALUE" "DIM" "STATUS"))

(new-routine "CMP_GETNR"
             "Read primitive array component as REAL "
             '("LOC" "NAME" "NDIM" "DIMX" "RVALUE" "DIM" "STATUS"))

(new-routine "CMP_GETVC"
             "Read primitive componant as CHARACTER as if vectorised"
             '("LOC" "NAME" "ELX" "CVALUE" "EL" "STATUS"))

(new-routine "CMP_GETVD"
             "Read primitive componant as DOUBLE PRECISION as if vectorised"
             '("LOC" "NAME" "ELX" "DVALUE" "EL" "STATUS"))

(new-routine "CMP_GETVI"
             "Read primitive componant as INTEGER as if vectorised"
             '("LOC" "NAME" "ELX" "IVALUE" "EL" "STATUS"))

(new-routine "CMP_GETVL"
             "Read primitive componant as LOGICAL as if vectorised"
             '("LOC" "NAME" "ELX" "LVALUE" "EL" "STATUS"))

(new-routine "CMP_GETVR"
             "Read primitive component as REAL as if vectorised"
             '("LOC" "NAME" "ELX" "RVALUE" "EL" "STATUS"))

(new-routine "CMP_LEN"
             "Enquire component precision"
             '("LOC" "NAME" "LEN" "STATUS"))

(new-routine "CMP_MAPN"
             "Map array component"
             '("LOC" "NAME" "TYPE" "MODE" "NDIM" "PNTR" "DIM" "STATUS"))

(new-routine "CMP_MAPV"
             "Map vectorised component"
             '("LOC" "NAME" "TYPE" "MODE" "PNTR" "EL" "STATUS"))

(new-routine "CMP_MOD"
             "Obtain component"
             '("LOC" "NAME" "TYPE" "NDIM" "DIM" "STATUS"))

(new-routine "CMP_MODC"
             "Obtain _CHAR component"
             '("LOC" "NAME" "LEN" "NDIM" "DIM" "STATUS"))

(new-routine "CMP_PRIM"
             "Enquire component primitive"
             '("LOC" "NAME" "REPLY" "STATUS"))

(new-routine "CMP_PUT0C"
             "Write CHARACTER data to a primitive scalar component"
             '("LOC" "NAME" "CVALUE" "STATUS"))

(new-routine "CMP_PUT0D"
             "Write DOUBLE PRECISION data to a primitive scalar component"
             '("LOC" "NAME" "DVALUE" "STATUS"))

(new-routine "CMP_PUT0I"
             "Write INTEGER data to a primitive scalar component"
             '("LOC" "NAME" "IVALUE" "STATUS"))

(new-routine "CMP_PUT0L"
             "Write LOGICAL data to a primitive scalar component"
             '("LOC" "NAME" "LVALUE" "STATUS"))

(new-routine "CMP_PUT0R"
             "Write REAL data to a primitive scalar component"
             '("LOC" "NAME" "RVALUE" "STATUS"))

(new-routine "CMP_PUT1C"
             "Write CHARACTER data to a primitive vector component"
             '("LOC" "NAME" "EL" "CVALUE" "STATUS"))

(new-routine "CMP_PUT1D"
             "Write DOUBLE PRECISION data to a primitive vector component"
             '("LOC" "NAME" "EL" "DVALUE" "STATUS"))

(new-routine "CMP_PUT1I"
             "Write INTEGER data to a primitive vector component"
             '("LOC" "NAME" "EL" "IVALUE" "STATUS"))

(new-routine "CMP_PUT1L"
             "Write LOGICAL data to a primitive vector component"
             '("LOC" "NAME" "EL" "LVALUE" "STATUS"))

(new-routine "CMP_PUT1R"
             "Write REAL data to a primitive vector component"
             '("LOC" "NAME" "EL" "RVALUE" "STATUS"))

(new-routine "CMP_PUTNC"
             "Write CHARACTER data to a primitive array component"
             '("LOC" "NAME" "NDIM" "DIMX" "CVALUE" "DIM" "STATUS"))

(new-routine "CMP_PUTND"
             "Write DOUBLE PRECISION data to a primitive array component"
             '("LOC" "NAME" "NDIM" "DIMX" "DVALUE" "DIM" "STATUS"))

(new-routine "CMP_PUTNI"
             "Write INTEGER data to a primitive array component"
             '("LOC" "NAME" "NDIM" "DIMX" "IVALUE" "DIM" "STATUS"))

(new-routine "CMP_PUTNL"
             "Write LOGICAL data to a primitive array component"
             '("LOC" "NAME" "NDIM" "DIMX" "LVALUE" "DIM" "STATUS"))

(new-routine "CMP_PUTNR"
             "Write REAL data to a primitive array component"
             '("LOC" "NAME" "NDIM" "DIMX" "RVALUE" "DIM" "STATUS"))

(new-routine "CMP_PUTVC"
             "Write CHARACTER data to a primitive component as if vectorised"
             '("LOC" "NAME" "EL" "CVALUE" "STATUS"))

(new-routine "CMP_PUTVD"
             "Write DOUBLE PRECISION data to a primitive component as if vectorised"
             '("LOC" "NAME" "EL" "DVALUE" "STATUS"))

(new-routine "CMP_PUTVI"
             "Write INTEGER data to a primitive component as if vectorised"
             '("LOC" "NAME" "EL" "IVALUE" "STATUS"))

(new-routine "CMP_PUTVL"
             "Write LOGICAL data to a primitive component as if vectorised"
             '("LOC" "NAME" "EL" "LVALUE" "STATUS"))

(new-routine "CMP_PUTVR"
             "Write REAL data to a primitive component as if vectorised"
             '("LOC" "NAME" "EL" "RVALUE" "STATUS"))

(new-routine "CMP_SHAPE"
             "Enquire component shape"
             '("LOC" "NAME" "NDIMX" "DIM" "NDIM" "STATUS"))

(new-routine "CMP_SIZE"
             "Enquire component size"
             '("LOC" "NAME" "SIZE" "STATUS"))

(new-routine "CMP_STRUC"
             "Enquire component structure"
             '("LOC" "NAME" "REPLY" "STATUS"))

(new-routine "CMP_TYPE"
             "Enquire component type"
             '("LOC" "NAME" "TYPE" "STATUS"))

(new-routine "CMP_UNMAP"
             "Unmap component"
             '("LOC" "NAME" "STATUS"))

(new-routine "CMP__DIMIN"
             "Dimensions invalid (error code)"
             ( ))

(new-routine "CMP__FATAL"
             "Fatal internal error (error code)"
             ( ))

(new-routine "CMP__ISMAP"
             "Data currently mapped (error code)"
             ( ))

(new-routine "CMP__NOMAP"
             "Not mapped (error code)"
             ( ))

(new-routine "CMP__TYPIN"
             "Type invalid (error code)"
             ( ))

(new-routine "DAT_ALTER"
             "Alter object size"
             '("LOC" "NDIM" "DIM" "STATUS"))

(new-routine "DAT_ANNUL"
             "Annul locator"
             '("LOC" "STATUS"))

(new-routine "DAT_ASSOC"
             "Return a locator associated with an ADAM parameter"
             '("PARAM" "MODE" "LOC" "STATUS"))

(new-routine "DAT_BASIC"
             "Map primitive as basic units"
             '("LOC" "MODE" "PNTR" "LEN" "STATUS"))

(new-routine "DAT_CANCL"
             "Cancel an ADAM parameter/data object association"
             '("PARAM" "STATUS"))

(new-routine "DAT_CCOPY"
             "Copy one structure level"
             '("LOC1" "LOC2" "NAME" "LOC3" "STATUS"))

(new-routine "DAT_CCTYP"
             "Create type string"
             '("SIZE" "TYPE"))

(new-routine "DAT_CELL"
             "Locate cell"
             '("LOC1" "NDIM" "SUB" "LOC2" "STATUS"))

(new-routine "DAT_CLEN"
             "Obtain character string length"
             '("LOC" "CLEN" "STATUS"))

(new-routine "DAT_CLONE"
             "Clone locator"
             '("LOC1" "LOC2" "STATUS"))

(new-routine "DAT_COERC"
             "Coerce object shape"
             '("LOC1" "NDIM" "LOC2" "STATUS"))

(new-routine "DAT_COPY"
             "Copy object"
             '("LOC1" "LOC2" "NAME" "STATUS"))

(new-routine "DAT_CREAT"
             "Create a data object via the ADAM parameter system"
             '("PARAM" "TYPE" "NDIM" "DIM" "STATUS"))

(new-routine "DAT_DEF"
             "Set an ADAM parameter system default to be a data object"
             '("PARAM" "LOC" "STATUS"))

(new-routine "DAT_DELET"
             "Delete an object via the ADAM parameter system"
             '("PARAM" "STATUS"))

(new-routine "DAT_DREP"
             "Obtain primitive data representation information"
             '("LOC" "FORMAT" "ORDER" "STATUS"))

(new-routine "DAT_ERASE"
             "Erase component"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_ERMSG"
             "Translate a status value into an error message"
             '("STATUS" "LENGTH" "MSG"))

(new-routine "DAT_ERR"
             "DAT__ error code definitions (include file)"
             ( ))

(new-routine "DAT_EXIST"
             "Associate an existing data object with an ADAM parameter"
             '("PARAM" "MODE" "LOC" "STATUS"))

(new-routine "DAT_FIND"
             "Find named component"
             '("LOC1" "NAME" "LOC2" "STATUS"))

(new-routine "DAT_GET"
             "Read primitive"
             '("LOC" "TYPE" "NDIM" "DIM" "VALUE" "STATUS"))

(new-routine "DAT_GET0C"
             "Read scalar primitive as CHARACTER"
             '("LOC" "CVALUE" "STATUS"))

(new-routine "DAT_GET0D"
             "Read scalar primitive as DOUBLE PRECISION"
             '("LOC" "DVALUE" "STATUS"))

(new-routine "DAT_GET0I"
             "Read scalar primitive as INTEGER"
             '("LOC" "IVALUE" "STATUS"))

(new-routine "DAT_GET0L"
             "Read scalar primitive as LOGICAL"
             '("LOC" "LVALUE" "STATUS"))

(new-routine "DAT_GET0R"
             "Read scalar primitive as REAL"
             '("LOC" "RVALUE" "STATUS"))

(new-routine "DAT_GET1C"
             "Read vector primitive as CHARACTER"
             '("LOC" "ELX" "CVALUE" "EL" "STATUS"))

(new-routine "DAT_GET1D"
             "Read vector primitive as DOUBLE PRECISION"
             '("LOC" "ELX" "DVALUE" "EL" "STATUS"))

(new-routine "DAT_GET1I"
             "Read vector primitive as INTEGER"
             '("LOC" "ELX" "IVALUE" "EL" "STATUS"))

(new-routine "DAT_GET1L"
             "Read vector primitive as LOGICAL"
             '("LOC" "ELX" "LVALUE" "EL" "STATUS"))

(new-routine "DAT_GET1R"
             "Read vector primitive as REAL"
             '("LOC" "ELX" "RVALUE" "EL" "STATUS"))

(new-routine "DAT_GETC"
             "Read primitive as CHARACTER"
             '("LOC" "NDIM" "DIM" "CVALUE" "STATUS"))

(new-routine "DAT_GETD"
             "Read primitive as DOUBLE PRECISION"
             '("LOC" "NDIM" "DIM" "DVALUE" "STATUS"))

(new-routine "DAT_GETI"
             "Read primitive as INTEGER"
             '("LOC" "NDIM" "DIM" "IVALUE" "STATUS"))

(new-routine "DAT_GETL"
             "Read primitive as LOGICAL"
             '("LOC" "NDIM" "DIM" "LVALUE" "STATUS"))

(new-routine "DAT_GETNC"
             "Read array primitive as CHARACTER"
             '("LOC" "NDIM" "DIMX" "CVALUE" "DIM" "STATUS"))

(new-routine "DAT_GETND"
             "Read array primitive as DOUBLE PRECISION"
             '("LOC" "NDIM" "DIMX" "DVALUE" "DIM" "STATUS"))

(new-routine "DAT_GETNI"
             "Read array primitive as INTEGER"
             '("LOC" "NDIM" "DIMX" "IVALUE" "DIM" "STATUS"))

(new-routine "DAT_GETNL"
             "Read array primitive as LOGICAL"
             '("LOC" "NDIM" "DIMX" "LVALUE" "DIM" "STATUS"))

(new-routine "DAT_GETNR"
             "Read array primitive as REAL"
             '("LOC" "NDIM" "DIMX" "RVALUE" "DIM" "STATUS"))

(new-routine "DAT_GETR"
             "Read primitive as REAL"
             '("LOC" "NDIM" "DIM" "RVALUE" "STATUS"))

(new-routine "DAT_GETVC"
             "Read primitive as CHARACTER as if vectorised"
             '("LOC" "ELX" "CVALUE" "EL" "STATUS"))

(new-routine "DAT_GETVD"
             "Read primitive as DOUBLE PRECISION as if vectorised"
             '("LOC" "ELX" "DVALUE" "EL" "STATUS"))

(new-routine "DAT_GETVI"
             "Read primitive as INTEGER as if vectorised"
             '("LOC" "ELX" "IVALUE" "EL" "STATUS"))

(new-routine "DAT_GETVL"
             "Read primitive as LOGICAL as if vectorised"
             '("LOC" "ELX" "LVALUE" "EL" "STATUS"))

(new-routine "DAT_GETVR"
             "Read primitive as REAL as if vectorised"
             '("LOC" "ELX" "RVALUE" "EL" "STATUS"))

(new-routine "DAT_INDEX"
             "Index into component list"
             '("LOC1" "INDEX" "LOC2" "STATUS"))

(new-routine "DAT_LEN"
             "Enquire primitive precision"
             '("LOC" "LEN" "STATUS"))

(new-routine "DAT_MAP"
             "Map primitive"
             '("LOC" "TYPE" "MODE" "NDIM" "DIM" "PNTR" "STATUS"))

(new-routine "DAT_MAPC"
             "Map primitive as CHARACTER"
             '("LOC" "MODE" "NDIM" "DIM" "PNTR" "STATUS"))

(new-routine "DAT_MAPD"
             "Map primitive as DOUBLE PRECISION"
             '("LOC" "MODE" "NDIM" "DIM" "PNTR" "STATUS"))

(new-routine "DAT_MAPI"
             "Map primitive as INTEGER"
             '("LOC" "MODE" "NDIM" "DIM" "PNTR" "STATUS"))

(new-routine "DAT_MAPL"
             "Map primitive as LOGICAL"
             '("LOC" "MODE" "NDIM" "DIM" "PNTR" "STATUS"))

(new-routine "DAT_MAPN"
             "Map array primitive"
             '("LOC" "TYPE" "MODE" "NDIM" "PNTR" "DIM" "STATUS"))

(new-routine "DAT_MAPR"
             "Map primitive as REAL"
             '("LOC" "MODE" "NDIM" "DIM" "PNTR" "STATUS"))

(new-routine "DAT_MAPV"
             "Map vectorised primitive"
             '("LOC" "TYPE" "MODE" "PNTR" "EL" "STATUS"))

(new-routine "DAT_MOULD"
             "Alter object shape"
             '("LOC" "NDIM" "DIM" "STATUS"))

(new-routine "DAT_MOVE"
             "Move object"
             '("LOC1" "LOC2" "NAME" "STATUS"))

(new-routine "DAT_MSG"
             "Assign the name of an HDS object to a message token"
             '("TOKEN" "LOC"))

(new-routine "DAT_NAME"
             "Enquire object name"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_NCOMP"
             "Enquire number of components"
             '("LOC" "NCOMP" "STATUS"))

(new-routine "DAT_NEW"
             "Create component"
             '("LOC" "NAME" "TYPE" "NDIM" "DIM" "STATUS"))

(new-routine "DAT_NEW0C"
             "Create scalar _CHAR component"
             '("LOC" "NAME" "LEN" "STATUS"))

(new-routine "DAT_NEW0D"
             "Create scalar _DOUBLE component"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_NEW0I"
             "Create scalar _INTEGER component"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_NEW0L"
             "Create scalar _LOGICAL component"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_NEW0R"
             "Create scalar _REAL component"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_NEW1C"
             "Create vector _CHAR component"
             '("LOC" "NAME" "LEN" "EL" "STATUS"))

(new-routine "DAT_NEW1D"
             "Create vector _DOUBLE component"
             '("LOC" "NAME" "EL" "STATUS"))

(new-routine "DAT_NEW1I"
             "Create vector _INTEGER component"
             '("LOC" "NAME" "EL" "STATUS"))

(new-routine "DAT_NEW1L"
             "Create vector _LOGICAL component"
             '("LOC" "NAME" "EL" "STATUS"))

(new-routine "DAT_NEW1R"
             "Create vector _REAL component"
             '("LOC" "NAME" "EL" "STATUS"))

(new-routine "DAT_NEWC"
             "Create _CHAR component"
             '("LOC" "NAME" "LEN" "NDIM" "DIM" "STATUS"))

(new-routine "DAT_PAR"
             "DAT__ symbolic constant definitions (include file)"
             ( ))

(new-routine "DAT_PAREN"
             "Locate parent structure"
             '("LOC1" "LOC2" "STATUS"))

(new-routine "DAT_PREC"
             "Enquire storage precision"
             '("LOC" "NBYTE" "STATUS"))

(new-routine "DAT_PRIM"
             "Enquire object primitive"
             '("LOC" "REPLY" "STATUS"))

(new-routine "DAT_PUT"
             "Write primitive"
             '("LOC" "TYPE" "NDIM" "DIM" "VALUE" "STATUS"))

(new-routine "DAT_PUT0C"
             "Write CHARACTER data to a scalar primitive"
             '("LOC" "CVALUE" "STATUS"))

(new-routine "DAT_PUT0D"
             "Write DOUBLE PRECISION data to a scalar primitive"
             '("LOC" "DVALUE" "STATUS"))

(new-routine "DAT_PUT0I"
             "Write INTEGER data to a scalar primitive"
             '("LOC" "IVALUE" "STATUS"))

(new-routine "DAT_PUT0L"
             "Write LOGICAL data to a scalar primitive"
             '("LOC" "LVALUE" "STATUS"))

(new-routine "DAT_PUT0R"
             "Write REAL data to a scalar primitive"
             '("LOC" "RVALUE" "STATUS"))

(new-routine "DAT_PUT1C"
             "Write CHARACTER data to a vector primitive"
             '("LOC" "EL" "CVALUE" "STATUS"))

(new-routine "DAT_PUT1D"
             "Write DOUBLE PRECISION data to a vector primitive"
             '("LOC" "EL" "DVALUE" "STATUS"))

(new-routine "DAT_PUT1I"
             "Write INTEGER data to a vector primitive"
             '("LOC" "EL" "IVALUE" "STATUS"))

(new-routine "DAT_PUT1L"
             "Write LOGICAL data to a vector primitive"
             '("LOC" "EL" "LVALUE" "STATUS"))

(new-routine "DAT_PUT1R"
             "Write REAL data to a vector primitive"
             '("LOC" "EL" "RVALUE" "STATUS"))

(new-routine "DAT_PUTC"
             "Write CHARACTER data to a primitive"
             '("LOC" "NDIM" "DIM" "CVALUE" "STATUS"))

(new-routine "DAT_PUTD"
             "Write DOUBLE PRECISION data to a primitive"
             '("LOC" "NDIM" "DIM" "DVALUE" "STATUS"))

(new-routine "DAT_PUTI"
             "Write INTEGER data to a primitive"
             '("LOC" "NDIM" "DIM" "IVALUE" "STATUS"))

(new-routine "DAT_PUTL"
             "Write LOGICAL data to a primitive"
             '("LOC" "NDIM" "DIM" "LVALUE" "STATUS"))

(new-routine "DAT_PUTNC"
             "Write CHARACTER data to an array primitive"
             '("LOC" "NDIM" "DIMX" "CVALUE" "DIM" "STATUS"))

(new-routine "DAT_PUTND"
             "Write DOUBLE PRECISION data to an array primitive"
             '("LOC" "NDIM" "DIMX" "DVALUE" "DIM" "STATUS"))

(new-routine "DAT_PUTNI"
             "Write INTEGER data to an array primitive"
             '("LOC" "NDIM" "DIMX" "IVALUE" "DIM" "STATUS"))

(new-routine "DAT_PUTNL"
             "Write LOGICAL data to an array primitive"
             '("LOC" "NDIM" "DIMX" "LVALUE" "DIM" "STATUS"))

(new-routine "DAT_PUTNR"
             "Write REAL data to an array primitive"
             '("LOC" "NDIM" "DIMX" "RVALUE" "DIM" "STATUS"))

(new-routine "DAT_PUTR"
             "Write REAL data to a primitive"
             '("LOC" "NDIM" "DIM" "RVALUE" "STATUS"))

(new-routine "DAT_PUTVC"
             "Write CHARACTER data to a primitive as if vectorised"
             '("LOC" "EL" "CVALUE" "STATUS"))

(new-routine "DAT_PUTVD"
             "Write DOUBLE PRECISION data to a primitive as if vectorised"
             '("LOC" "EL" "DVALUE" "STATUS"))

(new-routine "DAT_PUTVI"
             "Write INTEGER data to a primitive as if vectorised"
             '("LOC" "EL" "IVALUE" "STATUS"))

(new-routine "DAT_PUTVL"
             "Write LOGICAL data to a primitive as if vectorised"
             '("LOC" "EL" "LVALUE" "STATUS"))

(new-routine "DAT_PUTVR"
             "Write REAL data to a primitive as if vectorised"
             '("LOC" "EL" "RVALUE" "STATUS"))

(new-routine "DAT_REF"
             "Obtain a reference for an HDS object"
             '("LOC" "REF" "LREF" "STATUS"))

(new-routine "DAT_RENAM"
             "Rename object"
             '("LOC" "NAME" "STATUS"))

(new-routine "DAT_RESET"
             "Reset object state"
             '("LOC" "STATUS"))

(new-routine "DAT_RETYP"
             "Change object type"
             '("LOC" "TYPE" "STATUS"))

(new-routine "DAT_SHAPE"
             "Enquire object shape"
             '("LOC" "NDIMX" "DIM" "NDIM" "STATUS"))

(new-routine "DAT_SIZE"
             "Enquire object size"
             '("LOC" "SIZE" "STATUS"))

(new-routine "DAT_SLICE"
             "Locate slice"
             '("LOC1" "NDIM" "DIML" "DIMU" "LOC2" "STATUS"))

(new-routine "DAT_STATE"
             "Enquire object state"
             '("LOC" "REPLY" "STATUS"))

(new-routine "DAT_STRUC"
             "Enquire object structure"
             '("LOC" "REPLY" "STATUS"))

(new-routine "DAT_TEMP"
             "Create temporary object"
             '("TYPE" "NDIM" "DIM" "LOC" "STATUS"))

(new-routine "DAT_THERE"
             "Enquire component existence"
             '("LOC" "NAME" "REPLY" "STATUS"))

(new-routine "DAT_TYPE"
             "Enquire object type"
             '("LOC" "TYPE" "STATUS"))

(new-routine "DAT_UNMAP"
             "Unmap object"
             '("LOC" "STATUS"))

(new-routine "DAT_UPDAT"
             "Force update of an HDS file via the ADAM parameter system"
             '("PARAM" "STATUS"))

(new-routine "DAT_VALID"
             "Enquire locator valid"
             '("LOC" "REPLY" "STATUS"))

(new-routine "DAT_VEC"
             "Vectorise object"
             '("LOC1" "LOC2" "STATUS"))

(new-routine "DAT_WHERE"
             "Find position of primitive in HDS file"
             '("LOC" "BLOCK" "OFFSET" "STATUS"))

(new-routine "DAT__ACCON"
             "Access conflict (error code)"
             ( ))

(new-routine "DAT__BOUND"
             "Outside object bounds (error code)"
             ( ))

(new-routine "DAT__COMEX"
             "Component already exists (error code)"
             ( ))

(new-routine "DAT__CONER"
             "Conversion error (error code)"
             ( ))

(new-routine "DAT__DELIN"
             "Deletion invalid (error code)"
             ( ))

(new-routine "DAT__DIMIN"
             "Dimensions invalid (error code)"
             ( ))

(new-routine "DAT__FATAL"
             "Fatal internal error (error code)"
             ( ))

(new-routine "DAT__FILCK"
             "File locking error (error code)"
             ( ))

(new-routine "DAT__FILCL"
             "File close error (error code)"
             ( ))

(new-routine "DAT__FILCR"
             "File create error (error code)"
             ( ))

(new-routine "DAT__FILIN"
             "File invalid (error code)"
             ( ))

(new-routine "DAT__FILMP"
             "File mapping error (error code)"
             ( ))

(new-routine "DAT__FILND"
             "File not deleted (error code)"
             ( ))

(new-routine "DAT__FILNF"
             "File not found (error code)"
             ( ))

(new-routine "DAT__FILNX"
             "File not extended (error code)"
             ( ))

(new-routine "DAT__FILPR"
             "File protected (error code)"
             ( ))

(new-routine "DAT__FILRD"
             "File read error (error code)"
             ( ))

(new-routine "DAT__FILWR"
             "File write (error code)"
             ( ))

(new-routine "DAT__GRPIN"
             "Group invalid (error code)"
             ( ))

(new-routine "DAT__INCHK"
             "Integrity check (error code)"
             ( ))

(new-routine "DAT__ISMAP"
             "Data currently mapped (error code)"
             ( ))

(new-routine "DAT__LOCIN"
             "Locator invalid (error code)"
             ( ))

(new-routine "DAT__MODIN"
             "Mode invalid (error code)"
             ( ))

(new-routine "DAT__MXDIM"
             "Maximum number of HDS object dimensions (symbolic constant)"
             ( ))

(new-routine "DAT__NAMIN"
             "Name invalid (error code)"
             ( ))

(new-routine "DAT__NOLOC"
             "Null locator value (symbolic constant)"
             ( ))

(new-routine "DAT__OBJIN"
             "Object invalid (error code)"
             ( ))

(new-routine "DAT__OBJNF"
             "Object not found (error code)"
             ( ))

(new-routine "DAT__PRMAP"
             "Primitive data mapped (error code)"
             ( ))

(new-routine "DAT__SUBIN"
             "Subscripts invalid (error code)"
             ( ))

(new-routine "DAT__SZLOC"
             "Size of HDS locator (symbolic constant)"
             ( ))

(new-routine "DAT__SZMOD"
             "Size of HDS access mode string (symbolic constant)"
             ( ))

(new-routine "DAT__SZNAM"
             "Size of HDS name string (symbolic constant)"
             ( ))

(new-routine "DAT__SZTYP"
             "Size of HDS data type string (symbolic constant)"
             ( ))

(new-routine "DAT__TRUNC"
             "Text truncated (error code)"
             ( ))

(new-routine "DAT__TYPIN"
             "Type invalid (error code)"
             ( ))

(new-routine "DAT__UNSET"
             "Primitive data undefined (error code)"
             ( ))

(new-routine "DAT__VERMM"
             "Version mismatch (error code)"
             ( ))

(new-routine "HDS_CLOSE"
             "Close container file"
             '("LOC" "STATUS"))

(new-routine "HDS_COPY"
             "Copy an object to a new container file"
             '("LOC" "FILE" "NAME" "STATUS"))

(new-routine "HDS_ERASE"
             "Erase container file"
             '("LOC" "STATUS"))

(new-routine "HDS_FLUSH"
             "Flush locator group"
             '("GROUP" "STATUS"))

(new-routine "HDS_FREE"
             "Free container file"
             '("LOC" "STATUS"))

(new-routine "HDS_GROUP"
             "Enquire locator group"
             '("LOC" "GROUP" "STATUS"))

(new-routine "HDS_GTUNE"
             "Obtain tuning parameter value"
             '("PARAM" "VALUE" "STATUS"))

(new-routine "HDS_LINK"
             "Link locator to a group"
             '("LOC" "GROUP" "STATUS"))

(new-routine "HDS_LOCK"
             "Lock container file"
             '("LOC" "STATUS"))

(new-routine "HDS_NEW"
             "Create container file"
             '("FILE" "NAME" "TYPE" "NDIM" "DIM" "LOC" "STATUS"))

(new-routine "HDS_OPEN"
             "Open container file"
             '("FILE" "MODE" "LOC" "STATUS"))

(new-routine "HDS_RUN"
             "Run an HDS application subroutine"
             '("APP" "STATUS"))

(new-routine "HDS_SHOW"
             "Show HDS statistics"
             '("TOPIC" "STATUS"))

(new-routine "HDS_START"
             "Start up HDS"
             '("STATUS"))

(new-routine "HDS_STATE"
             "Enquire the current state of HDS"
             '("STATE" "STATUS"))

(new-routine "HDS_STOP"
             "Close down HDS"
             '("STATUS"))

(new-routine "HDS_TRACE"
             "Trace object path"
             '("LOC" "NLEV" "PATH" "FILE" "STATUS"))

(new-routine "HDS_TUNE"
             "Set HDS parameter"
             '("PARAM" "VALUE" "STATUS"))
