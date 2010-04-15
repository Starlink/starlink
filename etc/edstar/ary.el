(new-routine "ARY_ANNUL"
             "Annul an array identifier"
             '("IARY" "STATUS"))

(new-routine "ARY_BAD"
             "Determine if an array may contain bad pixels"
             '("IARY" "CHECK" "BAD" "STATUS"))

(new-routine "ARY_BASE"
             "Obtain an identifier for a base array"
             '("IARY1" "IARY2" "STATUS"))

(new-routine "ARY_BOUND"
             "Enquire the pixel-index bounds of an array"
             '("IARY" "NDIMX" "LBND" "UBND" "NDIM" "STATUS"))

(new-routine "ARY_CLONE"
             "Clone an array identifier"
             '("IARY1" "IARY2" "STATUS"))

(new-routine "ARY_CMPLX"
             "Determine whether an array holds complex values"
             '("IARY" "CMPLX" "STATUS"))

(new-routine "ARY_COPY"
             "Copy an array to a new location"
             '("IARY1" "PLACE" "IARY2" "STATUS"))

(new-routine "ARY_DELET"
             "Delete an array"
             '("IARY" "STATUS"))

(new-routine "ARY_DIM"
             "Enquire the dimension sizes of an array"
             '("IARY" "NDIMX" "DIM" "NDIM" "STATUS"))

(new-routine "ARY_DUPE"
             "Duplicate an array"
             '("IARY1" "PLACE" "IARY2" "STATUS"))

(new-routine "ARY_FIND"
             "Find an array in an HDS structure and import it into the ARY_ system"
             '("LOC" "NAME" "IARY" "STATUS"))

(new-routine "ARY_FORM"
             "Obtain the storage form of an array"
             '("IARY" "FORM" "STATUS"))

(new-routine "ARY_FTYPE"
             "Obtain the full data type of an array"
             '("IARY" "FTYPE" "STATUS"))

(new-routine "ARY_IMPRT"
             "Import an array into the ARY_ system from HDS"
             '("LOC" "IARY" "STATUS"))

(new-routine "ARY_ISACC"
             "Determine whether a specified type of array access is available"
             '("IARY" "ACCESS" "ISACC" "STATUS"))

(new-routine "ARY_ISBAS"
             "Enquire if an array is a base array"
             '("IARY" "BASE" "STATUS"))

(new-routine "ARY_ISMAP"
             "Determine if an array is currently mapped"
             '("IARY" "MAPPED" "STATUS"))

(new-routine "ARY_ISTMP"
             "Determine if an array is temporary"
             '("IARY" "TEMP" "STATUS"))

(new-routine "ARY_MAP"
             "Obtain mapped access to an array"
             '("IARY" "TYPE" "MMOD" "PNTR" "EL" "STATUS"))

(new-routine "ARY_MAPZ"
             "Obtain complex mapped access to an array"
             '("IARY" "TYPE" "MMOD" "RPNTR" "IPNTR" "EL" "STATUS"))

(new-routine "ARY_MSG"
             "Assign the name of an array to a message token"
             '("TOKEN" "IARY"))

(new-routine "ARY_NDIM"
             "Enquire the dimensionality of an array"
             '("IARY" "NDIM" "STATUS"))

(new-routine "ARY_NEW"
             "Create a new simple array"
             '("FTYPE" "NDIM" "LBND" "UBND" "PLACE" "IARY" "STATUS"))

(new-routine "ARY_NEWP"
             "Create a new primitive array"
             '("FTYPE" "NDIM" "UBND" "PLACE" "IARY" "STATUS"))

(new-routine "ARY_NOACC"
             "Disable a specified type of access to an array"
             '("ACCESS" "IARY" "STATUS"))

(new-routine "ARY_OFFS"
             "Obtain the pixel offset between two arrays"
             '("IARY1" "IARY2" "MXOFFS" "OFFS" "STATUS"))

(new-routine "ARY_PLACE"
             "Obtain an array placeholder"
             '("LOC" "NAME" "PLACE" "STATUS"))

(new-routine "ARY_RESET"
             "Reset an array to an undefined state"
             '("IARY" "STATUS"))

(new-routine "ARY_SAME"
             "Enquire if two arrays are part of the same base array"
             '("IARY1" "IARY2" "SAME" "ISECT" "STATUS"))

(new-routine "ARY_SBAD"
             "Set the bad-pixel flag for an array"
             '("BAD" "IARY" "STATUS"))

(new-routine "ARY_SBND"
             "Set new pixel-index bounds for an array"
             '("NDIM" "LBND" "UBND" "IARY" "STATUS"))

(new-routine "ARY_SECT"
             "Create an array section"
             '("IARY1" "NDIM" "LBND" "UBND" "IARY2" "STATUS"))

(new-routine "ARY_SHIFT"
             "Apply pixel-index shifts to an array"
             '("NSHIFT" "SHIFT" "IARY" "STATUS"))

(new-routine "ARY_SIZE"
             "Determine the size of an array"
             '("IARY" "NPIX" "STATUS"))

(new-routine "ARY_SSECT"
             "Create a similar array section to an existing one"
             '("IARY1" "IARY2" "IARY3" "STATUS"))

(new-routine "ARY_STATE"
             "Determine the state of an array (defined or undefined)"
             '("IARY" "STATE" "STATUS"))

(new-routine "ARY_STYPE"
             "Set a new type for an array"
             '("FTYPE" "IARY" "STATUS"))

(new-routine "ARY_TEMP"
             "Obtain a placeholder for a temporary array"
             '("PLACE" "STATUS"))

(new-routine "ARY_TRACE"
             "Set the internal ARY_ system error-tracing flag"
             '("NEWFLG" "OLDFLG"))

(new-routine "ARY_TYPE"
             "Obtain the numeric type of an array"
             '("IARY" "TYPE" "STATUS"))

(new-routine "ARY_UNMAP"
             "Unmap an array"
             '("IARY" "STATUS"))

(new-routine "ARY_VALID"
             "Determine whether an array identifier is valid"
             '("IARY" "VALID" "STATUS"))

(new-routine "ARY_VERFY"
             "Verify that an array's data structure is correctly constructed"
             '("IARY" "STATUS"))
