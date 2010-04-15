(let (new-ndf-routine)

;;; Define a local function to create a token for an NDF routine with
;;; associated argumant placeholders and a helpkey.
  (fset 'new-ndf-routine
        (lambda (name desc args &optional help)
          (if (not help) (setq help name))         ; use routine name to find
                                                   ; help by default
          (new-routine name desc args
                       (list (cons 'helpkey name)) ; routine name is helpkey so
                                                   ; put this into plist
                       "starfort")
          (new-helpkey name (list "sun33" help)    ; this is where the external
                                                   ; help can be found
                       nil "starfort")))


  (new-ndf-routine "NDF_ACGET"
                   "Obtain the value of an NDF axis character component"
                   '("indf" "comp" "iaxis" "value" "status"))

  (new-ndf-routine "NDF_ACLEN"
                   "Determine the length of an NDF axis character component"
                   '("indf" "comp" "iaxis" "length" "status"))

  (new-ndf-routine "NDF_ACMSG"
                   "Assign the value of an NDF axis character component to a message token"
                   '("token" "indf" "comp" "iaxis" "status"))

  (new-ndf-routine "NDF_ACPUT"
                   "Assign a value to an NDF axis character component"
                   '("value" "indf" "comp" "iaxis" "status"))

  (new-ndf-routine "NDF_ACRE"
                   "Ensure that an axis coordinate system exists for an NDF"
                   '("indf" "status"))

  (new-ndf-routine "NDF_AFORM"
                   "Obtain the storage form of an NDF axis array"
                   '("indf" "comp" "iaxis" "form" "status"))

  (new-ndf-routine "NDF_AMAP"
                   "Obtain mapped access to an NDF axis array"
                   '("indf" "comp" "iaxis" "type" "mmod" "pntr" "el" "status"))

  (new-ndf-routine "NDF_ANNUL"
                   "Annul an NDF identifier"
                   '("indf" "status"))

  (new-ndf-routine "NDF_ANORM"
                   "Obtain the logical value of an NDF axis normalisation flag"
                   '("indf" "iaxis" "norm" "status"))

  (new-ndf-routine "NDF_AREST"
                   "Reset an NDF axis component to an undefined state"
                   '("indf" "comp" "iaxis" "status"))

  (new-ndf-routine "NDF_ASNRM"
                   "Set a new logical value for an NDF axis normalisation flag"
                   '("norm" "indf" "iaxis" "status"))

  (new-ndf-routine "NDF_ASSOC"
                   "Associate an existing NDF with an ADAM parameter"
                   '("param" "mode" "indf" "status"))

  (new-ndf-routine "NDF_ASTAT"
                   "Determine the state of an NDF axis component (defined or undefined)"
                   '("indf" "comp" "iaxis" "state" "status"))

  (new-ndf-routine "NDF_ASTYP"
                   "Set a new numeric type for an NDF axis array"
                   '("type" "indf" "comp" "iaxis" "status"))

  (new-ndf-routine "NDF_ATYPE"
                   "Obtain the numeric type of an NDF axis array"
                   '("indf" "comp" "iaxis" "type" "status"))

  (new-ndf-routine "NDF_AUNMP"
                   "Unmap an NDF axis array"
                   '("indf" "comp" "iaxis" "status"))

  (new-ndf-routine "NDF_BAD"
                   "Determine if an NDF array component may contain bad pixels"
                   '("indf" "comp" "check" "bad" "status"))

  (new-ndf-routine "NDF_BASE"
                   "Obtain an identifier for a base NDF"
                   '("indf1" "indf2" "status"))

  (new-ndf-routine "NDF_BB"
                   "Obtain the bad-bits mask value for the quality component of an NDF"
                   '("indf" "badbit" "status"))

  (new-ndf-routine "NDF_BEGIN"
                   "Begin a new NDF context"
                   ( ))

  (new-ndf-routine "NDF_BLOCK"
                   "Obtain an NDF section containing a block of adjacent pixels"
                   '("indf1" "ndim" "mxdim" "iblock" "indf2" "status"))

  (new-ndf-routine "NDF_BOUND"
                   "Enquire the pixel-index bounds of an NDF"
                   '("indf" "ndimx" "lbnd" "ubnd" "ndim" "status"))

  (new-ndf-routine "NDF_CGET"
                   "Obtain the value of an NDF character component"
                   '("indf" "comp" "value" "status"))

  (new-ndf-routine "NDF_CHUNK"
                   "Obtain an NDF section containing a chunk of contiguous pixels"
                   '("indf1" "mxpix" "ichunk" "indf2" "status"))

  (new-ndf-routine "NDF_CINP"
                   "Obtain an NDF character component value via the ADAM parameter system"
                   '("param" "indf" "comp" "status"))

  (new-ndf-routine "NDF_CLEN"
                   "Determine the length of an NDF character component"
                   '("indf" "comp" "length" "status"))

  (new-ndf-routine "NDF_CLONE"
                   "Clone an NDF identifier"
                   '("indf1" "indf2" "status"))

  (new-ndf-routine "NDF_CMPLX"
                   "Determine whether an NDF array component holds complex values"
                   '("indf" "comp" "cmplx" "status"))

  (new-ndf-routine "NDF_CMSG"
                   "Assign the value of an NDF character component to a message token"
                   '("token" "indf" "comp" "status"))

  (new-ndf-routine "NDF_COPY"
                   "Copy an NDF to a new location"
                   '("indf1" "place" "indf2" "status"))

  (new-ndf-routine "NDF_CPUT"
                   "Assign a value to an NDF character component"
                   '("value" "indf" "comp" "status"))

  (new-ndf-routine "NDF_CREAT"
                   "Create a new simple NDF via the ADAM parameter system"
                   '("param" "ftype" "ndim" "lbnd" "ubnd" "indf" "status"))

  (new-ndf-routine "NDF_CREP"
                   "Create a new primitive NDF via the ADAM parameter system"
                   '("param" "ftype" "ndim" "ubnd" "indf" "status"))

  (new-ndf-routine "NDF_DELET"
                   "Delete an NDF"
                   '("indf" "status"))

  (new-ndf-routine "NDF_DIM"
                   "Enquire the dimension sizes of an NDF"
                   '("indf" "ndimx" "dim" "ndim" "status"))

  (new-ndf-routine "NDF_END"
                   "End the current NDF context"
                   '("status"))

  (new-ndf-routine "NDF_EXIST"
                   "See if an existing NDF is associated with an ADAM parameter"
                   '("param" "mode" "indf" "status"))

  (new-ndf-routine "NDF_FIND"
                   "Find an NDF in an HDS structure and import it into the NDF_ system"
                   '("loc" "name" "indf" "status"))

  (new-ndf-routine "NDF_FORM"
                   "Obtain the storage form of an NDF array component"
                   '("indf" "comp" "form" "status"))

  (new-ndf-routine "NDF_FTYPE"
                   "Obtain the full data type of an NDF array component"
                   '("indf" "comp" "ftype" "status"))

  (new-ndf-routine "NDF_GTUNE"
                   "Obtain the value of an NDF_ system tuning parameter"
                   '("tpar" "value" "status"))

  (new-ndf-routine "NDF_IMPRT"
                   "Import an NDF into the NDF_ system from HDS"
                   '("loc" "indf" "status"))

  (new-ndf-routine "NDF_ISACC"
                   "Determine whether a specified type of NDF access is available"
                   '("indf" "access" "isacc" "status"))

  (new-ndf-routine "NDF_ISBAS"
                   "Enquire if an NDF is a base NDF"
                   '("indf" "isbas" "status"))

  (new-ndf-routine "NDF_ISTMP"
                   "Enquire if an NDF is temporary"
                   '("indf" "istmp" "status"))

  (new-ndf-routine "NDF_LOC"
                   "Obtain an HDS locator for an NDF"
                   '("indf" "mode" "loc" "status"))

  (new-ndf-routine "NDF_MAP"
                   "Obtain mapped access to an array component of an NDF"
                   '("indf" "comp" "type" "mmod" "pntr" "el" "status"))

  (new-ndf-routine "NDF_MAPQL"
                   "Map the quality component of an NDF as an array of logical values"
                   '("indf" "pntr" "el" "bad" "status"))

  (new-ndf-routine "NDF_MAPZ"
                   "Obtain complex mapped access to an array component of an NDF"
                   '("indf" "comp" "type" "mmod" "rpntr" "ipntr" "el" "status"))

  (new-ndf-routine "NDF_MBAD"
                   "Merge the bad-pixel flags of the array components of a pair of NDFs"
                   '("badok" "indf1" "indf2" "comp" "check" "bad" "status"))

  (new-ndf-routine "NDF_MBADN"
                   "Merge the bad-pixel flags of the array components of a number of NDFs"
                   '("badok" "n" "ndfs" "comp" "check" "bad" "status"))

  (new-ndf-routine "NDF_MBND"
                   "Match the pixel-index bounds of a pair of NDFs"
                   '("option" "indf1" "indf2" "status"))

  (new-ndf-routine "NDF_MBNDN"
                   "Match the pixel-index bounds of a number of NDFs"
                   '("option" "n" "ndfs" "status"))

  (new-ndf-routine "NDF_MSG"
                   "Assign the name of an NDF to a message token"
                   '("token" "indf"))

  (new-ndf-routine "NDF_MTYPE"
                   "Match the types of the array components of a pair of NDFs"
                   '("typlst" "indf1" "indf2" "comp" "itype" "dtype" "status"))

  (new-ndf-routine "NDF_MTYPN"
                   "Match the types of the array components of a number of NDFs"
                   '("typlst" "n" "ndfs" "comp" "itype" "dtype" "status"))

  (new-ndf-routine "NDF_NBLOC"
                   "Determine the number of blocks of adjacent pixels in an NDF"
                   '("indf" "ndim" "mxdim" "nblock" "status"))

  (new-ndf-routine "NDF_NCHNK"
                   "Determine the number of chunks of contiguous pixels in an NDF"
                   '("indf" "mxpix" "nchunk" "status"))

  (new-ndf-routine "NDF_NEW"
                   "Create a new simple NDF"
                   '("ftype" "ndim" "lbnd" "ubnd" "place" "indf" "status"))

  (new-ndf-routine "NDF_NEWP"
                   "Create a new primitive NDF"
                   '("ftype" "ndim" "ubnd" "place" "indf" "status"))

  (new-ndf-routine "NDF_NOACC"
                   "Disable a specified type of access to an NDF"
                   '("access" "indf" "status"))

  (new-ndf-routine "NDF_PLACE"
                   "Obtain an NDF placeholder"
                   '("loc" "name" "place" "status"))

  (new-ndf-routine "NDF_PROP"
                   "Propagate NDF information to create a new NDF via the ADAM parameter system"
                   '("indf1" "clist" "param" "indf2" "status"))

  (new-ndf-routine "NDF_QMASK"
                   "Combine an NDF quality value with a bad-bits mask to give a logical result"
                   '("qual" "badbit"))

  (new-ndf-routine "NDF_QMF"
                   "Obtain the logical value of an NDF's quality masking flag"
                   '("indf" "qmf" "status"))

  (new-ndf-routine "NDF_RESET"
                   "Reset an NDF component to an undefined state"
                   '("indf" "comp" "status"))

  (new-ndf-routine "NDF_SAME"
                   "Enquire if two NDFs are part of the same base NDF"
                   '("indf1" "indf2" "same" "isect" "status"))

  (new-ndf-routine "NDF_SBAD"
                   "Set the bad-pixel flag for an NDF array component"
                   '("bad" "indf" "comp" "status"))

  (new-ndf-routine "NDF_SBB"
                   "Set a bad-bits mask value for the quality component of an NDF"
                   '("badbit" "indf" "status"))

  (new-ndf-routine "NDF_SBND"
                   "Set new pixel-index bounds for an NDF"
                   '("ndim" "lbnd" "ubnd" "indf" "status"))

  (new-ndf-routine "NDF_SECT"
                   "Create an NDF section"
                   '("indf1" "ndim" "lbnd" "ubnd" "indf2" "status"))

  (new-ndf-routine "NDF_SHIFT"
                   "Apply pixel-index shifts to an NDF"
                   '("nshift" "shift" "indf" "status"))

  (new-ndf-routine "NDF_SIZE"
                   "Determine the size of an NDF"
                   '("indf" "npix" "status"))

  (new-ndf-routine "NDF_SQMF"
                   "Set a new logical value for an NDF's quality masking flag"
                   '("qmf" "indf" "status"))

  (new-ndf-routine "NDF_SSARY"
                   "Create an array section, using an NDF section as a template"
                   '("iary1" "indf" "iary2" "status"))

  (new-ndf-routine "NDF_STATE"
                   "Determine the state of an NDF component (defined or undefined)"
                   '("indf" "comp" "state" "status"))

  (new-ndf-routine "NDF_STYPE"
                   "Set a new type for an NDF array component"
                   '("ftype" "indf" "comp" "status"))

  (new-ndf-routine "NDF_TEMP"
                   "Obtain a placeholder for a temporary NDF"
                   '("place" "status"))

  (new-ndf-routine "NDF_TRACE"
                   "Set the internal NDF_ system error-tracing flag"
                   '("newflg" "oldflg"))

  (new-ndf-routine "NDF_TUNE"
                   "Set an NDF_ system tuning parameter"
                   '("value" "tpar" "status"))

  (new-ndf-routine "NDF_TYPE"
                   "Obtain the numeric type of an NDF array component"
                   '("indf" "comp" "type" "status"))

  (new-ndf-routine "NDF_UNMAP"
                   "Unmap an NDF or a mapped NDF array"
                   '("indf" "comp" "status"))

  (new-ndf-routine "NDF_VALID"
                   "Determine whether an NDF identifier is valid"
                   '("indf" "valid" "status"))

  (new-ndf-routine "NDF_XDEL"
                   "Delete a specified NDF extension"
                   '("indf" "xname" "status"))

  (new-ndf-routine "NDF_XGT0C"
                   "Read a scalar character value from a component within a named NDF extension"
                   '("indf" "xname" "cmpt" "value" "status")
                   "NDF_XGT0x")

  (new-ndf-routine "NDF_XGT0D"
                   "Read a scalar double precision value from a component within a named NDF extension"
                   '("indf" "xname" "cmpt" "value" "status")
                   "NDF_XGT0x")

  (new-ndf-routine "NDF_XGT0I"
                   "Read a scalar integer value from a component within a named NDF extension"
                   '("indf" "xname" "cmpt" "value" "status")
                   "NDF_XGT0x")

  (new-ndf-routine "NDF_XGT0L"
                   "Read a scalar logical value from a component within a named NDF extension"
                   '("indf" "xname" "cmpt" "value" "status")
                   "NDF_XGT0x")

  (new-ndf-routine "NDF_XGT0R"
                   "Read a scalar real value from a component within a named NDF extension"
                   '("indf" "xname" "cmpt" "value" "status")
                   "NDF_XGT0x")

  (new-ndf-routine "NDF_XIARY"
                   "Obtain access to an array stored in an NDF extension"
                   '("indf" "xname" "cmpt" "mode" "iary" "status"))

  (new-ndf-routine "NDF_XLOC"
                   "Obtain access to a named NDF extension via an HDS locator"
                   '("indf" "xname" "mode" "loc" "status"))

  (new-ndf-routine "NDF_XNAME"
                   "Obtain the name of the N'th extension in an NDF"
                   '("indf" "n" "xname" "status"))

  (new-ndf-routine "NDF_XNEW"
                   "Create a new extension in an NDF"
                   '("indf" "xname" "type" "ndim" "dim" "loc" "status"))

  (new-ndf-routine "NDF_XNUMB"
                   "Determine the number of extensions in an NDF"
                   '("indf" "xnumb" "status"))

  (new-ndf-routine "NDF_XPT0C"
                   "Write a scalar character value to a component within a named NDF extension"
                   '("value" "indf" "xname" "cmpt" "status")
                   "NDF_XPT0x")

  (new-ndf-routine "NDF_XPT0D"
                   "Write a scalar double precision value to a component within a named NDF extension"
                   '("value" "indf" "xname" "cmpt" "status")
                   "NDF_XPT0x")

  (new-ndf-routine "NDF_XPT0I"
                   "Write a scalar integer value to a component within a named NDF extension"
                   '("value" "indf" "xname" "cmpt" "status")
                   "NDF_XPT0x")

  (new-ndf-routine "NDF_XPT0L"
                   "Write a scalar logical value to a component within a named NDF extension"
                   '("value" "indf" "xname" "cmpt" "status")
                   "NDF_XPT0x")

  (new-ndf-routine "NDF_XPT0R"
                   "Write a scalar real value to a component within a named NDF extension"
                   '("value" "indf" "xname" "cmpt" "status")
                   "NDF_XPT0x")

  (new-ndf-routine "NDF_XSTAT"
                   "Determine if a named NDF extension exists"
                   '("indf" "xname" "there" "status")))
