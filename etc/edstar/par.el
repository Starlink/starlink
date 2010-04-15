(new-routine "PAR_CANCL"
             "Cancel a parameter"
             '("PARAM" "STATUS"))

(new-routine "PAR_DEF0C"
             "Set CHARACTER dynamic default scalar parameter value"
             '("PARAM" "CVALUE" "STATUS"))

(new-routine "PAR_DEF0D"
             "Set DOUBLE PRECISION dynamic default scalar parameter value"
             '("PARAM" "DVALUE" "STATUS"))

(new-routine "PAR_DEF0I"
             "Set INTEGER dynamic default scalar parameter value"
             '("PARAM" "IVALUE" "STATUS"))

(new-routine "PAR_DEF0L"
             "Set LOGICAL dynamic default scalar parameter value"
             '("PARAM" "LVALUE" "STATUS"))

(new-routine "PAR_DEF0R"
             "Set REAL dynamic default scalar parameter value"
             '("PARAM" "RVALUE" "STATUS"))

(new-routine "PAR_DEF1C"
             "Set CHARACTER dynamic default vector parameter value"
             '("PARAM" "NVAL" "CVALUE" "STATUS"))

(new-routine "PAR_DEF1D"
             "Set DOUBLE PRECISION dynamic default vector parameter value"
             '("PARAM" "NVAL" "DVALUE" "STATUS"))

(new-routine "PAR_DEF1I"
             "Set INTEGER dynamic default vector parameter value"
             '("PARAM" "NVAL" "IVALUE" "STATUS"))

(new-routine "PAR_DEF1L"
             "Set LOGICAL dynamic default vector parameter value"
             '("PARAM" "NVAL" "LVALUE" "STATUS"))

(new-routine "PAR_DEF1R"
             "Set REAL dynamic default vector parameter value"
             '("PARAM" "NVAL" "RVALUE" "STATUS"))

(new-routine "PAR_DEFNC"
             "Set CHARACTER N-dimensional dynamic default parameter value"
             '("PARAM" "NDIM" "DIMX" "CVALUE" "DIMS" "STATUS"))

(new-routine "PAR_DEFND"
             "Set DOUBLE PRECISION N-dimensional dynamic default parameter value"
             '("PARAM" "NDIM" "DIMX" "DVALUE" "DIMS" "STATUS"))

(new-routine "PAR_DEFNI"
             "Set INTEGER N-dimensional dynamic default parameter value"
             '("PARAM" "NDIM" "DIMX" "IVALUE" "DIMS" "STATUS"))

(new-routine "PAR_DEFNL"
             "Set LOGICAL N-dimensional dynamic default parameter value"
             '("PARAM" "NDIM" "DIMX" "LVALUE" "DIMS" "STATUS"))

(new-routine "PAR_DEFNR"
             "Set REAL N-dimensional dynamic default parameter value"
             '("PARAM" "NDIM" "DIMX" "RVALUE" "DIMS" "STATUS"))

(new-routine "PAR_GET0C"
             "Read CHARACTER scalar parameter value"
             '("PARAM" "CVALUE" "STATUS"))

(new-routine "PAR_GET0D"
             "Read DOUBLE PRECISION scalar parameter value"
             '("PARAM" "DVALUE" "STATUS"))

(new-routine "PAR_GET0I"
             "Read INTEGER scalar parameter value"
             '("PARAM" "IVALUE" "STATUS"))

(new-routine "PAR_GET0L"
             "Read LOGICAL scalar parameter value"
             '("PARAM" "LVALUE" "STATUS"))

(new-routine "PAR_GET0R"
             "Read REAL scalar parameter value"
             '("PARAM" "RVALUE" "STATUS"))

(new-routine "PAR_GET1C"
             "Read CHARACTER vector parameter value"
             '("PARAM" "MAXVAL" "CVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GET1D"
             "Read DOUBLE PRECISION vector parameter value"
             '("PARAM" "MAXVAL" "DVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GET1I"
             "Read INTEGER vector parameter value"
             '("PARAM" "MAXVAL" "IVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GET1L"
             "Read LOGICAL vector parameter value"
             '("PARAM" "MAXVAL" "LVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GET1R"
             "Read REAL vector parameter value"
             '("PARAM" "MAXVAL" "RVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GETNC"
             "Read CHARACTER N-dimensional parameter value"
             '("PARAM" "NDIM" "DIMX" "CVALUE" "DIM" "STATUS"))

(new-routine "PAR_GETND"
             "Read DOUBLE PRECISION N-dimensional parameter value"
             '("PARAM" "NDIM" "DIMX" "DVALUE" "DIM" "STATUS"))

(new-routine "PAR_GETNI"
             "Read INTEGER N-dimensional parameter value"
             '("PARAM" "NDIM" "DIMX" "IVALUE" "DIM" "STATUS"))

(new-routine "PAR_GETNL"
             "Read LOGICAL N-dimensional parameter value"
             '("PARAM" "NDIM" "DIMX" "LVALUE" "DIM" "STATUS"))

(new-routine "PAR_GETNR"
             "Read REAL N-dimensional parameter value"
             '("PARAM" "NDIM" "DIMX" "RVALUE" "DIM" "STATUS"))

(new-routine "PAR_GETVC"
             "Get CHARACTER parameter values as if vectorised"
             '("PARAM" "MAXVAL" "CVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GETVD"
             "Get DOUBLE PRECISION parameter values as if vectorised"
             '("PARAM" "MAXVAL" "DVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GETVI"
             "Get INTEGER parameter values as if vectorised"
             '("PARAM" "MAXVAL" "IVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GETVL"
             "Get LOGICAL parameter values as if vectorised"
             '("PARAM" "MAXVAL" "LVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_GETVR"
             "Get REAL parameter values as if vectorised"
             '("PARAM" "MAXVAL" "RVALUE" "ACTVAL" "STATUS"))

(new-routine "PAR_PROMT"
             "Set a new prompt string for a parameter"
             '("PARAM" "PROMPT" "STATUS"))

(new-routine "PAR_PUT0C"
             "Write CHARACTER scalar parameter value"
             '("PARAM" "CVALUE" "STATUS"))

(new-routine "PAR_PUT0D"
             "Write DOUBLE PRECISION scalar parameter value"
             '("PARAM" "DVALUE" "STATUS"))

(new-routine "PAR_PUT0I"
             "Write INTEGER scalar parameter value"
             '("PARAM" "IVALUE" "STATUS"))

(new-routine "PAR_PUT0L"
             "Write LOGICAL scalar parameter value"
             '("PARAM" "LVALUE" "STATUS"))

(new-routine "PAR_PUT0R"
             "Write REAL scalar parameter value"
             '("PARAM" "RVALUE" "STATUS"))

(new-routine "PAR_PUT1C"
             "Write CHARACTER vector parameter values"
             '("PARAM" "NVAL" "CVALUE" "STATUS"))

(new-routine "PAR_PUT1D"
             "Write DOUBLE PRECISION vector parameter values"
             '("PARAM" "NVAL" "DVALUE" "STATUS"))

(new-routine "PAR_PUT1I"
             "Write INTEGER vector parameter values"
             '("PARAM" "NVAL" "IVALUE" "STATUS"))

(new-routine "PAR_PUT1L"
             "Write LOGICAL vector parameter values"
             '("PARAM" "NVAL" "LVALUE" "STATUS"))

(new-routine "PAR_PUT1R"
             "Write REAL vector parameter values"
             '("PARAM" "NVAL" "RVALUE" "STATUS"))

(new-routine "PAR_PUTNC"
             "Write CHARACTER N-dimensional parameter values"
             '("PARAM" "NDIM" "DIMX" "CVALUE" "DIM" "STATUS"))

(new-routine "PAR_PUTND"
             "Write DOUBLE PRECISION N-dimensional parameter values"
             '("PARAM" "NDIM" "DIMX" "DVALUE" "DIM" "STATUS"))

(new-routine "PAR_PUTNI"
             "Write INTEGER N-dimensional parameter values"
             '("PARAM" "NDIM" "DIMX" "IVALUE" "DIM" "STATUS"))

(new-routine "PAR_PUTNL"
             "Write LOGICAL N-dimensional parameter values"
             '("PARAM" "NDIM" "DIMX" "LVALUE" "DIM" "STATUS"))

(new-routine "PAR_PUTNR"
             "Write REAL N-dimensional parameter values"
             '("PARAM" "NDIM" "DIMX" "RVALUE" "DIM" "STATUS"))

(new-routine "PAR_PUTVC"
             "Write CHARACTER parameter values as if vectorised"
             '("PARAM" "NVAL" "CVALUES" "STATUS"))

(new-routine "PAR_PUTVD"
             "Write DOUBLE PRECISION parameter values as if vectorised"
             '("PARAM" "NVAL" "DVALUES" "STATUS"))

(new-routine "PAR_PUTVI"
             "Write INTEGER parameter values as if vectorised"
             '("PARAM" "NVAL" "IVALUES" "STATUS"))

(new-routine "PAR_PUTVL"
             "Write LOGICAL parameter values as if vectorised"
             '("PARAM" "NVAL" "LVALUES" "STATUS"))

(new-routine "PAR_PUTVR"
             "Write REAL parameter values as if vectorised"
             '("PARAM" "NVAL" "RVALUES" "STATUS"))

(new-routine "PAR_STATE"
             "Return the current state of a parameter"
             '("PARAM" "STATE" "STATUS"))
