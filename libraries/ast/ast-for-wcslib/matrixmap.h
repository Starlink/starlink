/* A dummy version of matrixmap.h for use in the WCSLIB sub-set of AST */
#define astMatrixMap(nin,nout,mode,mat,opts) \
   (astError(AST__INTER,"Internal AST programming error - " \
             "an attempt has been made to create a MatrixMap" ), \
   astError(AST__INTER,"The WCSLIB version of AST does not include " \
             "the MatrixMap class" ), \
   NULL)
