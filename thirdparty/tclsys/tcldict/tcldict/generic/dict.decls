# dict.decls --
#
#	This file contains the declarations for all supported public
#	functions that are exported by the Dict Tcl library via the 
#       stubs table.
#	This file is used to generate the dictDecls.h and dictStub.c files.
#	
#
# Copyright (c) 1998-1999 by Scriptics Corporation.
# Copyright (c) 2001, 2002 by Kevin B. Kenny.  All rights reserved.
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#   $Id$
# 
# Branched from: 
# RCS: @(#) Id: tcl.decls,v 1.103 2004/03/17 18:14:12 das Exp 
# for the dict backport.

library dict

# Define the dict interface, no sub interfaces.

interface dict
#hooks {}

# Declare each of the functions in the public dict interface.  Note that
# the an index should never be reused for a different function in order
# to preserve backwards compatibility.

# DICTIONARIES - TIP#111
declare 0 generic {
    int Tcl_DictObjPut(Tcl_Interp *interp, Tcl_Obj *dictPtr,
	    Tcl_Obj *keyPtr, Tcl_Obj *valuePtr)
}
declare 1 generic {
    int Tcl_DictObjGet(Tcl_Interp *interp, Tcl_Obj *dictPtr, Tcl_Obj *keyPtr,
	    Tcl_Obj **valuePtrPtr)
}
declare 2 generic {
    int Tcl_DictObjRemove(Tcl_Interp *interp, Tcl_Obj *dictPtr,
	    Tcl_Obj *keyPtr)
}
declare 3 generic {
    int Tcl_DictObjSize(Tcl_Interp *interp, Tcl_Obj *dictPtr, int *sizePtr)
}
declare 4 generic {
    int Tcl_DictObjFirst(Tcl_Interp *interp, Tcl_Obj *dictPtr,
	    Tcl_DictSearch *searchPtr,
	    Tcl_Obj **keyPtrPtr, Tcl_Obj **valuePtrPtr, int *donePtr)
}
declare 5 generic {
    void Tcl_DictObjNext(Tcl_DictSearch *searchPtr,
	    Tcl_Obj **keyPtrPtr, Tcl_Obj **valuePtrPtr, int *donePtr)
}
declare 6 generic {
    void Tcl_DictObjDone(Tcl_DictSearch *searchPtr)
}
declare 7 generic {
    int Tcl_DictObjPutKeyList(Tcl_Interp *interp, Tcl_Obj *dictPtr,
	    int keyc, Tcl_Obj *CONST *keyv, Tcl_Obj *valuePtr)
}
declare 8 generic {
    int Tcl_DictObjRemoveKeyList(Tcl_Interp *interp, Tcl_Obj *dictPtr,
	    int keyc, Tcl_Obj *CONST *keyv)
}
declare 9 generic {
    Tcl_Obj *Tcl_NewDictObj(void)
}
declare 10 generic {
    Tcl_Obj *Tcl_DbNewDictObj(CONST char *file, int line)
}

