#ifndef ptkpgplot_h
#define ptkpgplot_h

#ifdef __cplusplus
extern "C" {
#endif

  int PgplotCmd(ClientData context, Tcl_Interp *interp, int objc,
	 	       Tcl_Obj *CONST objv[]);

/*
 * Record the official PGPLOT device name of the widget driver.
 */
#define TK_PGPLOT_DEVICE "PTK"

#ifdef __cplusplus
}
#endif

#endif







