/*
 * tclXinitSA.c --
 *
 * Standalone TclX initialization.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1997 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXinitSA.c,v 8.2 1997/11/19 02:31:26 markd Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtend.h"

static char *tclxIndexScript[] = {
    "set auto_index(for_array_keys) {source -rsrc arrayprocs}",
    "set auto_index(TruncFileName) {source -rsrc buildhelp}",
    "set auto_index(EnsureDirs) {source -rsrc buildhelp}",
    "set auto_index(CreateFilterNroffManPageContext) {source -rsrc buildhelp}",
    "set auto_index(FilterNroffManPage) {source -rsrc buildhelp}",
    "set auto_index(CreateExtractNroffHeaderContext) {source -rsrc buildhelp}",
    "set auto_index(ExtractNroffHeader) {source -rsrc buildhelp}",
    "set auto_index(CreateExtractNroffHelpContext) {source -rsrc buildhelp}",
    "set auto_index(ExtractNroffHelp) {source -rsrc buildhelp}",
    "set auto_index(CreateExtractScriptHelpContext) {source -rsrc buildhelp}",
    "set auto_index(ExtractScriptHelp) {source -rsrc buildhelp}",
    "set auto_index(ProcessNroffFile) {source -rsrc buildhelp}",
    "set auto_index(ProcessTclScript) {source -rsrc buildhelp}",
    "set auto_index(buildhelp) {source -rsrc buildhelp}",
    "set auto_index(TCLSH:PutIdxEntry) {source -rsrc buildidx}",
    "set auto_index(TCLSH:ParsePkgHeader) {source -rsrc buildidx}",
    "set auto_index(TCLSH:CreateLibIndex) {source -rsrc buildidx}",
    "set auto_index(buildpackageindex) {source -rsrc buildidx}",
    "set auto_index(assign_fields) {source -rsrc compat}",
    "set auto_index(cexpand) {source -rsrc compat}",
    "set auto_index(server_open) {source -rsrc compat}",
    "set auto_index(server_connect) {source -rsrc compat}",
    "set auto_index(server_send) {source -rsrc compat}",
    "set auto_index(server_info) {source -rsrc compat}",
    "set auto_index(server_cntl) {source -rsrc compat}",
    "set auto_index(fmtclock) {source -rsrc compat}",
    "set auto_index(convertclock) {source -rsrc compat}",
    "set auto_index(getclock) {source -rsrc compat}",
    "set auto_index(mkdir) {source -rsrc compat}",
    "set auto_index(rmdir) {source -rsrc compat}",
    "set auto_index(unlink) {source -rsrc compat}",
    "set auto_index(frename) {source -rsrc compat}",
    "set auto_index(copyfile) {source -rsrc compat}",
    "set auto_index(tclx:ParseTclIndex) {source -rsrc convlib}",
    "set auto_index(convert_lib) {source -rsrc convlib}",
    "set auto_index(saveprocs) {source -rsrc edprocs}",
    "set auto_index(edprocs) {source -rsrc edprocs}",
    "set auto_index(mainloop) {source -rsrc events}",
    "set auto_index(acos) {source -rsrc fmath}",
    "set auto_index(asin) {source -rsrc fmath}",
    "set auto_index(atan) {source -rsrc fmath}",
    "set auto_index(ceil) {source -rsrc fmath}",
    "set auto_index(cos) {source -rsrc fmath}",
    "set auto_index(cosh) {source -rsrc fmath}",
    "set auto_index(exp) {source -rsrc fmath}",
    "set auto_index(fabs) {source -rsrc fmath}",
    "set auto_index(floor) {source -rsrc fmath}",
    "set auto_index(log) {source -rsrc fmath}",
    "set auto_index(log10) {source -rsrc fmath}",
    "set auto_index(sin) {source -rsrc fmath}",
    "set auto_index(sinh) {source -rsrc fmath}",
    "set auto_index(sqrt) {source -rsrc fmath}",
    "set auto_index(tan) {source -rsrc fmath}",
    "set auto_index(tanh) {source -rsrc fmath}",
    "set auto_index(fmod) {source -rsrc fmath}",
    "set auto_index(pow) {source -rsrc fmath}",
    "set auto_index(atan2) {source -rsrc fmath}",
    "set auto_index(abs) {source -rsrc fmath}",
    "set auto_index(double) {source -rsrc fmath}",
    "set auto_index(int) {source -rsrc fmath}",
    "set auto_index(round) {source -rsrc fmath}",
    "set auto_index(for_file) {source -rsrc forfile}",
    "set auto_index(recursive_glob) {source -rsrc globrecur}",
    "set auto_index(for_recursive_glob) {source -rsrc globrecur}",
    "set auto_index(help:RootDirs) {source -rsrc help}",
    "set auto_index(help:FlattenPath) {source -rsrc help}",
    "set auto_index(help:ConvertPath) {source -rsrc help}",
    "set auto_index(help:RelativePath) {source -rsrc help}",
    "set auto_index(help:ListSubject) {source -rsrc help}",
    "set auto_index(help:Display) {source -rsrc help}",
    "set auto_index(help:DisplayPage) {source -rsrc help}",
    "set auto_index(help:DisplayColumns) {source -rsrc help}",
    "set auto_index(help:HelpOnHelp) {source -rsrc help}",
    "set auto_index(help) {source -rsrc help}",
    "set auto_index(helpcd) {source -rsrc help}",
    "set auto_index(helppwd) {source -rsrc help}",
    "set auto_index(apropos) {source -rsrc help}",
    "set auto_index(auto_load) {source -rsrc autoload}",
    "set auto_index(profrep:sortcmp) {source -rsrc profrep}",
    "set auto_index(profrep:sort) {source -rsrc profrep}",
    "set auto_index(profrep:print) {source -rsrc profrep}",
    "set auto_index(profrep) {source -rsrc profrep}",
    "set auto_index(pushd) {source -rsrc pushd}",
    "set auto_index(popd) {source -rsrc pushd}",
    "set auto_index(dirs) {source -rsrc pushd}",
    "set auto_index(union) {source -rsrc setfuncs}",
    "set auto_index(lrmdups) {source -rsrc setfuncs}",
    "set auto_index(intersect3) {source -rsrc setfuncs}",
    "set auto_index(intersect) {source -rsrc setfuncs}",
    "set auto_index(showproc) {source -rsrc showproc}",
    "set auto_index(read_file) {source -rsrc stringfile}",
    "set auto_index(write_file) {source -rsrc stringfile}",
    "set auto_index(searchpath) {source -rsrc tcllib}",
    "set auto_index(auto_load_file) {source -rsrc tcllib}",
    "set auto_index(auto_packages) {source -rsrc tcllib}",
    "set auto_index(auto_commands) {source -rsrc tcllib}",
    (char *) NULL
};

#include "./tclx.c"
#include "./arrayprocs.c"
#include "./buildhelp.c"
#include "./buildidx.c"
#include "./compat.c"
#include "./convlib.c"
#include "./edprocs.c"
#include "./events.c"
#include "./fmath.c"
#include "./forfile.c"
#include "./globrecur.c"
#include "./help.c"
#include "./autoload.c"
#include "./profrep.c"
#include "./pushd.c"
#include "./setfuncs.c"
#include "./showproc.c"
#include "./stringfile.c"
#include "./tcllib.c"

static Tcl_StaticFile table[] = {
    {"tclx:tclIndex", tclxIndexScript},
    {"tclx", tclx_c},
    {"arrayprocs", arrayprocs_c},
    {"buildhelp", buildhelp_c},
    {"buildidx", buildidx_c},
    {"compat", compat_c},
    {"convlib", convlib_c},
    {"edprocs", edprocs_c},
    {"events", events_c},
    {"fmath", fmath_c},
    {"forfile", forfile_c},
    {"globrecur", globrecur_c},
    {"help", help_c},
    {"autoload", autoload_c},
    {"profrep", profrep_c},
    {"pushd", pushd_c},
    {"setfuncs", setfuncs_c},
    {"showproc", showproc_c},
    {"stringfile", stringfile_c},
    {"tcllib", tcllib_c},
    {(char *) NULL, (char **) NULL}
};

int
Tclx_InitStandAlone (interp)
    Tcl_Interp *interp;
{
    Tcl_DefineStaticFile(table);
    return Tclx_Init(interp);
}
