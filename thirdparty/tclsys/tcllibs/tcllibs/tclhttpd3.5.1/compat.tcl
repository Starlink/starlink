# compat.tcl
#@c Compatibility layer - deprecated
#
# Derived from doc.tcl
# Stephen Uhler / Brent Welch (c) 1997-1998 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# Colin McCormack (c) 2002
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::compat 3.3

foreach {oldname newname} {
    Doc_Cookie	Cookie_Get
    Doc_GetCookie	Cookie_Get
    Doc_SetCookie	Cookie_Set
    Doc_IsLinkToSelf	Url_IsLinkToSelf
    Doc_Redirect	Redirect_To
    Doc_RedirectSelf	Redirect_Self
    Doc_IsLinkToSelf	Url_IsLinkToSelf
    Doc_IndexFile	DirList_IndexFile
    Doc_Webmaster	Httpd_Webmaster
    Doc_CheckTemplates	Template_Check
    Doc_TemplateInterp	Template_Interp
    Doc_TemplateLibrary	Template_Library
    Doc_Dynamic		Template_Dynamic
    Doc_Subst		Subst_ReturnFile
    Doc_TemplateScope	Subst_Scope
    Doc_SubstInstall		Subst_Install

    Url_Redirect	Redirect_Url
    Url_RedirectSelf	Redirect_UrlSelf

} {
    interp alias {} $oldname {} $newname
}
