# pkgIndex.tcl - Copyright (C) 2004 Pat Thoyts <pat@zsplat.freeserve.co.uk>
#
# This pkgIndex file is built manually and will not be properly generated
# by calling the tcl pkg_mkIndex command. So don't.
#
# $Id$

package ifneeded SOAP 1.6.8 [list source [file join $dir SOAP.tcl]]
package ifneeded SOAP::CGI 1.0 [list source [file join $dir SOAP-CGI.tcl]]
package ifneeded SOAP::Domain 1.4 [list source [file join $dir SOAP-domain.tcl]]
package ifneeded SOAP::Service 0.4 [list source [file join $dir SOAP-service.tcl]]
package ifneeded SOAP::Utils 1.0.1 [list source [file join $dir utils.tcl]]
package ifneeded SOAP::WSDL 1.0 [list source [file join $dir WSDL.tcl]]
package ifneeded SOAP::Schema 0.1 [list source [file join $dir schema.tcl]]
package ifneeded SOAP::ftp 1.0 [list source [file join $dir ftp.tcl]]
package ifneeded SOAP::http 1.0 [list source [file join $dir http.tcl]]
package ifneeded SOAP::https 1.0 [list source [file join $dir https.tcl]]
package ifneeded SOAP::smtp 1.0 [list source [file join $dir smtp.tcl]]
package ifneeded SOAP::xpath 0.2 [list source [file join $dir xpath.tcl]]
package ifneeded XMLRPC 1.0 [list source [file join $dir XMLRPC.tcl]]
package ifneeded XMLRPC::Domain 1.0 [list source [file join $dir XMLRPC-domain.tcl]]
package ifneeded rpcvar 1.2 [list source [file join $dir rpcvar.tcl]]
package ifneeded soapinterop::base 1.0 [list source [file join $dir interop soapinterop.tcl]]
package ifneeded soapinterop::B 1.0 [list source [file join $dir interop soapinteropB.tcl]]
package ifneeded soapinterop::C 1.0 [list source [file join $dir interop soapinteropC.tcl]]
