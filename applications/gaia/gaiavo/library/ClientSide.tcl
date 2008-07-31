###############################################################################
##                                                                           ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
##  Copyright (c) 2006, Gerald W. Lester                                     ##
##  Copyright (c) 2006, Arnulf Wiedemann                                     ##
##  Copyright (c) 2006, Colin McCormack                                      ##
##  Copyright (c) 2006, Rolf Ade                                             ##
##  Copyright (c) 2001-2006, Pat Thoyts                                      ##
##  All rights reserved.                                                     ##
##                                                                           ##
##  Redistribution and use in source and binary forms, with or without       ##
##  modification, are permitted provided that the following conditions       ##
##  are met:                                                                 ##
##                                                                           ##
##    * Redistributions of source code must retain the above copyright       ##
##      notice, this list of conditions and the following disclaimer.        ##
##    * Redistributions in binary form must reproduce the above              ##
##      copyright notice, this list of conditions and the following          ##
##      disclaimer in the documentation and/or other materials provided      ##
##      with the distribution.                                               ##
##    * Neither the name of the Visiprise Software, Inc nor the names        ##
##      of its contributors may be used to endorse or promote products       ##
##      derived from this software without specific prior written            ##
##      permission.                                                          ##
##                                                                           ##
##  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      ##
##  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        ##
##  LIMITED  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       ##
##  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE           ##
##  COPYRIGHT OWNER OR  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     ##
##  INCIDENTAL, SPECIAL,  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    ##
##  BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        ##
##  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER         ##
##  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT       ##
##  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING IN       ##
##  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  ADVISED OF THE         ##
##  POSSIBILITY OF SUCH DAMAGE.                                              ##
##                                                                           ##
###############################################################################

if {![llength [info command dict]]} {
    package require dict
}
package require tdom
package require http
package require log
package require uri

catch {
    package require tls
    http::register https 443 ::tls::socket
}

namespace eval ::WS::Client {
    array set serviceArr {}
    set currentBaseUrl {}
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::LoadParsedWsdl
#
# Description : Load a saved service definition in
#
# Arguments :
#       serviceInfo - parsed service definition, as returned from
#                     ::WS::Client::ParseWsdl or ::WS::Client::GetAndParseWsdl
#       headers     - Extra headers to add to the HTTP request. This
#                       is a key value list argument. It must be a list with
#                       an even number of elements that alternate between
#                       keys and values. The keys become header field names.
#                       Newlines are stripped from the values so the header
#                       cannot be corrupted.
#                       This is an optional argument and defaults to {}.
#       serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns :     The name of the service loaded
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::LoadParsedWsdl {serviceInfo {headers {}} {serviceAlias {}}} {
    variable serviceArr

    if {[string length $serviceAlias]} {
        set serviceName $serviceAlias
    } else {
        set serviceName [dict get $serviceInfo service name]
    }
    if {[llength $headers]} {
        dict set serviceInfo headers $headers
    }
    set serviceArr($serviceName) $serviceInfo

    if {[dict exists $serviceInfo types]} {
        foreach {typeName partList} [dict get $serviceInfo types] {
            if {[string equal [lindex [split $typeName {:}] 0] {}]} {
                ::WS::Utils::ServiceTypeDef Client $serviceName $typeName $partList tns1
            } else {
                set xns [lindex [split $typeName {:}] 0]
                set typeName [lindex [split $typeName {:}] 1]
                ::WS::Utils::ServiceTypeDef Client $serviceName $typeName $partList $xns
            }
        }
    }

    if {[dict exists $serviceInfo simpletypes]} {
        foreach {typeName partList} [dict get $serviceInfo simpletypes] {
            if {[string equal [lindex [split $typeName {:}] 0] {}]} {
                ::WS::Utils::ServiceSimpleTypeDef Client $serviceName $typeName $partList tns1
            } else {
                set xns [lindex [split $typeName {:}] 0]
                set typeName [lindex [split $typeName {:}] 1]
                ::WS::Utils::ServiceSimpleTypeDef Client $serviceName $typeName $partList $xns
            }
        }
    }

    return $serviceName
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::GetAndParseWsdl
#
# Description :
#
# Arguments :
#       url     - The url of the WSDL
#       headers     - Extra headers to add to the HTTP request. This
#                       is a key value list argument. It must be a list with
#                       an even number of elements that alternate between
#                       keys and values. The keys become header field names.
#                       Newlines are stripped from the values so the header
#                       cannot be corrupted.
#                       This is an optional argument and defaults to {}.
#       serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed service definition
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::GetAndParseWsdl {url {headers {}} {serviceAlias {}}} {
    variable currentBaseUrl

    set currentBaseUrl $url
    switch [dict get [::uri::split $url] scheme] {
        file {
            upvar #0 [::uri::geturl $url] token
            set wsdlInfo [ParseWsdl $token(data) -headers $headers -serviceAlias $serviceAlias]
            unset token
        }
        http {
            if {[llength $headers]} {
                set token [::http::geturl $url -headers $headers]
            } else {
                set token [::http::geturl $url]
            }
            ::http::wait $token
            set wsdlInfo [ParseWsdl [::http::data $token] -headers $headers -serviceAlias $serviceAlias]
            ::http::cleanup $token
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKURLTYP $url] \
                "Unknown URL type '$url'"
        }
    }
    set currentBaseUrl {}

    return $wsdlInfo
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::ParseWsdl
#
# Description : Parse a WSDL
#
# Arguments :
#       wsdlXML - XML of the WSDL
#
# Optional Arguments:
#       -createStubs 0|1 - create stub routines for the service
#                               NOTE -- Webservice arguments are position
#                                       independent, thus the proc arguments
#                                       will be defined in alphabetical order.
#       -headers         - Extra headers to add to the HTTP request. This
#                          is a key value list argument. It must be a list with
#                          an even number of elements that alternate between
#                          keys and values. The keys become header field names.
#                          Newlines are stripped from the values so the header
#                          cannot be corrupted.
#                          This is an optional argument and defaults to {}.
#       -serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed service definition
#
# Side-Effects : None
#
# Exception Conditions :None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::ParseWsdl {wsdlXML args} {
    variable serviceArr

    array set defaults {
        -createStubs    0
        -headers        {}
        -serviceAlias   {}
    }

    array set defaults $args

    dom parse $wsdlXML wsdlDoc
    $wsdlDoc documentElement wsdlNode
    $wsdlDoc selectNodesNamespaces {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        s http://www.w3.org/2001/XMLSchema
    }
    if {[string length $defaults(-serviceAlias)]} {
        set serviceAlias $defaults(-serviceAlias)
    } else {
        set serviceAlias {}
    }

    set serviceInfo {}

    foreach serviceInfo [buildServiceInfo $wsdlNode $serviceInfo $serviceAlias] {
        set serviceName [dict get $serviceInfo service name]
        set serviceArr($serviceName) $serviceInfo

        if {[llength $defaults(-headers)]} {
            dict set serviceInfo headers $defaults(-headers)
        }

        if {$defaults(-createStubs)} {
            catch {namespace delete $serviceName}
            namespace eval $serviceName {}
            CreateStubs $serviceName
        }
    }

    $wsdlDoc delete

    return $serviceInfo

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::CreateStubs
#
# Description : Create stubs routines to make calls to Webservice Operations.
#               All routines will be create in a namespace that is the same
#               as the service name.  The procedure name will be the same
#               as the operation name.
#
#               NOTE -- Webservice arguments are position independent, thus
#                       the proc arguments will be defined in alphabetical order.
#
# Arguments :
#       serviceName     - The service to create stubs for
#
# Returns : A string describing the created procedures.
#
# Side-Effects : Existing namespace is deleted.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : Service must have been defined.
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::CreateStubs {serviceName} {
    variable serviceArr

    namespace eval [format {::%s::} $serviceName] {}

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }

    set serviceInfo $serviceArr($serviceName)

    set procList {}

    foreach operationName [dict get $serviceInfo operList] {
        set procName [format {::%s::%s} $serviceName $operationName]
        set argList {}
        foreach inputHeaderType [dict get $serviceInfo operation $operationName soapRequestHeader] {
            if {[string equal $inputHeaderType {}]} {
                continue
            }
            set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
            set headerFields [dict keys [dict get $headerTypeInfo definition]]
            if {![string equal $headerFields {}]} {
                lappend argList [lsort -dictionary $headerFields]
            }
        }
        set inputMsgType [dict get $serviceInfo operation $operationName inputs]
        set inputFields [dict keys [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType] definition]]
        if {![string equal $inputFields {}]} {
            lappend argList [lsort -dictionary $inputFields]
        }
        set argList [join $argList]

        set body {
            set procName [lindex [info level 0] 0]
            set serviceName [string trim [namespace qualifiers $procName] {:}]
            set operationName [string trim [namespace tail $procName] {:}]
            set argList {}
            foreach var [namespace eval ::${serviceName}:: [list info args $operationName]] {
                lappend argList $var [set $var]
            }
            ::log::log debug [list ::WS::Client::DoCall $serviceName $operationName $argList]
            ::WS::Client::DoCall $serviceName $operationName $argList
        }
        proc $procName $argList $body
        append procList "\n\t[list $procName $argList]"
    }
    return "$procList\n"
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoRawCall
#
# Description : Call an operation of a web service
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object.
#                         This is for both the Soap Header and Body messages.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       The XML of the operation.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#
# Pre-requisite Conditions :    Service must have been defined.
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::DoRawCall {serviceName operationName argList {headers {}}} {
    variable serviceArr

    ::log::log debug "Entering ::WS::Client::DoRawCall {$serviceName $operationName $argList}"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    }
    set url [dict get $serviceInfo service location]
    set query [buildCallquery $serviceName $operationName $url $argList]
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[dict exists $serviceInfo operation $operationName action]} {
        lappend headers  SOAPAction [dict get $serviceInfo operation $operationName action]
    }
    if {[llength $headers]} {
        set token [::http::geturl $url -query $query -type text/xml -headers $headers]
    } else {
        set token [::http::geturl $url -query $query -type text/xml]
    }
    ::http::wait $token

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    if {![string equal [::http::status $token] ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        set errorInfo {}
        set results [::http::error $token]
        set hadError 1
    } else {
        set hadError 0
        set results [::http::data $token]
    }
    ::http::cleanup $token
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoRawCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::log debug "Leaving ::WS::Client::DoRawCall with {$results}"
        return $results
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoCall
#
# Description : Call an operation of a web service
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       The return value of the operation as a dictionary object.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#       others                  - as raised by called Operation
#
# Pre-requisite Conditions :    Service must have been defined.
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::DoCall {serviceName operationName argList {headers {}}} {
    variable serviceArr

    ::log::log debug "Entering ::WS::Client::DoCall {$serviceName $operationName $argList}"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    }
    set url [dict get $serviceInfo service location]
    set query [buildCallquery $serviceName $operationName $url $argList]
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[dict exists $serviceInfo operation $operationName action]} {
        lappend headers  SOAPAction [dict get $serviceInfo operation $operationName action]
    }
    if {[llength $headers]} {
        ::log::log debug [list ::http::geturl $url -query $query -type text/xml -headers $headers]
        set token [::http::geturl $url -query $query -type text/xml -headers $headers]
    } else {
        ::log::log debug  [list ::http::geturl $url -query $query -type text/xml]
        set token [::http::geturl $url -query $query -type text/xml]
    }
    ::http::wait $token

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    ::log::log debug "\tReceived: $body"
    set httpStatus [::http::status $token]
    if {![string equal $httpStatus ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        ::log::log debug "\tHTTP error [array get $token]"
        set results [::http::error $token]
        if {[string equal $results {}] || [string equal $httpStatus eof]} {
            set results {Unexpected EOF received from Server}
            set errorCode [list WSCLIENT HTTPERROR UNEXPEOF]
        } else {
            set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        }
        set errorInfo {}
        set hadError 1
    } else {
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        if {$hadError} {
            ::log::log debug "Reply was [::http::data $token]"
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }
    ::http::cleanup $token
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::log debug "Leaving ::WS::Client::DoCall with {$results}"
        return $results
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoAsyncCall
#
# Description : Call an operation of a web service asynchronously
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       succesCmd       - A command prefix to be called if the operations
#                         does not raise an error.  The results, as a dictionary
#                         object are concatinated to the prefix.
#       errorCmd        - A command prefix to be called if the operations
#                         raises an error.  The error code and stack trace
#                         are concatinated to the prefix.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       None.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#       others                  - as raised by called Operation
#
# Pre-requisite Conditions :    Service must have been defined.
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::DoAsyncCall {serviceName operationName argList succesCmd errorCmd {headers {}}} {
    variable serviceArr

    ::log::log debug "Entering ::WS::Client::DoAsyncCall [list $serviceName $operationName $argList $succesCmd $errorCmd $headers]"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    }
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    set url [dict get $serviceInfo service location]
    set query [buildCallquery $serviceName $operationName $url $argList]
    if {[llength $headers]} {
        ::http::geturl $url \
            -query $query \
            -type text/xml \
            -headers $headers \
            -command [list ::WS::Client::asyncCallDone $serviceName $operationName $succesCmd $errorCmd]
    } else {
        ::http::geturl $url \
            -query $query \
            -type text/xml \
            -command [list ::WS::Client::asyncCallDone $serviceName $operationName $succesCmd $errorCmd]
    }
    ::log::log debug "Leaving ::WS::Client::DoAsyncCall"
    return;
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::List
#
# Description : List a Webservice's Operations.
#
#               NOTE -- Webservice arguments are position independent, thus
#                       the proc arguments will be defined in alphabetical order.
#
# Arguments :
#       serviceName     - The service to create stubs for
#
# Returns : A string describing the operations.
#
# Side-Effects : Existing namespace is deleted.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : Service must have been defined.
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  10/11/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::List {serviceName} {
    variable serviceArr

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }

    set serviceInfo $serviceArr($serviceName)

    set procList {}

    foreach operationName [dict get $serviceInfo operList] {
        set procName $operationName
        set argList {}
        foreach inputHeaderType [dict get $serviceInfo operation $operationName soapRequestHeader] {
            if {[string equal $inputHeaderType {}]} {
                continue
            }
            set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
            set headerFields [dict keys [dict get $headerTypeInfo definition]]
            if {![string equal $headerFields {}]} {
                lappend argList [lsort -dictionary $headerFields]
            }
        }
        set inputMsgType [dict get $serviceInfo operation $operationName inputs]
        set inputFields [dict keys [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType] definition]]
        if {![string equal $inputFields {}]} {
            lappend argList [lsort -dictionary $inputFields]
        }
        set argList [join $argList]

        append procList "\n\t$procName $argList"
    }
    return "$procList\n"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::asyncCallDone
#
# Description : Called when an asynchronous call is complete.  This routine
#               will call either the success or error callback depending on
#               if the operation succeeded or failed -- assuming the callback
#               is defined.
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    succesCmd          - the command prefix to call if no error
#    errorCmd           - the command prefix to call on an error
#    token              - the token from the HTTP request
#
# Returns : Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::asyncCallDone {serviceName operationName succesCmd errorCmd token} {
    ::log::log debug "Entering ::WS::Client::asyncCallDone {$serviceName $operationName $succesCmd $errorCmd $token}"

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    if {![string equal [::http::status $token] ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        set hadError 1
        set errorInfo [::http::error $token]
    } else {
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        if {$hadError} {
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }

    ##
    ## Call the appropriate callback
    ##
    if {$hadError} {
        set cmd $errorCmd
        lappend cmd $errorCode $errorInfo
    } else {
        set cmd $succesCmd
    }
    lappend cmd $results
    catch $cmd

    ##
    ## All done
    ##
    ::http::cleanup $token
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseResults
#
# Description : Convert the returned XML into a dictionary object
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    inXML              - the XML returned by the operation
#
# Returns : A dictionary object representing the results
#
# Side-Effects : None
#
# Exception Conditions :
#       WSCLIENT REMERR         - The remote end raised an exception, the third element of
#                                 the error code is the remote fault code.
#                                 Error info is set to the remote fault details.
#                                 The error message is the remote fault string;
#       WSCLIENT BADREPLY       - Badly formatted reply, the third element is a list of
#                                 what message type was received vs what was expected.
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseResults {serviceName operationName inXML} {
    variable serviceArr

    ::log::log debug "In parseResults $serviceName $operationName {$inXML}"
    set serviceInfo $serviceArr($serviceName)
    set expectedMsgType [dict get $serviceInfo operation $operationName outputs]

    dom parse $inXML doc
    $doc documentElement top
    set xns {
        ENV http://schemas.xmlsoap.org/soap/envelope/
        xsi "http://www.w3.org/2001/XMLSchema-instance"
        xs "http://www.w3.org/2001/XMLSchema"
    }
    foreach tmp [dict get $serviceInfo targetNamespace] {
        lappend xns [lindex $tmp 0] [lindex $tmp 1]
    }
    ::log::log debug "Using namespaces {$xns}"
    $doc selectNodesNamespaces $xns
    set body [$top selectNodes ENV:Body]
    set rootNode [$body childNodes]
    if {![string equal $rootNode {}]} {
       set rootNode [lindex $rootNode 0]
       set rootName [$rootNode localName]
    } else {
       set rootName {}
    }

    ##
    ## See if it is a standard error packet
    ##
    if {[string equal $rootName {Fault}]} {
        set faultcode {}
        set faultstring {}
        set errorInfo {}
        if {[catch {set faultcode [[$rootNode selectNodes ENV:faultcode] asText]}]} {
            catch {set faultcode [[$rootNode selectNodes faultcode] asText]}
        }
        if {[catch {set faultstring [[$rootNode selectNodes ENV:faultstring] asText]}]} {
            catch {set faultstring [[$rootNode selectNodes faultstring] asText]}
        }
        if {[catch {set errorInfo [[$rootNode selectNodes ENV:detail] asXML]}]} {
            catch {set errorInfo [[$rootNode selectNodes detail/] asXML]}
        }
        $doc delete
        return \
            -code error \
            -errorcode [list WSCLIENT REMERR $faultcode] \
            -errorinfo $errorInfo \
            $faultstring
    }

    ##
    ## Validated that it is the expected packet type
    ##
    if {![string equal $rootName $expectedMsgType]} {
        $doc delete
        return \
            -code error \
            -errorcode [list WSCLIENT BADREPLY [list $rootNode $expectedMsgType]] \
            "Bad reply type, received '$rootNode; but expected '$expectedMsgType'."
    }

    ##
    ## Convert the packet to a dictionary
    ##

    set results {}
    set headerRootNode [$rootNode selectNodes ENV:Header]
    foreach outHeaderType [dict get $serviceInfo operation $operationName soapReplyHeader] {
        if {[string equal $outHeaderType {}]} {
            continue
        }
        ::log::log debug "Calling [list ::WS::Utils::convertTypeToDict Client $serviceName $rootNode $outHeaderType]"
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $outputHeaderType] xns]
        set node [$headerRootNode selectNodes $xns:outHeaderType]
        lappend results [::WS::Utils::convertTypeToDict Client $serviceName $node $outHeaderType]
    }
    ::log::log debug "Calling [list ::WS::Utils::convertTypeToDict Client $serviceName $rootNode $expectedMsgType]"
    if {![string equal $rootName {}]} {
        lappend results [::WS::Utils::convertTypeToDict Client $serviceName $rootNode $expectedMsgType]
    }
    set results [join $results]
    $doc delete

    return $results

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#
# Returns : The XML for the call
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::buildCallquery {serviceName operationName url argList} {
    variable serviceArr

    set serviceInfo $serviceArr($serviceName)

    set style [dict get $serviceInfo operation $operationName style]

    switch $style {
        document/literal {
            set xml [buildDocLiteralCallquery $serviceName $operationName $url $argList]
        }
        rpc/encoded {
            set xml [buildRpcEncodedCallquery $serviceName $operationName $url $argList]
        }
    }

    ::log::log debug "Leaving ::::WS::Client::buildCallquery with {$xml}"
    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildDocLiteralCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#                         This is for both the Soap Header and Body messages.
#
# Returns : The XML for the call
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::buildDocLiteralCallquery {serviceName operationName url argList} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo operation $operationName inputs]
    set url [dict get $serviceInfo service location]
    set xnsList [dict get $serviceInfo targetNamespace]

    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute \
        "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
        "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/" \
        "xmlns:xsi"      "http://www.w3.org/2001/XMLSchema-instance" \
        "xmlns:xs"      "http://www.w3.org/2001/XMLSchema"
    foreach xns $xnsList {
        set tns [lindex $xns 0]
        set target [lindex $xns 1]
        $env  setAttribute \
            xmlns:$tns $target
    }

    set firstHeader 1
    foreach inputHeaderType [dict get $serviceInfo operation $operationName soapRequestHeader] {
        if {[string equal $inputHeaderType {}]} {
            continue
        }
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType] xns]
        if {$firstHeader} {
            $env appendChild [$doc createElement "SOAP-ENV:Header" header]
            set firstHeader 0
        }
        $header appendChild [$doc createElement $xns:$inputHeaderType headerData]
        ::WS::Utils::convertDictToType Client $serviceName $doc $headerData $argList $inputHeaderType
    }

    $env appendChild [$doc createElement "SOAP-ENV:Body" bod]
    set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $msgType] xns]

    $bod appendChild [$doc createElement $xns:$msgType reply]

    ::log::log debug "calling [list ::WS::Utils::convertDictToType Client $serviceName $doc $reply $argList $msgType]"
    ::WS::Utils::convertDictToType Client $serviceName $doc $reply $argList $msgType

    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] {} xml
    $doc delete

    ::log::log debug "Leaving ::::WS::Client::buildDocLiteralCallquery with {$xml}"

    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildRpcEncodedCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#                         This is for both the Soap Header and Body messages.
#
# Returns : The XML for the call
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::buildRpcEncodedCallquery {serviceName operationName url argList} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo operation $operationName inputs]
    #set url [dict get $serviceInfo service location]
    set xnsList [dict get $serviceInfo targetNamespace]
    #set action [dict get $serviceInfo operation $operationName action]

    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute \
        xmlns:SOAP-ENV "http://schemas.xmlsoap.org/soap/envelope/" \
        xmlns:xsi      "http://www.w3.org/2001/XMLSchema-instance" \
        xmlns:xs      "http://www.w3.org/2001/XMLSchema"
    foreach xns $xnsList {
        set tns [lindex $xns 0]
        set target [lindex $xns 1]
        $env  setAttribute \
            xmlns:$tns $target
    }

    foreach inputHeaderType [dict get $serviceInfo operation $operationName soapRequestHeader] {
        if {[string equal $inputHeaderType {}]} {
            continue
        }
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType] xns]
        $env appendChild [$doc createElement "SOAP-ENV:Header" header]
        $header appendChild [$doc createElement $xns:$inputHeaderType headerData]
        ::WS::Utils::convertDictToEncodedType Client $serviceName $doc $headerData $argList $inputHeaderType
    }

    $env appendChild [$doc createElement "SOAP-ENV:Body" bod]

    $bod appendChild [$doc createElement tns1:$operationName reply]
    $reply  setAttribute \
        SOAP-ENV:encodingStyle "http://schemas.xmlsoap.org/soap/encoding/"

    ::WS::Utils::convertDictToEncodedType Client $serviceName $doc $reply $argList $msgType

    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] {} xml
    $doc delete
    ::log::log debug "Leaving ::::WS::Client::buildRpcEncodedCallquery with {$xml}"

    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildServiceInfo
#
# Description : Parse the WSDL into our internal representation
#
# Arguments :
#    wsdlNode   - The top node of the WSDL
#    results    - Inital definition. This is optional and defaults to no definition.
#    serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed WSDL
#
# Side-Effects : Defines Client mode types as specified by the WSDL
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::buildServiceInfo {wsdlNode {serviceInfo {}} {serviceAlias {}}} {
    ##
    ## Need to refactor to foreach service parseService
    ##  Service drills down to ports, which drills down to bindings and messages
    ##
    ::log::log debug "Entering ::WS::Client::buildServiceInfo with doc $wsdlNode"

    ##
    ## Parse Service information
    ##
    set serviceNameList [$wsdlNode selectNodes w:service]
    if {[string length $serviceAlias] & ([llength $serviceNameList] > 1)} {
        return \
            -code error \
            -errorcode [list WS CLIENT MULTISVC] \
            "Can not specify alias when WSDL defines multiple services"
    } elseif {[llength $serviceNameList] == 0} {
        return \
            -code error \
            -errorcode [list WS CLIENT NOSVC] \
            "WSDL does not define any services"
    }


    foreach serviceNode $serviceNameList {
        lappend serviceInfo [parseService $wsdlNode $serviceNode $serviceAlias]
    }
    return $serviceInfo
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseService
#
# Description : Parse a service from a WSDL into our internal representation
#
# Arguments :
#    wsdlNode     - The top node of the WSDL
#    serviceNode  - The DOM node for the service.
#    serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed service WSDL
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseService {wsdlNode serviceNode serviceAlias} {
    if {[string length $serviceAlias]} {
        set serviceName $serviceAlias
    } else {
        set serviceName [$serviceNode getAttribute name]
    }
    set addressNodeList [$serviceNode getElementsByTagNameNS http://schemas.xmlsoap.org/wsdl/soap/ address]
    if {[llength $addressNodeList] == 1} {
        set addressNode [lindex $addressNodeList 0]
        set portNode [$addressNode parentNode]
        set location [$addressNode getAttribute location]
    } else {
        foreach addressNode $addressNodeList {
            set portNode [$addressNode parentNode]
            if {[$addressNode hasAttribute location]} {
                set location [$addressNode getAttribute location]
                break
            }
        }
    }
    if {![info exists location]} {
        return \
            -code error \
            -errorcode [list WS CLIENT NOSOAPADDR] \
            "Malformed WSDL -- No SOAP address node found."
    }
    dict set serviceInfo service name $serviceName
    dict set serviceInfo service location $location
    set bindingName [lindex [split [$portNode getAttribute binding] {:}] end]

    ##
    ## Parse types
    ##
    parseTypes $wsdlNode $serviceName serviceInfo

    ##
    ## Parse bindings
    ##
    parseBinding $wsdlNode $serviceName $bindingName serviceInfo

    ##
    ## All done, so return results
    ##
    return $serviceInfo
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseTypes
#
# Description : Parse the types for a service from a WSDL into
#               our internal representation
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceNode    - The DOM node for the service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : Nothing
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseTypes {wsdlNode serviceName serviceInfoVar} {
    upvar $serviceInfoVar serviceInfo

    set tnsCount 0
    set baseUrl [dict get $serviceInfo service location]
    foreach schemaNode [$wsdlNode selectNodes w:types/s:schema] {
        ::WS::Utils::parseScheme Client $baseUrl $schemaNode $serviceName serviceInfo tnsCount
    }

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseBinding
#
# Description : Parse the bindings for a service from a WSDL into our
#               internal representation
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceName    - The name service.
#    bindingName    - The name binding we are to parse.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : Nothing
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseBinding {wsdlNode serviceName bindingName serviceInfoVar} {
    upvar $serviceInfoVar serviceInfo

    set bindQuery [format {w:binding[attribute::name='%s']} $bindingName]
    array set msgToOper {}
    foreach binding [$wsdlNode selectNodes $bindQuery] {
        array unset msgToOper *
        set portName [lindex [split [$binding  getAttribute type] {:}] end]
        set operList [$binding selectNodes w:operation]
        set styleNode [$binding selectNodes d:binding]
        if {![info exists style]} {
            if {[catch {$styleNode getAttribute style} tmpStyle]} {
                set styleNode [$binding selectNodes {w:operation[1]/d:operation}]
                if {[string equal $styleNode {}]} {
                    ##
                    ## This binding is for a SOAP level other than 1.1
                    ##
                    continue
                }
                set style [$styleNode getAttribute style]
                #puts "Using style for first operation {$style}"
            } else {
                set style $tmpStyle
                #puts "Using style for first binding {$style}"
            }
            if {!([string equal $style document] || [string equal $style rpc])} {
                return \
                    -code error \
                    -errorcode [list WSCLIENT UNSSTY $style] \
                    "Unsupported calling style: '$style'"
            }

            if {![info exists use]} {
                set use [[$binding selectNodes {w:operation[1]/w:input/d:body}] getAttribute use]
                if {!([string equal $style document] && [string equal $use literal]) &&
                    !([string equal $style rpc] && [string equal $use encoded])} {
                    return \
                        -code error \
                        -errorcode [list WSCLIENT UNSMODE $use] \
                        "Unsupported mode: $style/$use"
                }
            }
        }

        ##
        ## Process each operation
        ##
        foreach oper $operList {
            set operName [$oper getAttribute name]
            dict lappend serviceInfo operList $operName

            #puts "Processing operation $operName"
            set actionNode [$oper selectNodes d:operation]
            if {[string equal $actionNode {}]} {
                continue
            }
            dict set serviceInfo operation $operName style $style/$use
            catch {
                set action [$actionNode getAttribute soapAction]
                dict set serviceInfo operation $operName action $action
            }

            ##
            ## Get the input headers, if any
            ##
            set soapRequestHeaderList {{}}
            foreach inHeader [$oper selectNodes w:input/d:header] {
                set part [$inHeader getAttribute part]
                set tmp [$inHeader getAttribute use]
                if {![string equal $tmp $use]} {
                    return \
                        -code error \
                        -errorcode [list WSCLIENT MIXUSE $use $tmp] \
                        "Mixed usageage not supported!'"
                }
                set messagePath [$inHeader getAttribute message]
                set msgName [lindex [split $messagePath {:}] end]
                set type [messageToType $wsdlNode $serviceName $operName $msgName serviceInfo]
                lappend soapRequestHeaderList $type
            }
            dict set serviceInfo operation $operName soapRequestHeader $soapRequestHeaderList
            if {![dict exists [dict get $serviceInfo operation $operName] action]} {
                dict set serviceInfo operation $operName action $serviceName
            }

            ##
            ## Get the output header, if one
            ##
            set soapReplyHeaderList {{}}
            foreach outHeader [$oper selectNodes w:output/d:header] {
                set part [$outHeader getAttribute part]
                set tmp [$outHeader getAttribute use]
                if {![string equal $tmp $use]} {
                    return \
                        -code error \
                        -errorcode [list WSCLIENT MIXUSE $use $tmp] \
                        "Mixed usageage not supported!'"
                }
                set messagePath [$outHeader getAttribute message]
                set msgName [lindex [split $messagePath {:}] end]
                set type [messageToType $wsdlNode $serviceName $operName $msgName serviceInfo]
                lappend soapReplyHeaderList $type
            }
            dict set serviceInfo operation $operName soapReplyHeader $soapReplyHeaderList

            ##
            ## Validate that the input and output uses
            ##
            set inUse $use
            set outUse $use
            catch {set inUse [[$oper selectNodes w:input/d:body] getAttribute use]}
            catch {set outUse [[$oper selectNodes w:output/d:body] getAttribute use]}
            foreach tmp [list $inUse $outUse] {
                if {![string equal $tmp $use]} {
                    return \
                        -code error \
                        -errorcode [list WSCLIENT MIXUSE $use $tmp] \
                        "Mixed usageage not supported!'"
                }
            }
            set typeList [getTypesForPort $wsdlNode $serviceName $operName $portName serviceInfo]
            foreach type $typeList mode {inputs outputs} {
                dict set serviceInfo operation $operName $mode $type
            }
            ##
            ## Handle target namespace defined at WSDL level for older RPC/Encoded
            ##
            if {![dict exists $serviceInfo targetNamespace]} {
                catch {
                    #puts "attempting to get tragetNamespace"
                    dict lappend serviceInfo targetNamespace [list tns1 [[$oper selectNodes w:input/d:body] getAttribute namespace]]
                }
            }
        }
    }
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::getTypesForPort
#
# Description : Get the types for a port.
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceNode    - The DOM node for the service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : A list containing the input and output types
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::getTypesForPort {wsdlNode serviceName operName portName serviceInfoVar} {
    upvar $serviceInfoVar serviceInfo

    set style [dict get $serviceInfo operation $operName style]
    set inType {}
    set outType {}

    set portQuery [format {w:portType[attribute::name='%s']} $portName]
    set portNode [lindex [$wsdlNode selectNodes $portQuery] 0]
    set operQuery [format {w:operation[attribute::name='%s']} $operName]
    set operNode [lindex [$portNode selectNodes $operQuery] 0]

    set inputMsgNode [$operNode selectNodes {w:input}]
    if {![string equal $inputMsgNode {}]} {
        set inputMsgPath [$inputMsgNode getAttribute message]
        set inputMsg [lindex [split $inputMsgPath {:}] end]
        set inType [messageToType $wsdlNode $serviceName $operName $inputMsg serviceInfo]
    }

    set outputMsgNode [$operNode selectNodes {w:output}]
    if {![string equal $outputMsgNode {}]} {
        set outputMsgPath [$outputMsgNode getAttribute message]
        set outputMsg [lindex [split $outputMsgPath {:}] end]
        set outType [messageToType $wsdlNode $serviceName $operName $outputMsg serviceInfo]
    }

    ##
    ## Return the types
    ##
    return [list $inType $outType]
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::messageToType
#
# Description : Get a type name from a message
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceName    - The name of the service.
#    operName       - The name of the operation.
#    msgName        - The name of the message.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : The requested type name
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::messageToType {wsdlNode serviceName operName msgName serviceInfoVar} {
    upvar $serviceInfoVar serviceInfo

    #puts "Message to Type $serviceName $operName $msgName"

    set style [dict get $serviceInfo operation $operName style]
    set msgQuery [format {w:message[attribute::name='%s']} $msgName]
    set msg [$wsdlNode selectNodes $msgQuery]
    switch $style {
        document/literal {
            set partNode [$msg selectNodes w:part]
            set partNodeCount [llength $partNode]
            if {$partNodeCount == 1} {
                if {[$partNode hasAttribute element]} {
                    set typePath [$partNode getAttribute element]
                    set type [lindex [split $typePath {:}] end]
                }
            }
            if {($partNodeCount > 1) || ![info exist type]} {
                set tmpType {}
                foreach part [$msg selectNodes w:part] {
                    set partName [$part getAttribute name]
                    if {[$part hasAttribute type]} {
                        set partType [lindex [split [$part getAttribute type] {:}] end]
                    } else {
                        set partType [lindex [split [$part getAttribute element] {:}] end]
                    }
                    lappend tmpType $partName [list type $partType comment {}]
                }
                set  type $msgName
                dict set serviceInfo types $type $tmpType
                ::WS::Utils::ServiceTypeDef Client $serviceName $type $tmpType
            } elseif {!$partNodeCount} {
                return \
                    -code error \
                    -errorCode [list WS CLIENT BADMSGSEC $msgName] \
                    "Invalid format for message '$msgName'"
            }
        }
        rpc/encoded {
            set tmpType {}
            foreach part [$msg selectNodes w:part] {
                set partName [$part getAttribute name]
                if {[$part hasAttribute type]} {
                    set partType [lindex [split [$part getAttribute type] {:}] end]
                } else {
                    set partType [lindex [split [$part getAttribute element] {:}] end]
                }
                lappend tmpType $partName [list type $partType comment {}]
            }
            set  type $msgName
            dict set serviceInfo types $type $tmpType
            ::WS::Utils::ServiceTypeDef Client $serviceName $type $tmpType xs
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKSTYUSE [list $style $use]] \
                "Unknown style/use combination $style/$use"
        }
    }

    ##
    ## Return the type name
    ##
    return $type
}
