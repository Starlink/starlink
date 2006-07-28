#-----------------------------------------------------------------------------
#   Copyright (C) 1999-2004 Jochen C. Loewer (loewerj@web.de)
#-----------------------------------------------------------------------------
#
#   A (partial) LDAPv3 protocol implementation in plain Tcl.
#
#   See RFC 2251 and ASN.1 (X.680) and BER (X.690).
#
#
#   This software is copyrighted by Jochen C. Loewer (loewerj@web.de). The
#   following terms apply to all files associated with the software unless
#   explicitly disclaimed in individual files.
#
#   The authors hereby grant permission to use, copy, modify, distribute,
#   and license this software and its documentation for any purpose, provided
#   that existing copyright notices are retained in all copies and that this
#   notice is included verbatim in any distributions. No written agreement,
#   license, or royalty fee is required for any of the authorized uses.
#   Modifications to this software may be copyrighted by their authors
#   and need not follow the licensing terms described here, provided that
#   the new terms are clearly indicated on the first page of each file where
#   they apply.
#
#   IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
#   FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
#   ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
#   DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#
#   THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
#   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
#   IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
#   NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
#   MODIFICATIONS.
#
#
#   $Log$
#   Revision 1.2  2006/07/28 08:52:44  pwd
#   Merge the gaia-xmlrpc branch to MAIN 28/7/06
#
#   Revision 1.1.2.1  2006/06/09 13:19:46  pwd
#   Various packages (pure Tcl) to provide a built-in web and soap/XML-RPC
#   server. Here until it's decided where to keep them.
#
#   Revision 1.7  2005/07/20 11:58:17  mic42
#      * ldap.tcl: Applied fix for bug 1239915. Thanks to Pierre David for the patch.
#      * pkgIndex.tcl: Version raised to 1.2.1
#
#   Revision 1.6  2005/03/16 18:21:51  andreas_kupries
#
#       * ldap.tcl (ldap::asnGetInteger): Fixed [SF Tcllib Bug 1164663], a
#         copy/paste bug in the definition of this procedure. It belongs
#         into the ldap namespace, not the asn namespace.
#
#   Revision 1.5  2005/02/16 03:54:24  andreas_kupries
#   Reformatting for indentation, trimmed trailing whitespace.
#
#   ldap merge, manual help required.
#
#   Import of fixes for ldap by Michael Schlenker, cross-ported
#   from the asn fixes.
#
#   Import of asn fixes by Michael Schlenker.
#
#   More fixes and 8.5 feature removal for the compiler
#   packages.
#
#   Revision 1.4  2005/02/15 19:05:16  mic42
#   Fixed various issues with signed/unsigned values in the ldap module by crossporting from the asn module
#
#   Revision 1.3  2004/09/24 06:54:24  andreas_kupries
#   Scattered small fixes, mostly adding braces to unbraced
#   expressions.
#
#   Fixed problem with mismatched package names for the packages
#   implementing the standard types.
#
#   Revision 1.1  2004/04/27 19:45:35  andreas_kupries
#
#       * installed_modules.tcl: Added new module.
#       * examples/ldap:
#       * modules/ldap: New module: LDAP client. Provided to us by Joechen
#         Loewer <loewerj@web.de>.
#
#       * Added doctools documentation.
#
#   Revision 1.1  2000/03/23  17:40:22  17:40:22  jolo (Jochen Loewer)
#   Initial revision
#
#
#   written by Jochen Loewer
#   3 June, 1999
#
#-----------------------------------------------------------------------------

package require Tcl 8.4
package provide ldap 1.2.1


namespace eval ldap {

    namespace export  connect secure_connect \
                      disconnect             \
                      bind unbind            \
                      search                 \
                      modify                 \
                      add                    \
                      delete                 \
                      modifyDN

    variable SSLCertifiedAuthoritiesFile
    variable doDebug

    set doDebug 0

    array set resultCode2String {
         0  success
         1  operationsError
         2  protocolError
         3  timeLimitExceeded
         4  sizeLimitExceeded
         5  compareFalse
         6  compareTrue
         7  authMethodNotSupported
         8  strongAuthRequired
        10  referral
        11  adminLimitExceeded
        12  unavailableCriticalExtension
        13  confidentialityRequired
        14  saslBindInProgress
        16  noSuchAttribute
        17  undefinedAttributeType
        18  inappropriateMatching
        19  constraintViolation
        20  attributeOrValueExists
        21  invalidAttributeSyntax
        32  noSuchObject
        33  aliasProblem
        34  invalidDNSyntax
        35  isLeaf
        36  aliasDereferencingProblem
        48  inappropriateAuthentication
        49  invalidCredentials
        50  insufficientAccessRights
        51  busy
        52  unavailable
        53  unwillingToPerform
        54  loopDetect
        64  namingViolation
        65  objectClassViolation
        66  notAllowedOnNonLeaf
        67  notAllowedOnRDN
        68  entryAlreadyExists
        69  objectClassModsProhibited
        80  other
    }
}


#-----------------------------------------------------------------------------
#    connect
#
#-----------------------------------------------------------------------------
proc ldap::connect { host {port 389} } {

    #--------------------------------------
    #   connect via TCP/IP
    #--------------------------------------
    set sock [socket $host $port]
    fconfigure $sock -blocking yes -translation binary

    #--------------------------------------
    #   initialize connection array
    #--------------------------------------
    upvar ::ldap::ldap$sock conn
    catch { unset conn }

    set conn(sock)      $sock
    set conn(messageId) 0

    return ::ldap::ldap$sock
}

#-----------------------------------------------------------------------------
#    secure_connect
#
#-----------------------------------------------------------------------------
proc ldap::secure_connect { host {port 636} } {

    variable SSLCertifiedAuthoritiesFile

    package require tls

    #------------------------------------------------------------------
    #   connect via TCP/IP
    #------------------------------------------------------------------
    set sock [socket $host $port]
    fconfigure $sock -blocking yes -translation binary

    #------------------------------------------------------------------
    #   make it a SSL connection
    #
    #------------------------------------------------------------------
    #tls::import $sock -cafile $SSLCertifiedAuthoritiesFile -ssl2 no -ssl3 yes -tls1 yes
    tls::import $sock -cafile "" -certfile "" -keyfile "" \
                      -request 1 -server 0 -require 0 -ssl2 no -ssl3 yes -tls1 yes
    set retry 0
    while {1} {
        if {$retry > 20} {
            close $sock
            return -code error "too long retry to setup SSL connection"
        }
        if {[catch { tls::handshake $sock } err]} {
            if {[string match "*resource temporarily unavailable*" $err]} {
                after 50
                incr retry
            } else {
                close $sock
                return -code error $err
            }
        } else {
            break
        }
    }
    #puts stderr [tls::status $sock]

    #--------------------------------------
    #   initialize connection array
    #--------------------------------------
    upvar ::ldap::ldap$sock conn
    catch { unset conn }

    set conn(sock)      $sock
    set conn(messageId) 0

    return ::ldap::ldap$sock
}


#-----------------------------------------------------------------------------
#    bind  -  does a bind with simple authentication
#
#-----------------------------------------------------------------------------
proc ldap::bind { handle {name ""} {password ""} } {

    upvar $handle conn

    incr conn(messageId)

    #-----------------------------------------------------------------
    #   marshal bind request packet and send it
    #
    #-----------------------------------------------------------------
    set request [asnSequence                           \
                    [asnInteger $conn(messageId)]      \
                    [asnApplicationConstr 0            \
                        [asnInteger 3]                 \
                        [asnOctetString $name]         \
                        [asnChoice 0 $password]        \
                    ]                                  \
                ]
    debugData bindRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)

    #-----------------------------------------------------------------
    #   receive (blocking) bind response packet(s) and unmarshal it
    #
    #-----------------------------------------------------------------
    asnGetResponse $conn(sock) response
    debugData bindResponse $response

    asnGetSequence response response
    asnGetInteger  response MessageID
    if { $MessageID != $conn(messageId) } {
        error "umatching response packet ($MessageID != $conn(messageId))"
    }
    asnGetApplication response appNum
    if { $appNum != 1 } {
        error "unexpected application number ($appNum != 1)"
    }
    asnGetEnumeration response resultCode
    asnGetOctetString response matchedDN
    asnGetOctetString response errorMessage
    if {$resultCode != 0} {
        return -code error "LDAP error $ldap::resultCode2String($resultCode) '$matchedDN': $errorMessage"
    }
}


#-----------------------------------------------------------------------------
#    unbind
#
#-----------------------------------------------------------------------------
proc ldap::unbind { handle } {

    upvar $handle conn

    incr conn(messageId)

    #------------------------------------------------
    #   marshal unbind request packet and send it
    #------------------------------------------------
    set request [asnSequence                      \
                    [asnInteger $conn(messageId)] \
                    [asnApplication 2 ""]         ]

    debugData unbindRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)
}


#-----------------------------------------------------------------------------
#    buildUpFilter  -   parses the text representation of LDAP search
#                       filters and transforms it into the correct
#                       marshalled representation for the search request
#                       packet
#
#-----------------------------------------------------------------------------
proc ldap::buildUpFilter { filter } {

    set first [lindex $filter 0]
    set data ""
    switch -regexp -- $first {
        ^\\&$ {  #--- and -------------------------------------------
            foreach term [lrange $filter 1 end] {
                append data [buildUpFilter $term]
            }
            return [asnChoiceConstr 0 $data]
        }
        ^\\|$ {  #--- or --------------------------------------------
            foreach term [lrange $filter 1 end] {
                append data [buildUpFilter $term]
            }
            return [asnChoiceConstr 1 $data]
        }
        ^\\!$ {  #--- not -------------------------------------------
            return [asnChoiceConstr 2 [buildUpFilter [lindex $filter 1]]]
        }
        =\\*$ {  #--- present ---------------------------------------
            set endpos [expr {[string length $first] -3}]
            set attributetype [string range $first 0 $endpos]
            return [asnChoice 7 $attributetype]
        }
        ^[0-9A-z.]*~= {  #--- approxMatch --------------------------
            regexp {^([0-9A-z.]*)~=(.*)$} $first all attributetype value
            return [asnChoiceConstr 8 [asnOctetString $attributetype] \
                                      [asnOctetString $value]         ]
        }
        ^[0-9A-z.]*<= {  #--- lessOrEqual --------------------------
            regexp {^([0-9A-z.]*)<=(.*)$} $first all attributetype value
            return [asnChoiceConstr 6 [asnOctetString $attributetype] \
                                      [asnOctetString $value]         ]
        }
        ^[0-9A-z.]*>= {  #--- greaterOrEqual -----------------------
            regexp {^([0-9A-z.]*)>=(.*)$} $first all attributetype value
            return [asnChoiceConstr 5 [asnOctetString $attributetype] \
                                      [asnOctetString $value]         ]
        }
        ^[0-9A-z.]*=.*\\*.* {  #--- substrings -----------------
            regexp {^([0-9A-z.]*)=(.*)$} $first all attributetype value
            regsub -all {\*+} $value {*} value
            set value [split $value "*"]
            
            set firstsubstrtype 0       ;# initial
            set lastsubstrtype  2       ;# final
            if {[string equal [lindex $value 0] ""]} {
                set firstsubstrtype 1       ;# any
                set value [lreplace $value 0 0]
            }
            if {[string equal [lindex $value end] ""]} {
                set lastsubstrtype 1        ;# any
                set value [lreplace $value end end]
            }
        
            set n [llength $value]
        
            set i 1
            set l {}
            set substrtype 0            ;# initial
            foreach str $value {
            if {$i == 1 && $i == $n} {
                if {$firstsubstrtype == 0} {
                set substrtype 0    ;# initial
                } elseif {$lastsubstrtype == 2} {
                set substrtype 2    ;# final
                } else {
                set substrtype 1    ;# any
                }
            } elseif {$i == 1} {
                set substrtype $firstsubstrtype
            } elseif {$i == $n} {
                set substrtype $lastsubstrtype
            } else {
                set substrtype 1        ;# any
            }
            lappend l [asnChoice $substrtype $str]
            incr i
            }
            return [asnChoiceConstr 4 [asnOctetString $attributetype]     \
                      [asnSequenceFromList $l] ]
        }
        ^[0-9A-z.]*= {  #--- equal ---------------------------------
            regexp {^([0-9A-z.]*)=(.*)$} $first all attributetype value
            trace "equal: attributetype='$attributetype' value='$value'"
            return [asnChoiceConstr 3 [asnOctetString $attributetype] \
                                      [asnOctetString $value]         ]
        }
        default {
            return [buildUpFilter $first]
            #error "cant handle $first for filter part"
        }
    }
}

#-----------------------------------------------------------------------------
#    search  -  performs a LDAP search below the baseObject tree using a
#               complex LDAP search expression (like "|(cn=Linus*)(sn=Torvalds*)"
#               and returns all matching objects (DNs) with given attributes
#               (or all attributes if empty list is given) as list:
#
#               dn1 { attr1 val1 attr2 val2 ... } dn2 { a1 v1 } ....
#
#-----------------------------------------------------------------------------
proc ldap::search { handle baseObject filterString attributes } {

    upvar $handle conn

    set scope        2
    set derefAliases 0
    set sizeLimit    0
    set timeLimit    0
    set attrsOnly    0

    #----------------------------------------------------------
    #   marshal filter and attributes parameter
    #----------------------------------------------------------
    regsub -all {\(} $filterString " \{" filterString
    regsub -all {\)} $filterString "\} " filterString

    set berFilter [buildUpFilter $filterString]

    set berAttributes ""
    foreach attribute $attributes {
        append berAttributes [asnOctetString $attribute]
    }

    #----------------------------------------------------------
    #   marshal search request packet and send it
    #----------------------------------------------------------
    incr conn(messageId)
    set request [asnSequence                            \
                    [asnInteger $conn(messageId)]       \
                    [asnApplicationConstr 3             \
                        [asnOctetString $baseObject]    \
                        [asnEnumeration $scope]         \
                        [asnEnumeration $derefAliases]  \
                        [asnInteger     $sizeLimit]     \
                        [asnInteger     $timeLimit]     \
                        [asnBoolean     $attrsOnly]     \
                        $berFilter                      \
                        [asnSequence    $berAttributes] \
                    ]                                   \
                ]
    debugData searchRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)

    #----------------------------------------------------------
    #   receive (blocking) search response packet(s)
    #----------------------------------------------------------
    set results    {}
    set lastPacket 0
    while { !$lastPacket } {

        asnGetResponse $conn(sock) response
        debugData searchResponse $response

        asnGetSequence response response
        asnGetInteger  response MessageID
        if { $MessageID != $conn(messageId) } {
            error "umatching response packet ($MessageID != $conn(messageId))"
        }
        asnGetApplication response appNum
        if { ($appNum != 4) && ($appNum != 5) } {
             error "unexpected application number ($appNum != 4 or 5)"
        }
        if {$appNum == 4} {
            #----------------------------------------------------------
            #   unmarshal search data packet
            #----------------------------------------------------------
            asnGetOctetString response objectName
            asnGetSequence    response attributes
            set result_attributes {}
            while { [string length $attributes] != 0 } {
                asnGetSequence attributes attribute
                asnGetOctetString attribute attrType
                asnGetSet  attribute attrValues
                set result_attrValues {}
                while { [string length $attrValues] != 0 } {
                    asnGetOctetString attrValues attrValue
                    lappend result_attrValues $attrValue
                }
                lappend result_attributes $attrType $result_attrValues
            }
            lappend results [list $objectName $result_attributes]
        }
        if {$appNum == 5} {
            #----------------------------------------------------------
            #   unmarshal search final response packet
            #----------------------------------------------------------
            asnGetEnumeration response resultCode
            asnGetOctetString response matchedDN
            asnGetOctetString response errorMessage
            if {$resultCode != 0} {
                return -code error "LDAP error $ldap::resultCode2String($resultCode): $errorMessage"
            }
            set lastPacket 1
        }
    }
    return $results
}


#-----------------------------------------------------------------------------
#    modify  -  provides attribute modifications on one single object (DN):
#                 o replace attributes with new values
#                 o delete attributes (having certain values)
#                 o add attributes with new values
#
#-----------------------------------------------------------------------------
proc ldap::modify { handle dn
                    attrValToReplace { attrToDelete {} } { attrValToAdd {} } } {

    upvar $handle conn

    set operationAdd     0
    set operationDelete  1
    set operationReplace 2

    #------------------------------------------------------------------
    #   marshal attribute modify operations
    #    - always mode 'replace' ! see rfc2251:
    #
    #        replace: replace all existing values of the given attribute
    #        with the new values listed, creating the attribute if it
    #        did not already exist.  A replace with no value will delete
    #        the entire attribute if it exists, and is ignored if the
    #        attribute does not exist.
    #
    #------------------------------------------------------------------
    set modifications {}
    foreach { attrName attrValue } $attrValToReplace {
        append modifications [asnSequence                            \
                                 [asnEnumeration $operationReplace ] \
                                 [asnSequence                        \
                                    [asnOctetString $attrName  ]     \
                                    [asnSet                          \
                                        [asnOctetString $attrValue ] \
                                    ]                                \
                                 ]                                   \
                             ]
    }

    #------------------------------------------------------------------
    #   marshal attribute add operations
    #
    #------------------------------------------------------------------
    foreach { attrName attrValue } $attrValToAdd {
        append modifications [asnSequence                            \
                                 [asnEnumeration $operationAdd ]     \
                                 [asnSequence                        \
                                    [asnOctetString $attrName  ]     \
                                    [asnSet                          \
                                        [asnOctetString $attrValue ] \
                                    ]                                \
                                 ]                                   \
                             ]
    }

    #------------------------------------------------------------------
    #   marshal attribute delete operations
    #
    #     - a non-empty value will trigger to delete only those
    #       attributes which have the same value as the given one
    #
    #     - an empty value will trigger to delete the attribute
    #       in all cases
    #
    #------------------------------------------------------------------
    foreach { attrName attrValue } $attrToDelete {
        if {$attrValue == ""} {
            set val [asnSet ""]
        } else {
            set val [asnSet [asnOctetString $attrValue]]
        }
        append modifications [asnSequence                            \
                                 [asnEnumeration $operationDelete ]  \
                                 [asnSequence                        \
                                    [asnOctetString $attrName  ]     \
                                    $val                             \
                                 ]                                   \
                             ]
    }

    #----------------------------------------------------------
    #   marshal 'modify' request packet and send it
    #----------------------------------------------------------
    incr conn(messageId)
    set request [asnSequence                             \
                    [asnInteger $conn(messageId)]        \
                    [asnApplicationConstr 6              \
                        [asnOctetString $dn ]            \
                        [asnSequence    $modifications ] \
                    ]                                    \
                ]
    debugData modifyRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)

    #-----------------------------------------------------------------------
    #   receive (blocking) 'modify' response packet(s) and unmarshal it
    #-----------------------------------------------------------------------
    asnGetResponse $conn(sock) response
    debugData bindResponse $response

    asnGetSequence response response
    asnGetInteger  response MessageID

    if { $MessageID != $conn(messageId) } {
        error "umatching response packet ($MessageID != $conn(messageId))"
    }
    asnGetApplication response appNum
    if { $appNum != 7 } {
         error "unexpected application number ($appNum != 7)"
    }
    asnGetEnumeration response resultCode
    asnGetOctetString response matchedDN
    asnGetOctetString response errorMessage
    if {$resultCode != 0} {
        return -code error "LDAP error $ldap::resultCode2String($resultCode) $matchedDN: $errorMessage"
    }
}


#-----------------------------------------------------------------------------
#    add  -  will create a new object using given DN and sets the given
#            attributes
#
#-----------------------------------------------------------------------------
proc ldap::add { handle dn attrValueTuples } {

    upvar $handle conn

    #------------------------------------------------------------------
    #   marshal attribute list
    #
    #------------------------------------------------------------------
    set attrList ""
    foreach { attrName attrValue } $attrValueTuples {
        append attrList [asnSequence                         \
                            [asnOctetString $attrName ]      \
                            [asnSet                          \
                                [asnOctetString $attrValue ] \
                            ]                                \
                        ]
    }

    #----------------------------------------------------------
    #   marshal search 'add' request packet and send it
    #----------------------------------------------------------
    incr conn(messageId)
    set request [asnSequence                        \
                    [asnInteger $conn(messageId)]   \
                    [asnApplicationConstr 8         \
                        [asnOctetString $dn       ] \
                        [asnSequence    $attrList ] \
                    ]                               \
                ]

    debugData addRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)

    #-----------------------------------------------------------------------
    #   receive (blocking) 'add' response packet(s) and unmarshal it
    #
    #-----------------------------------------------------------------------
    asnGetResponse $conn(sock) response
    debugData bindResponse $response

    asnGetSequence response response
    asnGetInteger  response MessageID

    if { $MessageID != $conn(messageId) } {
        error "umatching response packet ($MessageID != $conn(messageId))"
    }
    asnGetApplication response appNum
    if { $appNum != 9 } {
         error "unexpected application number ($appNum != 9)"
    }
    asnGetEnumeration response resultCode
    asnGetOctetString response matchedDN
    asnGetOctetString response errorMessage
    if {$resultCode != 0} {
        return -code error "LDAP error $ldap::resultCode2String($resultCode) $matchedDN: $errorMessage"
    }
}

#-----------------------------------------------------------------------------
#    delete  -  removes the whole object (DN) inclusive all attributes
#
#-----------------------------------------------------------------------------
proc ldap::delete { handle dn } {

    upvar $handle conn

    #----------------------------------------------------------
    #   marshal 'delete' request packet and send it
    #----------------------------------------------------------
    incr conn(messageId)
    set request [asnSequence                      \
                    [asnInteger $conn(messageId)] \
                    [asnApplication 10 $dn      ] \
                ]

    debugData deleteRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)

    #-----------------------------------------------------------------------
    #   receive (blocking) 'delete' response packet(s) and unmarshal it
    #
    #-----------------------------------------------------------------------
    asnGetResponse $conn(sock) response
    debugData bindResponse $response

    asnGetSequence response response
    asnGetInteger  response MessageID

    if { $MessageID != $conn(messageId) } {
        error "umatching response packet ($MessageID != $conn(messageId))"
    }
    asnGetApplication response appNum
    if { $appNum != 11 } {
         error "unexpected application number ($appNum != 11)"
    }
    asnGetEnumeration response resultCode
    asnGetOctetString response matchedDN
    asnGetOctetString response errorMessage
    if {$resultCode != 0} {
        return -code error "LDAP error $ldap::resultCode2String($resultCode) $matchedDN: $errorMessage"
    }
}


#-----------------------------------------------------------------------------
#    modifyDN  -  moves an object (DN) to another (relative) place
#
#-----------------------------------------------------------------------------
proc ldap::modifyDN { handle dn newrdn { deleteOld 1 } } {

    upvar $handle conn

    #----------------------------------------------------------
    #   marshal 'modifyDN' request packet and send it
    #----------------------------------------------------------
    incr conn(messageId)
    set request [asnSequence                             \
                    [asnInteger $conn(messageId)]        \
                    [asnApplicationConstr 12             \
                        [asnOctetString $dn ]            \
                        [asnOctetString $newrdn ]        \
                        [asnBoolean     $deleteOld ]     \
                    ]                                    \
                ]
    debugData modifyRequest $request
    puts -nonewline $conn(sock) $request
    flush $conn(sock)

    #-----------------------------------------------------------------------
    #   receive (blocking) 'modifyDN' response packet(s) and unmarshal it
    #-----------------------------------------------------------------------
    asnGetResponse $conn(sock) response
    debugData bindResponse $response

    asnGetSequence response response
    asnGetInteger  response MessageID

    if { $MessageID != $conn(messageId) } {
        error "umatching response packet ($MessageID != $conn(messageId))"
    }
    asnGetApplication response appNum
    if { $appNum != 13 } {
         error "unexpected application number ($appNum != 13)"
    }
    asnGetEnumeration response resultCode
    asnGetOctetString response matchedDN
    asnGetOctetString response errorMessage
    if {$resultCode != 0} {
        return -code error "LDAP error $ldap::resultCode2String($resultCode) $matchedDN: $errorMessage"
    }
}


#-----------------------------------------------------------------------------
#    disconnect
#
#-----------------------------------------------------------------------------
proc ldap::disconnect { handle } {

    upvar $handle conn

    # should we sent an 'unbind' ?
    close $conn(sock)
    unset conn

    return
}


#-----------------------------------------------------------------------------
#    trace
#
#-----------------------------------------------------------------------------
proc ldap::trace { message } {

    variable doDebug

    if {!$doDebug} return

    puts stderr $message
}


#-----------------------------------------------------------------------------
#    debugData
#
#-----------------------------------------------------------------------------
proc ldap::debugData { info data } {

    variable doDebug

    if {!$doDebug} return

    set len [string length $data]
    trace "$info ($len bytes):"
    set address ""
    set hexnums ""
    set ascii   ""
    for {set i 0} {$i < $len} {incr i} {
        set v [string index $data $i]
        binary scan $v H2 hex
        binary scan $v c  num
        set num [expr {( $num + 0x100 ) % 0x100}]
        set text .
        if {$num > 31} {
            set text $v
        }
        if { ($i % 16) == 0 } {
            if {$address != ""} {
                trace [format "%4s  %-48s  |%s|" $address $hexnums $ascii ]
                set address ""
                set hexnums ""
                set ascii   ""
            }
            append address [format "%04d" $i]
        }
        append hexnums "$hex "
        append ascii   $text
        #trace [format "%3d %2s %s" $i $hex $text]
    }
    if {$address != ""} {
        trace [format "%4s  %-48s  |%s|" $address $hexnums $ascii ]
    }
    trace ""
}


#-----------------------------------------------------------------------------
#    asnLength
#
#-----------------------------------------------------------------------------
proc ldap::asnLength { len } {
    if {$len < 0} {
        return -code error "Negative length octet requested"
    }
    if {$len < 128} {
        # short form: ISO X.690 8.1.3.4
        return [binary format c $len]
    }
    # long form: ISO X.690 8.1.3.5
    # try to use a minimal encoding,
    # even if not required by BER, but it is required by DER
    # take care for signed vs. unsigned issues
    if {$len < 256  } {
        return [binary format H2c 81 [expr {$len - 256}]]
    }
    if {$len < 32769} {
        # two octet signed value
        return [binary format H2S 82 $len]
    }
    if {$len < 65536} {
        return [binary format H2S 82 [expr {$len - 65536}]]
    }
    if {$len < 8388608} {
        # three octet signed value
        return [binary format H2cS 83 [expr {$len >> 16}] [expr {($len & 0xFFFF) - 65536}]]
    }
    if {$len < 16777216} {
        # three octet signed value
        return [binary format H2cS 83 [expr {($len >> 16) -256}] [expr {($len & 0xFFFF) -65536}]]
    }
    if {$len < 2147483649} {
        # four octet signed value
        return [binary format H2I 84 $len]
    }
    if {$len < 4294967296} {
        # four octet unsigned value
        return [binary format H2I 84 [expr {$len - 4294967296}]]
    }
    if {$len < 1099511627776} {
        # five octet unsigned value
        return [binary format H2 85][string range [binary format W $len] 3 end]
    }
    if {$len < 281474976710656} {
        # six octet unsigned value
        return [binary format H2 86][string range [binary format W $len] 2 end]
    }
    if {$len < 72057594037927936} {
        # seven octet value
        return [binary format H2 87][string range [binary format W $len] 1 end]
    }

    # must be a 64-bit wide signed value
    return [binary format H2W 88 $len]
}


#-----------------------------------------------------------------------------
#    asnSequence
#
#-----------------------------------------------------------------------------
proc ldap::asnSequence { args } {

    return [asnSequenceFromList $args]
}


#-----------------------------------------------------------------------------
#    asnSequenceFromList
#
#-----------------------------------------------------------------------------
proc ldap::asnSequenceFromList { lst } {

    set out ""
    foreach part $lst {
        append out $part
    }
    set len [string length $out]
    return [binary format H2a*a$len 30 [asnLength $len] $out]
}


#-----------------------------------------------------------------------------
#    asnSet
#
#-----------------------------------------------------------------------------
proc ldap::asnSet { args } {

    set out ""
    foreach part $args {
        append out $part
    }
    set len [string length $out]
    return [binary format H2a*a$len 31 [asnLength $len] $out]
}


#-----------------------------------------------------------------------------
#    asnApplicationConstr
#
#-----------------------------------------------------------------------------
proc ldap::asnApplicationConstr { appNumber args } {

    set out ""
    foreach part $args {
        append out $part
    }
    set code [expr {0x060 + $appNumber}]
    set len  [string length $out]
    return [binary format ca*a$len $code [asnLength $len] $out]
}

#-----------------------------------------------------------------------------
#    asnApplication
#
#-----------------------------------------------------------------------------
proc ldap::asnApplication { appNumber data } {

    set code [expr {0x040 + $appNumber}]
    set len  [string length $data]
    return [binary format ca*a$len $code [asnLength $len] $data]
}


#-----------------------------------------------------------------------------
#    asnChoice
#
#-----------------------------------------------------------------------------
proc ldap::asnChoice { appNumber args } {

    set out ""
    foreach part $args {
        append out $part
    }
    set code [expr {0x080 + $appNumber}]
    set len  [string length $out]
    return [binary format ca*a$len $code [asnLength $len] $out]
}

#-----------------------------------------------------------------------------
#    asnChoiceConstr
#
#-----------------------------------------------------------------------------
proc ldap::asnChoiceConstr { appNumber args } {

    set out ""
    foreach part $args {
        append out $part
    }
    set code [expr {0x0A0 + $appNumber}]
    set len  [string length $out]
    return [binary format ca*a$len $code [asnLength $len] $out]
}

#-----------------------------------------------------------------------------
# asnInteger : Encode integer value.
#-----------------------------------------------------------------------------

proc ::ldap::asnInteger {number} {
    asnIntegerOrEnum 02 $number
}

#-----------------------------------------------------------------------------
# asnEnumeration : Encode enumeration value.
#-----------------------------------------------------------------------------

proc ::ldap::asnEnumeration {number} {
    asnIntegerOrEnum 0a $number
}

#-----------------------------------------------------------------------------
# asnIntegerOrEnum : Common code for Integers and Enumerations
#                    No Bignum version, as we do not expect large Enums.
#-----------------------------------------------------------------------------

proc ::ldap::asnIntegerOrEnum {tag number} {
    # The integer tag is 0x02 , the Enum Tag 0x0a otherwise identical.
    # The length is 1, 2, 3, or 4, coded in a
    # single byte. This can be done directly, no need to go through
    # asnLength. The value itself is written in big-endian.

    # Known bug/issue: The command cannot handle very wide integers, i.e.
    # anything above 8 bytes length. Use asnBignumInteger for those.

    # check if we really have an int
    set num $number
    incr num

    if {($number >= -128) && ($number < 128)} {
        return [binary format H2H2c $tag 01 $number]
    }
    if {($number >= -32768) && ($number < 32768)} {
        return [binary format H2H2S $tag 02 $number]
    }
    if {($number >= -8388608) && ($number < 8388608)} {
        set numberb [expr {$number & 0xFFFF}]
        set numbera [expr {($number >> 16) & 0xFF}]
        return [binary format H2H2cS $tag 03 $numbera $numberb]
    }
    if {($number >= -2147483648) && ($number < 2147483648)} {
        return [binary format H2H2I $tag 04 $number]
    }
    if {($number >= -549755813888) && ($number < 549755813888)} {
        set numberb [expr {$number & 0xFFFFFFFF}]
        set numbera [expr {($number >> 32) & 0xFF}]
        return [binary format H2H2cI $tag 05 $numbera $numberb]
    }
    if {($number >= -140737488355328) && ($number < 140737488355328)} {
        set numberb [expr {$number & 0xFFFFFFFF}]
        set numbera [expr {($number >> 32) & 0xFFFF}]
        return [binary format H2H2SI $tag 06 $numbera $numberb]
    }
    if {($number >= -36028797018963968) && ($number < 36028797018963968)} {
        set numberc [expr {$number & 0xFFFFFFFF}]
        set numberb [expr {($number >> 32) & 0xFFFF}]
        set numbera [expr {($number >> 48) & 0xFF}]
        return [binary format H2H2cSI $tag 07 $numbera $numberb $numberc]
    }
    if {($number >= -9223372036854775808) && ($number <= 9223372036854775807)} {
        return [binary format H2H2W $tag 08 $number]
    }
    return -code error "Integer value to large to encode, use asnBigInteger"
}


#-----------------------------------------------------------------------------
#    asnBoolean
#
#-----------------------------------------------------------------------------
proc ldap::asnBoolean { bool } {

    return [binary format H2H2c 01 01 [expr {$bool ? 0x0FF : 0}]]
}


#-----------------------------------------------------------------------------
#    asnOctetString
#
#-----------------------------------------------------------------------------
proc ldap::asnOctetString { string } {

    set len [string length $string]
    return [binary format H2a*a$len 04 [asnLength $len] $string]
}


#-----------------------------------------------------------------------------
#    asnGetResponse - LDAP specific ?
#
#-----------------------------------------------------------------------------
proc ldap::asnGetResponse { sock data_var } {

    upvar $data_var data

    set tag [read $sock 1]

    if {$tag == "\x30"} {
        set len1 [read $sock 1]
        binary scan $len1 c num
        set length [expr {($num + 0x100) % 0x100}]
        trace "asnGetResponse length=$length"
        if {$length  >= 0x080} {
            set len_length [expr {$length & 0x7f}]
            set lengthBytes [read $sock $len_length]
            switch $len_length {
            1 {
        # Efficiently coded data will not go through this
        # path, as small length values can be coded directly,
        # without a prefix.

                binary scan $lengthBytes     c length
                set length [expr {($length + 0x100) % 0x100}]
            }
            2 {
        binary scan $lengthBytes     S length
                set length [expr {($length + 0x10000) % 0x10000}]
            }
            3 {
        binary scan \x00$lengthBytes I length
                set length [expr {($length + 0x1000000) % 0x1000000}]
            }
            4 {
        binary scan $lengthBytes     I length
                set length [expr {(wide($length) + 0x100000000) % 0x100000000}]
            }
            default {
                binary scan $lengthBytes H* hexstr
                # skip leading zeros which are allowed by BER
                set hexlen [string trimleft $hexstr 0]
                # check if it fits into a 64-bit signed integer
                if {[string length $hexlen] > 16} {
                    return -code {ARITH IOVERFLOW
                            {Length value too large for normal use, try asnGetBigLength}} "Length value to large"
                } elseif {[string length $hexlen] == 16 && ([string index $hexlen 0] & 0x8)} {
                    # check most significant bit, if set we need bignum
                    return -code {ARITH IOVERFLOW
                            {Length value too large for normal use, try asnGetBigLength}} "Length value to large"
                } else {
                    scan $hexstr "%lx" length
                }
            }
            }
        }
        set rest [read $sock $length]
        set data [binary format aa*a$length $tag [asnLength $length] $rest]
    }  else {
        set tag_hex ""
        binary scan $tag H2 tag_hex
        error "unknown start tag [string length $tag] $tag_hex"
    }
}


#-----------------------------------------------------------------------------
#    asnGetByte
#
#-----------------------------------------------------------------------------
proc ldap::asnGetByte { data_var byte_var } {

    upvar $data_var data $byte_var byte

    binary scan [string index $data 0] c byte
    set byte [expr {($byte + 0x100) % 0x100}]
    set data [string range $data 1 end]

    trace "asnGetByte $byte"
}


#-----------------------------------------------------------------------------
#    asnGetBytes
#
#-----------------------------------------------------------------------------
proc ldap::asnGetBytes { data_var length bytes_var } {

    upvar $data_var data  $bytes_var bytes

    incr length -1
    set bytes [string range $data 0 $length]
    incr length
    set data [string range $data $length end]

    debugData asnGetBytes $bytes
}

#-----------------------------------------------------------------------------
# asnGetLength : Decode an ASN length value (See notes)
#-----------------------------------------------------------------------------

proc ::ldap::asnGetLength {data_var length_var} {
    upvar $data_var data  $length_var length

    asnGetByte data length
    if {$length == 0x080} {
        return -code error "Indefinite length BER encoding not yet supported"
    }
    if {$length > 0x080} {
    # The retrieved byte is a prefix value, and the integer in the
    # lower nibble tells us how many bytes were used to encode the
    # length data following immediately after this prefix.

        set len_length [expr {$length & 0x7f}]

        if {[string length $data] < $len_length} {
            return -code error "length information invalid, not enough octets left"
        }

        asnGetBytes data $len_length lengthBytes

        switch $len_length {
            1 {
        # Efficiently coded data will not go through this
        # path, as small length values can be coded directly,
        # without a prefix.

        binary scan $lengthBytes     c length
        set length [expr {($length + 0x100) % 0x100}]
            }
            2 {
        binary scan $lengthBytes     S length
        set length [expr {($length + 0x10000) % 0x10000}]
            }
            3 {
        binary scan \x00$lengthBytes I length
        set length [expr {($length + 0x1000000) % 0x1000000}]
            }
            4 {
        binary scan $lengthBytes     I length
        set length [expr {(wide($length) + 0x100000000) % 0x100000000}]
            }
            default {
                binary scan $lengthBytes H* hexstr
                # skip leading zeros which are allowed by BER
                set hexlen [string trimleft $hexstr 0]
                # check if it fits into a 64-bit signed integer
                if {[string length $hexlen] > 16} {
                    return -code {ARITH IOVERFLOW
                            {Length value too large for normal use, try asnGetBigLength}} \
                    "Length value to large"
                } elseif {[string length $hexlen] == 16 && ([string index $hexlen 0] & 0x8)} {
                    # check most significant bit, if set we need bignum
                    return -code {ARITH IOVERFLOW
                            {Length value too large for normal use, try asnGetBigLength}} \
                    "Length value to large"
                } else {
                    scan $hexstr "%lx" length
                }
            }
        }
    }
    trace "asnGetLength -> length = $length"
    return
}

#-----------------------------------------------------------------------------
# asnGetInteger : Retrieve integer.
#-----------------------------------------------------------------------------

proc ldap::asnGetInteger {data_var int_var} {
    # Tag is 0x02.

    upvar $data_var data $int_var int

    asnGetByte   data tag

    if {$tag != 0x02} {
        return -code error \
            [format "Expected Integer (0x02), but got %02x" $tag]
    }

    asnGetLength data len
    asnGetBytes  data $len integerBytes

    set int ?

    trace "asnGetInteger len=$len"
    switch $len {
        1 { binary scan $integerBytes     c int }
        2 { binary scan $integerBytes     S int }
        3 {
            # check for negative int and pad
            scan [string index $integerBytes 0] %c byte
            if {$byte & 128} {
                binary scan \xff$integerBytes I int
            } else {
                binary scan \x00$integerBytes I int
            }
          }
        4 { binary scan $integerBytes     I int }
        5 -
        6 -
        7 -
        8 {
            # check for negative int and pad
            scan [string index $integerBytes 0] %c byte
            if {$byte & 128} {
                set pad [string repeat \xff [expr {8-$len}]]
            } else {
                set pad [string repeat \x00 [expr {8-$len}]]
            }
            binary scan $pad$integerBytes W int
        }
        default {
        # Too long
            return -code error "length information too long"
        }
    }
    trace "asnGetInteger int=$int"
    return
}



#-----------------------------------------------------------------------------
#    asnGetEnumeration
#
#-----------------------------------------------------------------------------
proc ldap::asnGetEnumeration { data_var enum_var } {

    upvar $data_var data $enum_var enum

    asnGetByte   data tag
    asnGetLength data len
    asnGetBytes  data $len integerBytes

    if {$tag != 0x0a} {
        error "Expected Enumeration, but got $tag"
    }
    set enum ?

    trace "asnGetEnumeration  len=$len"
    switch $len {
        1 { binary scan $integerBytes     c enum }
        2 { binary scan $integerBytes     S enum }
        3 { binary scan \x00$integerBytes I enum }
        4 { binary scan $integerBytes     I enum }
        default {
            error "length information too long"
        }
    }
    trace "asnGetEnumeration enum=$enum"
}


#-----------------------------------------------------------------------------
#    asnGetOctetString
#
#-----------------------------------------------------------------------------
proc ldap::asnGetOctetString { data_var string_var } {

    upvar $data_var data $string_var string

    asnGetByte data byte
    if {$byte != 0x04} {
        error "Got different tag than octet string (0x04)"
    }
    asnGetLength data length
    asnGetBytes  data $length temp
    set string $temp
}


#-----------------------------------------------------------------------------
#    asnGetSequence
#
#-----------------------------------------------------------------------------
proc ldap::asnGetSequence { data_var sequence_var } {

    upvar $data_var data $sequence_var sequence

    asnGetByte data byte
    if {$byte != 0x030} {
        error "Got different tag than sequence (0x030)"
    }
    asnGetLength data length
    asnGetBytes  data $length temp
    set sequence $temp
}


#-----------------------------------------------------------------------------
#    asnGetSet
#
#-----------------------------------------------------------------------------
proc ldap::asnGetSet { data_var set_var } {

    upvar $data_var data $set_var set

    asnGetByte data byte
    if {$byte != 0x031} {
        error "Got different tag than set (0x031)"
    }
    asnGetLength data length
    asnGetBytes  data $length temp
    set set $temp
}


#-----------------------------------------------------------------------------
#    asnGetApplication
#
#-----------------------------------------------------------------------------
proc ldap::asnGetApplication { data_var appNumber_var } {

    upvar $data_var data $appNumber_var appNumber

    asnGetByte   data byte
    asnGetLength data length

    if {($byte & 0xE0) != 0x060} {
        error "Got different tag than application (0x060)"
    }
    set appNumber [expr {$byte & 0x1F}]
}
