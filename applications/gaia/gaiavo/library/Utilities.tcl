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
package require log
package require tdom

#  Uncomment to see debug messages.
#::log::lvSuppress debug 0

namespace eval ::WS {}

namespace eval ::WS::Utils {
    set typeInfo {}
    set currentSchema {}
    array set importedXref {}
    set nsList {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        s http://www.w3.org/2001/XMLSchema
    }
    array set simpleTypes {
        string 1
        boolean 1
        decimal 1
        float 1
        double 1
        duration 1
        dateTime 1
        time 1
        date 1
        gYearMonth 1
        gYear 1
        gMonthDay 1
        gDay 1
        gMonth 1
        hexBinary 1
        base64Binary 1
        anyURI 1
        QName 1
        NOTATION 1
        normalizedString 1
        token 1
        language 1
        NMTOKEN 1
        NMTOKENS 1
        Name 1
        NCName 1
        ID 1
        IDREF 1
        IDREFS 1
        ENTITY 1
        ENTITIES 1
        integer 1
        nonPositiveInteger 1
        negativeInteger 1
        long 1
        int 1
        short 1
        byte 1
        nonNegativeInteger 1
        unsignedLong 1
        unsignedInt 1
        unsignedShort 1
        unsignedByte 1
        positiveInteger 1
    }
}



###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ServiceTypeDef
#
# Description : Define a type for a service.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this type definition is for
#       type            - The type to be defined/redefined
#       definition      - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is: {type typeName comment commentString}
#                           typeName can be any simple or defined type.
#                           commentString is a quoted string describing the field.
#       xns             - The namespace
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
proc ::WS::Utils::ServiceTypeDef {mode service type definition {xns {}}} {
    variable typeInfo

    if {![string length $xns]} {
        set xns $service
    }
    dict set typeInfo $mode $service $type definition $definition
    dict set typeInfo $mode $service $type xns $xns
    return;
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ServiceSimpleTypeDef
#
# Description : Define a type for a service.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this type definition is for
#       type            - The type to be defined/redefined
#       definition      - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is list of name value:
#                           basetype typeName - any simple or defined type.
#                           comment commentString - a quoted string describing the field.
#                           pattern value
#                           length value
#                           fixed "true"|"false"
#                           maxLength value
#                           minLength value
#                           minInclusive value
#                           maxInclusive value
#                           enumeration value
#
#       xns             - The namespace
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
proc ::WS::Utils::ServiceSimpleTypeDef {mode service type definition {xns {tns1}}} {
    variable simpleTypes

    if {![dict exists $definition xns]} {
        set simpleTypes($mode,$service,$type) [concat $definition xns $xns]
    } else {
        set simpleTypes($mode,$service,$type) $definition
    }
    return;
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name :      ::WS::Utils::GetServiceTypeDef
#
# Description : Query for type definitions.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this query is for
#       type            - The type to be retrieved (optional)
#
# Returns :
#       If type not provided, a dictionary object describing all of the complex types
#       for the service.
#       If type provided, a dictionary object describing the type.
#         A definition consist of a dictionary object with the following key/values:
#           xns         - The namespace for this type.
#           definition  - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is: {type typeName comment commentString}
#                         Where field info is list of name value:
#                           basetype typeName - any simple or defined type.
#                           comment commentString - a quoted string describing the field.
#                           pattern value
#                           length value
#                           fixed "true"|"false"
#                           maxLength value
#                           minLength value
#                           minInclusive value
#                           maxInclusive value
#                           enumeration value
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    The service must be defined.
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
proc ::WS::Utils::GetServiceTypeDef {mode service {type {}}} {
    variable typeInfo
    variable simpleTypes

    if {[string equal $type {}]} {
        set results [dict get $typeInfo $mode $service]
    } else {
        set typeInfoList [TypeInfo $mode $service $type]
        if {[lindex $typeInfoList 0] == 0} {
            if {[info exists simpleTypes($mode,$service,$type)]} {
                set results $simpleTypes($mode,$service,$type)
            } elseif {[info exists simpleTypes($type)]} {
                set results [list type $type]
            } else {
                set results {}
            }
        } else {
            set results [dict get $typeInfo $mode $service $type]
        }
    }

    return $results
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name :      ::WS::Utils::GetServiceSimpleTypeDef
#
# Description : Query for type definitions.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this query is for
#       type            - The type to be retrieved (optional)
#
# Returns :
#       If type not provided, a dictionary object describing all of the simple types
#       for the service.
#       If type provided, a dictionary object describing the type.
#         A definition consist of a dictionary object with the following key/values:
#           xns         - The namespace for this type.
#           definition  - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is: {type typeName comment commentString}
#                         Where field info is list of name value and any restrictions:
#                           basetype typeName - any simple or defined type.
#                           comment commentString - a quoted string describing the field.
#                           pattern value
#                           length value
#                           fixed "true"|"false"
#                           maxLength value
#                           minLength value
#                           minInclusive value
#                           maxInclusive value
#                           enumeration value
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    The service must be defined.
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
proc ::WS::Utils::GetServiceSimpleTypeDef {mode service {type {}}} {
    variable simpleTypes

    if {[string equal $type {}]} {
        set results {}
        foreach {key value} [array get simpleTypes $mode,$service,*] {
            lappend results [list [lindex [split $key {,}] end] $simpleTypes($key)]
        }
    } else {
        if {[info exists simpleTypes($mode,$service,$type)]} {
            set results $simpleTypes($mode,$service,$type)
        } elseif {[info exists simpleTypes($type)]} {
            set results [list type $type]
        } else {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKSMPTYP $type] \
                "Unknown simple type '$type'"
        }
    }

    return $results
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ProcessImportXml
#
# Description : Parse the bindings for a service from a WSDL into our
#               internal representation
#
# Arguments :
#    xml            - The XML string to parse
#    serviceName    - The name service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#    tnsCountVar    - The name of the variable containing the count of the
#                     namespace.
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
proc ::WS::Utils::ProcessImportXml {mode baseUrl xml serviceName serviceInfoVar tnsCountVar} {
    ::log::log debug "Entering ProcessImportXml $mode $baseUrl $xml $serviceName $serviceInfoVar $tnsCountVar"
    upvar $serviceInfoVar serviceInfo
    upvar $tnsCountVar tnsCount
    variable currentSchema

   catch {
    dom parse $xml doc
    $doc selectNodesNamespaces {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        s http://www.w3.org/2001/XMLSchema
    }
    $doc documentElement schema
    set prevSchema $currentSchema
    set currentSchema $schema
    parseScheme $mode $baseUrl $schema $serviceName serviceInfo tnsCount

    set currentSchema $prevSchema
    $doc delete
   } msg
   puts "ProcessImportXml of $baseUrl failed (skipped:$msg)"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::TypeInfo
#
# Description : Return a list indicating if the type is simple or complex
#               and if it is a scalar or an array.
#
# Arguments :
#    type       - the type name, possiblely with a () to specify it is an array
#
# Returns : A list of two elements, as follows:
#               0|1 - 0 means a simple type, 1 means a complex type
#               0|1 - 0 means a scalar, 1 means an array
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
proc ::WS::Utils::TypeInfo {mode service type} {
    variable simpleTypes
    variable typeInfo

    set type [string trim $type]
    if {[string equal [string range $type end-1 end] {()}]} {
        set isArray 1
        set type [string range $type 0 end-2]
    } elseif {[string equal $type {array}]} {
        set isArray 1
    } else {
        set isArray 0
    }
    set isNotSimple [dict exists $typeInfo $mode $service $type]
    return [list $isNotSimple $isArray]
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::CheckAndBuild::ValidateRequest
#
# Description : Given a schema validate a XML string given as parameter
#               using a XML schema description (in WS:: form) for
#               validation
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       xmlString   - The XML string to validate
#       tagName     - The name of the starting tag
#       typeName    - The type for the tag
#
# Returns :     1 if valition ok, 0 if not
#
# Side-Effects :
#       ::errorCode - cleared if validation ok
#                   - contains validation failure information if validation
#                       failed.
#
# Exception Conditions :
#       WS CHECK START_NODE_DIFFERS - Start node not what was expected
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/14/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to handle qualified XML
#
#
###########################################################################
proc ::WS::Utils::Validate {mode serviceName xmlString tagName typeName} {

    dom parse $xmlString resultTree
    $resultTree documentElement currNode
    set nodeName [$currNode localName]
    if {![string equal $nodeName $tagName]} {
        return \
            -code error \
            -errorcode [list WS CHECK START_NODE_DIFFERS [list $tagName $nodeName]] \
            "start node differs expected: $tagName found: $nodeName"
    }
    set ::errorCode {}
    set result [checkTags $mode $serviceName $currNode $typeName]
    $resultTree delete
    return $result
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::BuildRequest
#
# Description : Given a schema check the body of a request handed in
#               as a XML string using a XML schema description (in WS:: form)
#               for validation
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       tagName     - The name of the starting tag
#       typeName    - The type for the tag
#       valueInfos  - The dictionary of the values
#
# Returns :     The body of the request as xml
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/13/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to generate qualified XML
#
###########################################################################
proc ::WS::Utils::BuildRequest {mode serviceName tagName typeName valueInfos} {
    upvar $valueInfos values
    variable resultTree
    variable currNode

    set resultTree [::dom createDocument $tagName]
    set typeInfo [GetServiceTypeDef $mode $serviceName $typeName]
    $resultTree documentElement currNode
    if {[catch {buildTags $mode $serviceName $typeName $valueInfos $resultTree $currNode} msg]} {
        set tmpErrorCode $::errorCode
        set tmpErrorInfo $::errorInfo
        $resultTree delete
        return \
            -code error \
            -errorcode $tmpErrorCode \
            -errorinfo $tmpErrorInfo \
            $msg
    }
    set xml [$resultTree asXML]
    $resultTree delete
    return $xml
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertTypeToDict
#
# Description : Convert the XML, in DOM representation, to a dictionary object for
#               a given type.
#
# Arguments :
#    mode        - The mode, Client or Server
#    serviceName - The service name the type is defined in
#    node        - The base node for the type.
#    type        - The name of the type
#
# Returns : A dictionary object for a given type.
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
proc ::WS::Utils::convertTypeToDict {mode serviceName node type} {
    variable typeInfo

    ::log::log debug [list ::WS::Utils::convertTypeToDict $mode $serviceName $node ([$node nodeName])? $type]
    set typeDefInfo [dict get $typeInfo $mode $serviceName $type]
    ::log::log debug "\t type def = {$typeDefInfo}"
    set xns [dict get $typeDefInfo xns]
    set results {}
    foreach partName [dict keys [dict get $typeDefInfo definition]] {

        set partType [dict get $typeDefInfo definition $partName type]

        if {[string equal $partName *] && [string equal $partType *]} {

            ##
            ## Type infomation being handled dynamically for this part
            ##
            set savedTypeInfo $typeInfo
            parseDynamicType $mode $serviceName $node $type
            set tmp [convertTypeToDict $mode $serviceName $node $type]
            foreach partName [dict keys $tmp] {
                dict set results $partName [dict get $tmp $partName]
            }
            set typeInfo $savedTypeInfo
            continue
        }
        set partXns $xns
        catch {set partXns  [dict get $typeInfo $mode $serviceName $partType xns]}
        set typeInfoList [TypeInfo $mode $serviceName $partType]
        ::log::log debug "\tpartName $partName partType $partType xns $xns typeInfoList $typeInfoList"

        ##
        ## Try for fully qualified name
        ##
        ::log::log debug "Trying #1 [list $node selectNodes $partXns:$partName]"
        if {[catch {llength [set item [$node selectNodes $partXns:$partName]]} len] || ($len == 0)} {
            ::log::log debug "Trying #2 [list $node selectNodes $xns:$partName]"
            if {[catch {llength [set item [$node selectNodes $xns:$partName]]} len] || ($len == 0)} {
                ##
                ## Try for unqualified name
                ##
                ::log::log debug "Trying #3 [list $node selectNodes $partName]"
                if {[catch {llength [set item [$node selectNodes $partName]]} len] || ($len == 0)} {
                    ::log::log debug "Trying #4 -- search of children"
                    set item {}
                    set matchList [list $partXns:$partName  $xns:$partName $partName]
                    foreach childNode [$node childNodes] {
                        set childName [$childNode nodeName]
                        ::log::log debug "\tChecking $childNode $childName agaisnt {$matchList}"
                       if {[lsearch -exact $matchList $childName] != -1 } {
                            set item $childNode
                            break
                        }
                    }
                    if {![string length $item]} {
                        ::log::log debug "\tSkipping"
                        continue
                    }
                }
            }
        }
        switch $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                dict set results $partName [$item asText]
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set tmp {}
                foreach row $item {
                    lappend tmp [$row asText]
                }
                dict set results $partName $tmp
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                dict set results $partName [convertTypeToDict $mode $serviceName $item $partType]
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set partType [string trim $partType {()}]
                set tmp [list]
                foreach row $item {
                    lappend tmp [convertTypeToDict $mode $serviceName $row $partType]
                }
                dict set results $partName $tmp
            }
        }
    }
    ::log::log debug [list Leaving ::WS::Utils::convertTypeToDict with $results]
    return $results
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertDictToType
#
# Description : Convert a dictionary object into a XML DOM tree.
#
# Arguments :
#    mode        - The mode, Client or Server
#    service     - The service name the type is defined in
#    parent      - The parent node of the type.
#    dict        - The dictionary to convert
#    type        - The name of the type
#
# Returns : None
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
proc ::WS::Utils::convertDictToType {mode service doc parent dict type} {
    ::log::log debug "Entering ::WS::Utils::convertDictToType $mode $service $doc $parent {$dict} $type"
    variable typeInfo
    variable simpleTypes

    set typeInfoList [TypeInfo $mode $service $type]
    if {[lindex $typeInfoList 0]} {
        set itemList [dict get $typeInfo $mode $service $type definition]
        set xns [dict get $typeInfo $mode $service $type xns]
    } else {
        set xns $simpleTypes($mode,$service,$type)
        set itemList [list $type {type string}]
    }
    ::log::log debug "\titemList is {$itemList}"
    foreach {itemName itemDef} $itemList {
        set itemType [dict get $itemDef type]
        ::log::log debug "\t\titemName = {$itemName} itemDef = {$itemDef}"
        set typeInfoList [TypeInfo $mode $service $itemType]
        if {![dict exists $dict $itemName]} {
            continue
        }
        ::log::log debug "\t\titemName = {$itemName} itemDef = {$itemDef} typeInfoList = {$typeInfoList}"
        switch $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                $parent appendChild [$doc createElement $xns:$itemName retNode]
                set resultValue [dict get $dict $itemName]
                $retNode appendChild [$doc createTextNode $resultValue]
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set dataList [dict get $dict $itemName]
                foreach resultValue $dataList {
                    $parent appendChild [$doc createElement $xns:$itemName retNode]
                    $retNode appendChild [$doc createTextNode $resultValue]
                }
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                #$parent appendChild [$doc createElement $xns:$itemName retnode]
                #convertDictToType $mode $service $doc $retnode [dict get $dict $itemName] $itemType

                ## Assume XML.
                $parent appendChild [$doc createElement $xns:$itemName retNode]
                set resultValue [dict get $dict $itemName]
                $retNode appendXML $resultValue
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set dataList [dict get $dict $itemName]
                set tmpType [string trim $itemType ()]
                foreach item $dataList {
                    $parent appendChild [$doc createElement $xns:$itemName retnode]
                    convertDictToType $mode $service $doc $retnode $item $tmpType
                }
            }
        }
    }
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertDictToEncodedType
#
# Description : Convert a dictionary object into a XML DOM tree with type
#               enconding.
#
# Arguments :
#    mode        - The mode, Client or Server
#    service     - The service name the type is defined in
#    parent      - The parent node of the type.
#    dict        - The dictionary to convert
#    type        - The name of the type
#
# Returns : None
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
proc ::WS::Utils::convertDictToEncodedType {mode service doc parent dict type} {
    ::log::log debug "Entering ::WS::Utils::convertDictToEncodedType $mode $service $doc $parent {$dict} $type"
    variable typeInfo

    set itemList [dict get $typeInfo $mode $service $type definition]
    set xns [dict get $typeInfo $mode $service $type xns]
    ::log::log debug "\titemList is {$itemList}"
    foreach {itemName itemDef} $itemList {
        set itemType [dict get $itemList $itemName type]
        set typeInfoList [TypeInfo $mode $service $itemType]
        if {![dict exists $dict $itemName]} {
            continue
        }
        switch $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                $parent appendChild [$doc createElement $xns:$itemName retNode]
                $retNode setAttribute xsi:type xs:$itemType
                set resultValue [dict get $dict $itemName]
                $retNode appendChild [$doc createTextNode $resultValue]
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set dataList [dict get $dict $itemName]
                set tmpType [string trim $itemType {()}]
                foreach resultValue $dataList {
                    $parent appendChild [$doc createElement $xns:$itemName retNode]
                    $retNode setAttribute xsi:type xs:$itemType
                    set resultValue [dict get $dict $itemName]
                    $retNode appendChild [$doc createTextNode $resultValue]
                }
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                $parent appendChild [$doc createElement $xns:$itemName retNode]
                $retNode setAttribute xsi:type xs:$itemType
                                 convertDictToEncodedType $mode $service $doc $retNode [dict get $dict $itemName] $itemType
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set dataList [dict get $dict $itemName]
                set tmpType [string trim $itemType ()]
                foreach item $dataList {
                    $parent appendChild [$doc createElement $xns:$itemName retNode]
                    $retNode setAttribute xsi:type xs:$itemType
                    convertDictToEncodedType $mode $service $doc $retNode $item $tmpType
                }
            }
        }
    }
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseDynamicType
#
# Description : Parse the Xschme for a dynamically typed part.
#
# Arguments :
#    mode        - The mode, Client or Server
#    serviceName - The service name the type is defined in
#    node        - The base node for the type.
#    type        - The name of the type
#
# Returns : A dictionary object for a given type.
#
# Side-Effects : Type deginitions added
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
proc ::WS::Utils::parseDynamicType {mode serviceName node type} {
    variable typeInfo
    variable nsList

    ::log::log debug [list ::WS::Utils::parseDynamicType $mode $serviceName $node $type]

    foreach child [$node childNodes] {
        ::log::log debug "\t Child $child is [$child nodeName]"
    }

    ##
    ## Get type being defined
    ##
    set schemeNode [$node selectNodes -namespaces $nsList s:schema]
    set newTypeNode [$node selectNodes -namespaces $nsList  s:schema/s:element]
    set newTypeName [lindex [split [$newTypeNode getAttribute name] :] end]

    ##
    ## Get sibling node to scheme and add tempory type definitions
    ##
    ## type == sibing of temp type
    ## temp_type == newType of newType
    ##
    set tnsCountVar [llength [dict get $::WS::Client::serviceArr($serviceName) targetNamespace]]
    set tns tnx$tnsCountVar
    set dataNode {}
    $schemeNode nextSibling dataNode
    if {![info exists dataNode] || ![string length $dataNode]} {
        $schemeNode previousSibling dataNode
    }
    set dataNodeNameList [split [$dataNode nodeName] :]
    set dataTnsName [lindex $dataNodeNameList 0]
    set dataNodeName [lindex $dataNodeNameList end]
    set tempTypeName 1_temp_type
    dict set typeInfo $mode $serviceName $tempTypeName [list  xns $tns definition [list $newTypeName [list type $newTypeName comment {}]]]
    dict set typeInfo $mode $serviceName $type [list xns $dataTnsName definition [list $dataNodeName [list type $tempTypeName comment {}]]]

    ##
    ## Parse the Scheme --gwl
    ##
    parseScheme $mode {} $schemeNode $serviceName typeInfo tnsCountVar

    ##
    ## All done
    ##
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseScheme
#
# Description : Parse the types for a service from a Schema into
#               our internal representation
#
# Arguments :
#    SchemaNode       - The top node of the Schema
#    serviceNode    - The DOM node for the service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : Nothing
#
# Side-Effects : Defines mode types for the service as specified by the Schema
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
proc ::WS::Utils::parseScheme {mode baseUrl schemaNode serviceName serviceInfoVar tnsCountVar} {
    ::log::log debug "Entering! parseScheme $mode $baseUrl $schemaNode $serviceName $serviceInfoVar $tnsCountVar"

    upvar $tnsCountVar tnsCount
    upvar $serviceInfoVar serviceInfo
    variable currentSchema
    variable nsList

    set currentSchema $schemaNode
    if {[$schemaNode hasAttribute targetNamespace]} {
        set xns [$schemaNode getAttribute targetNamespace]
    } else {
        set xns $baseUrl
    }
    set tns [format {tns%d} [incr tnsCount]]
    dict lappend serviceInfo targetNamespace [list $tns $xns]

    ##
    ## Process Imports
    ##
    foreach element [$schemaNode selectNodes -namespaces $nsList s:import] {
        ::log::log debug "\tprocessing $element"
        processImport $mode $baseUrl $element $serviceName serviceInfo tnsCount
    }

    ::log::log debug  "Parsing Element types"
    foreach element [$schemaNode selectNodes -namespaces $nsList s:element] {
        ::log::log debug "\tprocessing $element"
        parseElementalType $mode serviceInfo $serviceName $element $tns
    }

    ::log::log debug "Parsing Simple types"
    foreach element [$schemaNode selectNodes -namespaces $nsList s:simpleType] {
        ::log::log debug "\tprocessing $element"
        parseSimpleType $mode serviceInfo $serviceName $element $tns
    }

    ::log::log debug  "Parsing Complex types"
    foreach element [$schemaNode selectNodes -namespaces $nsList s:complexType] {
        ::log::log debug "\tprocessing $element"
        parseComplexType $mode serviceInfo $serviceName $element $tns
    }
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::processImport
#
# Description : Parse the bindings for a service from a Schema into our
#               internal representation
#
# Arguments :
#    baseUrl        - The url of the importing node
#    importNode     - The node to import
#    serviceName    - The name service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#    tnsCountVar    - The name of the variable containing the count of the
#                     namespace.
#
# Returns : Nothing
#
# Side-Effects : Defines mode types for the service as specified by the Schema
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
proc ::WS::Utils::processImport {mode baseUrl importNode serviceName serviceInfoVar tnsCountVar} {
    upvar $serviceInfoVar serviceInfo
    upvar $tnsCountVar tnsCount
    variable currentSchema
    variable importedXref

    ::log::log debug "Entering [info level 0]"
    ##
    ## Get the xml
    ##
    set attrName schemaLocation
    if {![$importNode hasAttribute $attrName]} {
        set attrName namespace
        if {![$importNode hasAttribute $attrName]} {
            ::log::log debug "\t No schema location, existing"
            return \
                -code error \
                -errorcode [list WS CLIENT MISSCHLOC $baseUrl] \
                "Missing Schema Location in '$baseUrl'"
        }
    }
    set url [::uri::resolve $baseUrl  [$importNode getAttribute $attrName]]
    ::log::log debug "\t Importing {$url}"
    set importedXref($mode,$serviceName,$url) [list $mode $serviceName $tnsCount]
    switch [dict get [::uri::split $url] scheme] {
        file {
            upvar #0 [::uri::geturl $url] token
            set xml $token(data)
            unset token
        }
        http {

            #  Don't process any external schemas, not that important.
            return

            set token [::http::geturl $url]
            ::http::wait $token
            set ncode [::http::ncode $token]
            set xml [::http::data $token]
            ::http::cleanup $token
            if {$ncode != 200} {

               #  Try a simple name transformation (xxx/ -> xxx.xsd)
               if { [string match {*/} $url] } {
                  set url [string replace $url end end ".xsd"]
               }
               set token [::http::geturl $url]
               ::http::wait $token
               set ncode [::http::ncode $token]
               set xml [::http::data $token]
               ::http::cleanup $token
               if {$ncode != 200} {
                  puts "failed to load: $url (skipped)"
                  return
               }
            }
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKURLTYP $url] \
                "Unknown URL type '$url'"
        }
    }
    ProcessImportXml $mode $baseUrl $xml $serviceName $serviceInfoVar $tnsCountVar
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseComplexType
#
# Description : Parse a complex type declaration from the Schema into our
#               internal representation
#
# Arguments :
#    dcitVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::parseComplexType {mode dictVar serviceName node tns} {
    upvar $dictVar results
    variable currentSchema
    variable nsList

    ::log::log debug "Entering [info level 0]"

    set typeName [$node getAttribute name]
    set partList {}
    set nodeFound 0
    array set attrArr {}
    foreach middleNode [$node childNodes] {
        set middle [$middleNode localName]
        ::log::log debug "Complex Type is $typeName, middle is $middle"
        #puts "Complex Type is $typeName, middle is $middle"
        switch $middle {
            element -
            attribute {
                set nodeFound 1
                set partName [$middleNode getAttribute name]
                set partType [lindex [split [$middleNode getAttribute type string:string] {:}] end]
                set partMax [$middleNode getAttribute maxOccurs 1]
                if {[string equal $partMax 1]} {
                    lappend partList $partName [list type $partType comment {}]
                } else {
                    lappend partList $partName [list type [string trim ${partType} {()}]() comment {}]
                }
            }
            extension {
                set baseName [lindex [split [$middleNode getAttribute base] {:}] end]
                set tmp [partList $mode $middleNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set nodeFound 1
                    set partList [concat $partList $tmp]
                }
            }
            choice -
            sequence -
            all {
                set elementList [$middleNode selectNodes -namespaces $nsList s:element]
                set partMax [$middleNode getAttribute maxOccurs 1]
                set tmp [partList $mode $middleNode $serviceName results $tns $partMax]
                if {[llength $tmp]} {
                    ::log::log debug "\tadding {$tmp} to partslist"
                    set nodeFound 1
                    set partList [concat $partList $tmp]
                } else {
                    ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $typeName [list base string comment {}] $tns
                    return
                }
            }
            complexType {
                $middleNode setAttribute name $typeName
                parseComplexType $mode results $serviceName $middleNode $tns
            }
            complexContent {
                set contentType [[$middleNode childNodes] localName]
                switch $contentType {
                    restriction {
                        set nodeFound 1
                        set restriction [$middleNode selectNodes -namespaces $nsList s:restriction]
                        catch {
                            set element [$middleNode selectNodes -namespaces $nsList s:restriction/s:attribute]
                            set typeInfoList [list baseType [$restriction getAttribute base]]
                            array unset attrArr
                            foreach attr [$element attributes] {
                                if {[llength $attr] > 1} {
                                    set name [lindex $attr 0]
                                    set ref [lindex $attr 1]:[lindex $attr 0]
                                } else {
                                    set name $attr
                                    set ref $attr
                                }
                                catch {set attrArr($name) [$element getAttribute $ref]}
                            }
                            set partName item
                            set partType [lindex [split $attrArr(arrayType) {:}] end]
                            set partType [string map {{[]} {()}} $partType]
                            lappend partList $partName [list type [string trim ${partType} {()}]() comment {}]
                            set nodeFound 1
                        }
                    }
                    extension {
                        set tmp [partList $mode $middleNode $serviceName results $tns]
                        if {[llength $tmp]} {
                        set nodeFound 1
                            set partList [concat $partList $tmp]
                        }
                    }
                }
            }
            simpleContent {
                set tmp [partList $mode $middleNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set nodeFound 1
                    set partList [concat $partList $tmp]
                }
            }
            restriction {
                parseSimpleType $mode results $serviceName $node $tns
                return
            }
            defualt {
                parseElementalType $mode results $serviceName $node $tns
                return
            }
        }
    }
    if {[llength $partList]} {
        dict set results types $typeName $partList
        ::WS::Utils::ServiceTypeDef $mode $serviceName $typeName $partList $tns
    } elseif {!$nodeFound} {
        #puts "Defined $typeName as simple type"
        ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $typeName [list base string comment {}] $tns
    } else {
        set xml [string trim [$node asXML]]
        return \
            -code error \
            -errorcode [list WS $mode BADCPXTYPDEF [list $typeName $xml]] \
            "Bad complex type definition for '$typeName' :: '$xml'"
    }
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::partList
#
# Description : Prase the list of parts of a type definition from the Schema into our
#               internal representation
#
# Arguments :
#    dcitVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::partList {mode node serviceName dictVar tns {occurs {}}} {
    variable currentSchema
    variable nsList
    upvar $dictVar results

    set partList {}
    set middle [$node localName]
    ::log::log debug "Entering [info level 0] -- for $middle"
    switch $middle {
        element -
        attribute {
            catch {
                set partName [$node getAttribute name]
                set partType [lindex [split [$node getAttribute type string:string] {:}] end]
                set partMax [$node getAttribute maxOccurs 1]
                if {[string equal $partMax 1]} {
                    set partList [list $partName [list type $partType comment {}]]
                } else {
                    set partList [list $partName [list type [string trim ${partType} {()}]() comment {}]]
                }
            }
        }
        extension {
            set baseName [lindex [split [$node getAttribute base] {:}] end]
            #puts "base name $baseName"
            if {[lindex [::WS::Utils::TypeInfo Client $serviceName $baseName] 0]} {
                if {[catch {::WS::Utils::GetServiceTypeDef Client $serviceName $baseName}]} {
                    set baseQuery [format {child::*[attribute::name='%s']} $baseName]
                    set baseNode [$currentSchema selectNodes $baseQuery]
                    #puts "$baseQuery gave {$baseNode}"
                    set baseNodeType [$baseNode localName]
                    switch $baseNodeType {
                        complexType {
                            parseComplexType $mode serviceInfo $serviceName $baseNode $tns
                        }
                        element {
                            parseElementalType $mode serviceInfo $serviceName $baseNode $tns
                        }
                        simpleType {
                            parseSimpleType $mode serviceInfo $serviceName $baseNode $tns
                        }
                    }
                }
                set baseInfo [::WS::Utils::GetServiceTypeDef $mode $serviceName $baseName]
                catch {set partList [concat $partList [dict get $baseInfo definition]]}
            }
            foreach elementNode [$node childNodes] {
                set tmp [partList $mode $elementNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set partList [concat $partList $tmp]
                }
            }
        }
        choice -
        sequence -
        all {
            set elementList [$node selectNodes -namespaces $nsList s:element]
            set elementsFound 0
            ::log::log debug "\telement list is {$elementList}"
            foreach element $elementList {
                ::log::log debug "\t\tprocessing $element ([$element nodeName])"
                if {[catch {
                    set elementsFound 1
                    set attrName name
                    set isRef 0
                    if {![$element hasAttribute name]} {
                        set attrName ref
                        set isRef 1
                    }
                    set partName [$element getAttribute $attrName]
                    if {$isRef} {
                        set partType [dict get [::WS::Utils::GetServiceTypeDef $mode $serviceName $partName] definition $partName type]
                    } else {
                        ##
                        ## See if really a complex definition
                        ##
                        if {[$element hasChildNodes]} {
                            set partType $partName
                            parseComplexType $mode results $serviceName $element $tns
                        } else {
                            set partType [lindex [split [$element getAttribute type string:string] {:}] end]
                        }
                    }
                    if {[string length $occurs]} {
                        set partMax $occurs
                    } else {
                        set partMax [$element getAttribute maxOccurs 1]
                    }
                    if {[string equal $partMax 1]} {
                        lappend partList $partName [list type $partType comment {}]
                    } else {
                        lappend partList $partName [list type [string trim ${partType} {()}]() comment {}]
                    }
                } msg]} {
                        ::log::log error "\tError processing {$msg}"
                }
            }
            if {!$elementsFound} {
                return
            }
        }
        complexContent {
            set contentType [[$node childNodes] localName]
            switch $contentType {
                restriction {
                    set restriction [$node selectNodes -namespaces $nsList s:restriction]
                    set element [$node selectNodes -namespaces $nsList s:restriction/s:attribute]
                    set typeInfoList [list baseType [$restriction getAttribute base]]
                    array unset attrArr
                    foreach attr [$element attributes] {
                        if {[llength $attr] > 1} {
                            set name [lindex $attr 0]
                            set ref [lindex $attr 1]:[lindex $attr 0]
                        } else {
                            set name $attr
                            set ref $attr
                        }
                        catch {set attrArr($name) [$element getAttribute $ref]}
                    }
                    set partName item
                    set partType [lindex [split $attrArr(arrayType) {:}] end]
                    set partType [string map {{[]} {()}} $partType]
                    set partList [list $partName [list type [string trim ${partType} {()}]() comment {}]]
                }
                extension {
                    set extension [$node selectNodes -namespaces $nsList s:extension]
                    set partList [partList $mode $extension $serviceName results $tns]
                }
            }
        }
        simpleContent {
            foreach elementNode [$node childNodes] {
                set tmp [partList $mode $elementNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set partList [concat $partList $tmp]
                }
            }
        }
        restriction {
            parseSimpleType $mode results $serviceName $node $tns
            return
        }
        default {
            parseElementalType $mode results $serviceName $node $tns
            return
        }
    }
    return $partList
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseElementalType
#
# Description : Parse an elemental type declaration from the Schema into our
#               internal representation
#
# Arguments :
#    dcitVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::parseElementalType {mode dictVar serviceName node tns} {

    upvar $dictVar results
    variable importedXref
    variable nsList

    ::log::log debug "Entering [info level 0]"

    set attributeName name
    if {![$node hasAttribute $attributeName]} {
        set attributeName ref
    }
    set typeName [$node getAttribute $attributeName]
    set typeType ""
    if {[$node hasAttribute type]} {
            set typeType [$node getAttribute type]
    }
    ::log::log debug "Elemental Type is $typeName"
    set partList {}
    set elements [$node selectNodes -namespaces $nsList s:complexType/s:sequence/s:element]
    ::log::log debug "\t element list is {$elements}"
    foreach element $elements {
        ::log::log debug "\t\t Processing element {[$element nodeName]}"
        set elementsFound 1
        set typeAttribute ""
        if {[$element hasAttribute ref]} {
            ::log::log debug "\t\t has a ref of {[$element getAttribute ref]}"
            set refTypeInfo [split [$element getAttribute ref] {:}]
            set refNS [lindex $refTypeInfo 0]
            if {[string equal $refNS {}]} {
                set refType [lindex $refTypeInfo 1]
                set namespaceList [$element selectNodes namespace::*]
                set index [lsearch -glob $namespaceList "xmlns:$refNS *"]
                set url [lindex $namespaceList $index 1]
                ::log::log debug "\t\t reference is {$refNS} {$refType} {$url}"
                if {![info exists importedXref($mode,$serviceName,$url)]} {
                    return \
                        -code error \
                        -errorcode [list WS CLIENT NOTIMP $url] \
                        "Schema not imported: {$url}'"
                }
                set partName $refType
                set partType $refType
            } elseif {[string equal -nocase [lindex $refTypeInfo 1] schema]} {
                set partName *
                set partType *
            } else {
                set partName $refTypeInfo
                set partType $refTypeInfo
            }
        } else {
            ::log::log debug "\t\t has no ref has {[$element attributes]}"
            set childList [$element selectNodes -namespaces $nsList s:complexType/s:sequence/s:element]
            if {[llength $childList]} {
                ##
                ## Element defines another element layer
                ##
                set partName [$element getAttribute name]
                set partType $partName
                parseElementalType $mode results $serviceName $element $tns
            } else {
                set partName [$element getAttribute name]
                set partType [lindex [split [$element getAttribute type string:string] {:}] end]
            }
        }
        set partMax [$element getAttribute maxOccurs 1]
        ::log::log debug "\t\t part is {$partName} {$partType} {$partMax}"

        if {[string equal $partMax 1]} {
            lappend partList $partName [list type $partType comment {}]
        } else {
            lappend partList $partName [list type [string trim ${partType} {()}]() comment {}]
        }
    }
    if {[llength $elements] == 0} {
        #
        # Validate this is not really a complex type
        #
        if {[$node hasChildNodes]} {
            set childNode [lindex [$node childNodes] 0]
            $childNode setAttribute name $typeName
            parseComplexType $mode results $serviceName $childNode $tns
            return
        }
        # have an element with a type only, so do the work here
        set partType [lindex [split [$node getAttribute type string:string] {:}] end]
        set partMax [$node getAttribute maxOccurs 1]
        if {[string equal $partMax 1]} {
            ##
            ## See if this is just a restriction on a simple type
            ##
            if {([lindex [::WS::Utils::TypeInfo $mode $serviceName $partType] 0] == 0) &&
                [string equal $typeName $partType]} {
                return
            } else {
                lappend partList $typeName [list type $partType comment {}]
            }
        } else {
            lappend partList $typeName [list type [string trim ${partType} {()}]() comment {}]
        }
    }
    if {[llength $partList]} {
        dict set results types $typeName $partList
        ::WS::Utils::ServiceTypeDef $mode $serviceName $typeName $partList $tns
    } else {
        if {![dict exists $results types $typeName]} {
            set partList [list base string comment {} xns $tns]
            ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $typeName $partList
            dict set results simpletypes $typeName $partList
        }
    }
     ::log::log debug "\t returning"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseSimpleType
#
# Description : Parse a simnple type declaration from the Schema into our
#               internal representation
#
# Arguments :
#    dcitVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::parseSimpleType {mode dictVar serviceName node tns} {
    upvar $dictVar results
    variable nsList

    ::log::log debug "Entering [info level 0]"

    set typeName [$node getAttribute name]
    ::log::log debug "Simple Type is $typeName"
    #puts "Simple Type is $typeName"
    set restrictionNode [$node selectNodes -namespaces $nsList s:restriction]
    if {[string equal $restrictionNode {}]} {
        set restrictionNode [$node selectNodes -namespaces $nsList s:list/s:simpleType/s:restriction]
    }
    if {[string equal $restrictionNode {}]} {
        set xml [string trim [$node asXML]]
        return \
            -code error \
            -errorcode [list WS $mode BADSMPTYPDEF [list $typeName $xml]] \
            "Bad simple type definition for '$typeName' :: \n'$xml'"
    }
    set baseType [lindex [split [$restrictionNode getAttribute base] {:}] end]
    set partList [list baseType $baseType xns $tns]
    set enumList {}
    foreach item [$restrictionNode childNodes] {
        set itemName [$item localName]
        set value [$item getAttribute value]
        #puts "\t Item {$itemName} = {$value}"
        if {[string equal $itemName {enumeration}]} {
            lappend enumList $value
        } else {
            lappend partList $itemName $value
        }
        if {[$item hasAttribute fixed]} {
            lappend partList fixed [$item getAttribute fixed]
        }
    }
    if {[llength $enumList]} {
        lappend partList enumeration $enumList
    }
    if {![dict exists $results types $typeName]} {
        ServiceSimpleTypeDef $mode $serviceName $typeName $partList
        dict set results simpletypes $typeName $partList
    }
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::checkTags
#
# Description : Recursivly check the tags and values inside the tags
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       currNode    - The node to process
#       typeName    - The type name of the node
#
# Returns :     1 if ok, 0 otherwise
#
# Side-Effects :
#       ::errorCode - contains validation failure information if validation
#                       failed.
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/13/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to handle qualified XML
#
###########################################################################
proc ::WS::Utils::checkTags {mode serviceName currNode typeName} {

    ##
    ## Assume success
    ##
    set result 1

    ##
    ## Get the type information
    ##
    set typeInfoList [TypeInfo $mode $serviceName $typeName]
    set baseTypeName [string trimright $typeName {()}]
    set typeInfo [GetServiceTypeDef $mode $serviceName $baseTypeName]
    set isComplex [lindex $typeInfoList 0]
    set isArray [lindex $typeInfoList 1]

    if {$isComplex} {
        ##
        ## Is complex
        ##
        array set fieldInfoArr {}
        ##
        ## Build array of what is present
        ##
        foreach node [$currNode childNodes] {
            set localName [$node localName]
            lappend fieldInfoArr($localName) $node
        }
        ##
        ## Walk through each field and validate the information
        ##
        foreach {field fieldDef} [dict get $typeInfo definition] {
            array unset fieldInfoArr
            set fieldInfoArr(minOccurs) 0
            array set fieldInfoArr $fieldDef
            if {$fieldInfoArr(minOccurs) && ![info exists fieldInfoArr($field)]} {
                ##
                ## Fields was required but is missing
                ##
                set ::errorCode [list WS CHECK MISSREQFLD [list $type $field]]
                set result 0
            } elseif {$fieldInfoArr(minOccurs) &&
                      ($fieldInfoArr(minOccurs) > [llength $fieldInfoArr($field)])} {
                ##
                ## Fields was required and present, but not enough times
                ##
                set ::errorCode [list WS CHECK MINOCCUR [list $type $field]]
                set result 0
            } elseif {[info exists fieldInfoArr(maxOccurs)] &&
                      [string is integer fieldInfoArr(maxOccurs)] &&
                      ($fieldInfoArr(maxOccurs) < [llength $fieldInfoArr($field)])} {
                ##
                ## Fields was required and present, but too many times
                ##
                set ::errorCode [list WS CHECK MAXOCCUR [list $type $field]]
                set result 0
            } elseif {[info exists fieldInfoArr($field)]} {
                foreach node $fieldInfoArr($field) {
                    set result [checkTags $mode $serviceName $node $fieldInfoArr(type)]
                    if {!$result} {
                        break
                    }
                }
            }
            if {!$result} {
                break
            }
        }
    } else {
        ##
        ## Get the value
        ##
        set value [$currNode asText]
        set result [checkValue $mode $serviceName $baseTypeName $value]
    }

    return $result
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::checkValue
#
# Description : Check a Value between tags of a XML document against the
#               type in the XML schema description
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The name of the service
#       type        - The type to check
#       value       - The value to check
#
# Returns :     1 if ok or 0 if checking not ok
#
# Side-Effects :
#       ::errorCode - contains validation failure information if validation
#                       failed.
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/14/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to handle qualified XML
#
###########################################################################
proc ::WS::Utils::checkValue {mode serviceName type value} {

    set result 0
    array set typeInfos {
        minLength 0
        maxLength -1
        fixed false
    }
    array set typeInfos [GetServiceTypeDef $mode $serviceName $type]
    foreach {var value} [array get typeInfos] {
        set $var $value
    }
    set result 1

    if {$minLength >= 0 && [string length $value] < $minLength} {
        set ::errorCode [list WS CHECK VALUE_TO_SHORT [list $key $value $minLength $typeInfo]]
        set result 0
    } elseif {$maxLength >= 0 && [string length $value] > $maxLength} {
        set ::errorCode [list WS CHECK VALUE_TO_LONG [list $key $value $maxLength $typeInfo]]
        set result 0
    } elseif {[info exists enumeration] && ([lsearch -exact $enumeration $value] == -1)} {
        set errorCode [list WS CHECK VALUE_NOT_IN_ENUMERATION [list $key $value $enumerationVals $typeInfo]]
        set result 0
    } elseif {[info exists pattern] && (![regexp $pattern $value])} {
        set errorCode [list WS CHECK VALUE_NOT_MATCHES_PATTERN [list $key $value $pattern $typeInfo]]
        set result 0
    }

    return $result
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::buildTags
#
# Description : Recursivly build the tags by checking the values to put
#               inside the tags and append to the dom tree resultTree
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       typeName    - The type for the tag
#       valueInfos  - The dictionary of the values
#       doc         - The DOM Document
#       currentNode - Node to append values to
#
# Returns :     nothing
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/13/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to generate qualified XML
#
###########################################################################
proc ::WS::Utils::buildTags {mode serviceName typeName valueInfos doc currentNode} {
    upvar $valueInfos values

    ##
    ## Get the type information
    ##
    set baseTypeName [string trimright $typeName {()}]
    set typeInfo [GetServiceTypeDef $mode $serviceName $baseTypeName]
    set xns [dict get $typeInfo $mode $service $type xns]

    foreach {field fieldDef} [dict get $typeInfo definition] {
        ##
        ## Get info about this field and its type
        ##
        array unset fieldInfoArr
        set fieldInfoArr(minOccurs) 0
        array set fieldInfoArr $fieldDef
        set typeInfoList [TypeInfo $mode $serviceName $fieldInfoArr(type)]
        set fieldBaseType [string trimright $fieldInfoArr(type) {()}]
        set isComplex [lindex $typeInfoList 0]
        set isArray [lindex $typeInfoList 1]
        if {[dict exists $valueInfos $field]} {
            if {$isArray} {
                set valueList [dict get $valueInfos $field]
            } else {
                set valueList [list [dict get $valueInfos $field]]
            }
            set valueListLenght [llength $valueList]
        } else {
            set valueListLenght -1
        }

        if {$fieldInfoArr(minOccurs) && ![dict exists $valueInfos $field]} {
            ##
            ## Fields was required but is missing
            ##
            return \
                -errorcode [list WS CHECK MISSREQFLD [list $type $field]] \
                "Field '$field' of type '$typeName' was required but is missing"
        } elseif {$fieldInfoArr(minOccurs) &&
                  ($fieldInfoArr(minOccurs) > $valueListLenght)} {
            ##
            ## Fields was required and present, but not enough times
            ##
            set minOccurs $fieldInfoArr(minOccurs)
            return \
                -errorcode [list WS CHECK MINOCCUR [list $type $field $minOccurs $valueListLenght]] \
                "Field '$field' of type '$typeName' was required to occur $minOccurs time(s) but only occured $valueListLenght time(s)"
        } elseif {[info exists fieldInfoArr(maxOccurs)] &&
                  [string is integer fieldInfoArr(maxOccurs)] &&
                  ($fieldInfoArr(maxOccurs) < $valueListLenght)} {
            ##
            ## Fields was required and present, but too many times
            ##
            set minOccurs $fieldInfoArr(maxOccurs)
            return \
                -errorcode [list WS CHECK MAXOCCUR [list $type $field]] \
                "Field '$field' of type '$typeName' could only occur $minOccurs time(s) but occured $valueListLenght time(s)"
        } elseif {[dict exists $valueInfos $field]} {
            foreach value $valueList {
                $currentNode appendChild [$doc createElement $xns:$field retNode]
                if {$isComplex} {
                    buildTags $mode $serviceName $fieldBaseType $value $doc $retNode
                } else {
                    if {[info exists fieldInfoArr(enumeration)] &&
                        [info exists fieldInfoArr(fixed)] && $fieldInfoArr(fixed)} {
                        set value [lindex $fieldInfoArr(enumeration) 0]
                    }
                    if {[checkValue $mode $serviceName $fieldBaseType $value]} {
                        $retNode appendChild [$doc createTextNode $value]
                    } else {
                        set msg "Field '$field' of type '$typeName' "
                        switch -exact [lindex $::errorCode 2] {
                            VALUE_TO_SHORT {
                                append msg "value required to be $fieldInfoArr(minLength) long but is only [string length $value] long"
                            }
                            VALUE_TO_LONG {
                                append msg "value allowed to be only $fieldInfoArr(minLength) long but is [string length $value] long"
                            }
                            VALUE_NOT_IN_ENUMERATION {
                                append msg "value '$value' not in ([join $fieldInfoArr(enumeration) {, }])"
                            }
                            VALUE_NOT_MATCHES_PATTERN {
                                append msg "value '$value' does not match pattern: $fieldInfoArr(pattern)"
                            }
                        }
                        return \
                            -errorcode $::errorCode \
                            $msg
                    }
                }
            }
        }
    }
}
