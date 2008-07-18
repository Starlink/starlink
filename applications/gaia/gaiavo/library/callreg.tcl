lappend auto_path [pwd]

package require WS::Client
package require dict
package require Tclx

::http::config -proxyhost wwwcache.dur.ac.uk
::http::config -proxyport 8080

set registry "http://nvo.stsci.edu/vor10/NVORegInt.asmx?WSDL"
set endmethod "VOTCapabilityPredicate"

##
## Get Definition of the offered services
##
set def [::WS::Client::GetAndParseWsdl $registry {} VORegistry]

## Set query dictionary.
set inputs [list "predicate" "title like '%infra%'" "capability" "SimpleImageAccess"]

##
## RAW, using the dict access methods doesn't work (doesn't respect XML
## content, returns text).
##
puts stdout "Calling VOTCapabilityPredicate!"
set results [::WS::Client::DoRawCall VORegistry $endmethod $inputs]

::dom parse -keepEmpties $results doc
$doc documentElement top
set xns {
   ENV "http://schemas.xmlsoap.org/soap/envelope/"
   xsi "http://www.w3.org/2001/XMLSchema-instance"
   xs "http://www.w3.org/2001/XMLSchema"
}
$doc selectNodesNamespaces $xns
set body [$top selectNodes ENV:Body]

## Check body for the VOTABLE result.

#set predicate_result [$body getElementsByTagName VOTCapabilityPredicateResult]

set resource [$body getElementsByTagName RESOURCE]
if { $resource != {} } {
   set fid [::open "callreg.vot" "w"]
   puts $fid {<VOTABLE version="1.1"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://www.ivoa.net/xml/VOTable/v1.1 http://www.ivoa.net/xml/VOTable/v1.1"
 xmlns="http://www.ivoa.net/xml/VOTable/v1.1">
<!--
 !  VOTable written by GAIA
 !-->
   }
   $resource asXML -channel $fid
   puts $fid {</VOTABLE>}
}

exit
