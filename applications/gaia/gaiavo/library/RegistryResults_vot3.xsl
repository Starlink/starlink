<?xml version="1.0" encoding="UTF-8"?>

<!-- Stylesheet to transform a registry query into a VOTable.
  -  Each capability becomes a row in the table associated with some of the
  -  metadata of the main resource.
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:vs="http://www.ivoa.net/xml/VODataService/v1.0"
                xmlns:stc="http://www.ivoa.net/xml/STC/stc-v1.30.xsd"
                xmlns:vr="http://www.ivoa.net/xml/VOResource/v1.0"
                xmlns:ri="http://www.ivoa.net/xml/RegistryInterface/v1.0"
                xmlns:vot="http://www.ivoa.net/xml/VOTable/v1.1"
                xmlns="http://www.ivoa.net/xml/VOTable/v1.1"
                version="1.0">

  <!-- Throw any unmatched text away, do not propagate -->
  <xsl:template match="text()|@*" />

  <!-- Start with the VOResources -->
  <xsl:template match="ri:VOResources" xml:space="preserve">

<VOTABLE xmlns="http://www.ivoa.net/xml/VOTable/v1.1">
   <DESCRIPTION>Registry Search Results</DESCRIPTION>
   <RESOURCE name="Search Results">
      <TABLE name="results">
         <FIELD ID="shortName" name="shortName" datatype="char" arraysize="*"/>
         <FIELD ID="title" name="title" datatype="char" arraysize="*"/>
         <FIELD ID="description" name="description" datatype="char" arraysize="*"/>
         <FIELD ID="publisher" name="publisher" datatype="char" arraysize="*"/>
         <FIELD ID="waveband" name="waveband" datatype="char" arraysize="*"/>
         <FIELD ID="identifier" name="identifier" datatype="char" arraysize="*" ucd="ID_MAIN"/>
         <FIELD ID="updated" name="descriptionUpdated" datatype="char" arraysize="*"/>
         <FIELD ID="subject" name="subject" datatype="char" arraysize="*"/>
         <FIELD ID="type" name="type" datatype="char" arraysize="*"/>
         <FIELD ID="contentLevel" name="contentLevel" datatype="char" arraysize="*"/>
         <FIELD ID="version" name="version" datatype="char" arraysize="*"/>
         <FIELD ID="capabilityClass" name="class" datatype="char" arraysize="*"/>
         <FIELD ID="capabilityID" name="standardID" datatype="char" arraysize="*"/>
         <FIELD ID="interfaceClass" name="interfaceClass" datatype="char" arraysize="*"/>
         <FIELD ID="accessURL" name="accessURL" datatype="char" arraysize="*"/>
         <FIELD ID="publisherID" name="publisherIdentifier" datatype="char" arraysize="*"/>
         <FIELD ID="referenceURL" name="referenceURL" datatype="char" arraysize="*"/>
         <FIELD ID="resourceDescription" name="resourceDescription" datatype="char" arraysize="*"/>
         <DATA>
            <TABLEDATA>
              <!-- Each row is a capability -->
              <xsl:apply-templates match="capability" />
            </TABLEDATA>
         </DATA>
      </TABLE>
   </RESOURCE>

</VOTABLE>
   </xsl:template>

   <!--  Get features of the capability and add some useful other data from
     -   the parent Resource, lot of repeated content, but that's necesary for
     -   a VOTable transformation.
     -->
   <xsl:template match="capability">
     <xsl:text>    </xsl:text>
     <TR>
       <!-- shortName -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../shortName" />
       </xsl:call-template>
       <!-- title -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../title" />
       </xsl:call-template>
       <!-- capability description -->
       <xsl:call-template name="truncateValOrNull">
         <xsl:with-param name="val" select="description | ../content/description" />
       </xsl:call-template>
       <!-- publisher -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../curation/publisher" />
       </xsl:call-template>
       <!-- waveband -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../coverage/waveband" />
         <xsl:with-param name="asarray" select="true()"/>
       </xsl:call-template>
       <!-- identifier -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../identifier" />
       </xsl:call-template>
       <!-- discriptionUpdated -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../@updated" />
       </xsl:call-template>
       <!-- subject -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../content/subject" />
         <xsl:with-param name="asarray" select="true()"/>
       </xsl:call-template>
       <!-- type -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../content/type" />
         <xsl:with-param name="asarray" select="true()"/>
       </xsl:call-template>
       <!-- contentLevel -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../content/contentLevel" />
         <xsl:with-param name="asarray" select="true()"/>
       </xsl:call-template>
       <!-- version -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../curation/version" />
       </xsl:call-template>
       <!-- class -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="@xsi:type" />
         <xsl:with-param name="removeScope" select="true()"></xsl:with-param>
       </xsl:call-template>
       <!-- standardID -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="@standardID" />
       </xsl:call-template>
       <!-- interfaceClass -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="interface/self::node()[1]/@xsi:type" />
         <xsl:with-param name="removeScope" select="true()"></xsl:with-param>
       </xsl:call-template>
       <!-- accessURL -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="interface/self::node()[1]/accessURL[1]" />
       </xsl:call-template>
       <!-- publisherID -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../curation/publisher/@ivo-id" />
       </xsl:call-template>
       <!-- referenceURL -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../content/referenceURL" />
       </xsl:call-template>
       <!-- full description for resource -->
       <xsl:call-template name="valOrNull">
         <xsl:with-param name="val" select="../content/description" />
       </xsl:call-template>
     </TR>
     <xsl:text>
</xsl:text>
   </xsl:template>

   <!--  Output value if found or null otherwise -->
   <xsl:template name="valOrNull">
     <xsl:param name="val"/>
     <xsl:param name="asarray" select="false()"/>
     <xsl:param name="removeScope" select="false()"/>
     <xsl:variable name="count" select="count($val)"/>
     <xsl:text>      </xsl:text>
     <TD>
       <xsl:choose>
         <xsl:when test="$asarray">
           <xsl:if test="count($val)>0">
             <xsl:text>#</xsl:text>
           </xsl:if>
           <xsl:for-each select="$val">
             <xsl:value-of select="normalize-space(.)"/>
             <xsl:text>#</xsl:text>
           </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
           <xsl:variable name="withscope">
             <xsl:value-of select="normalize-space($val)"/>
           </xsl:variable>
           <xsl:choose>
             <xsl:when test="$removeScope and contains($withscope, ':')">
               <xsl:value-of select="substring-after($withscope, ':')"/>
             </xsl:when>
             <xsl:otherwise>
               <xsl:value-of select="$withscope"/>
             </xsl:otherwise>
           </xsl:choose>
         </xsl:otherwise>
       </xsl:choose>
       </TD>
       <xsl:text>
</xsl:text>
   </xsl:template>

   <!--  Output truncated value if found or null otherwise -->
   <xsl:template name="truncateValOrNull">
     <xsl:param name="val"/>
     <TD>
       <xsl:value-of select="substring($val,1,80)"/>
     </TD>
     <xsl:text>
</xsl:text>
   </xsl:template>

</xsl:stylesheet>
