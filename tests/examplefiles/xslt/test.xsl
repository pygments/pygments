<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml"/>
	<xsl:template match="/">
		<customers>
			<xsl:apply-templates select="customers/customer[@Country='Germany']"/>
		</customers>
	</xsl:template>
	<xsl:template match="customers">
		<xsl:apply-templates/>

	</xsl:template>
	<xsl:template match="customer">
		<customer>
			<xsl:attribute name="CompanyName"><xsl:value-of select="@CompanyName"/></xsl:attribute>
			<xsl:attribute name="CustomerID"><xsl:value-of select="@CustomerID"/></xsl:attribute>
			<xsl:attribute name="Country"><xsl:value-of select="@Country"/></xsl:attribute>
		</customer>
		<xsl:apply-templates/>
	</xsl:template>

</xsl:stylesheet>

