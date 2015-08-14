###########################################################
### XML class to Xpath and compute on nodes for an XML tree.
### Includes application of a function to handle multiple and missing
### nodes in the tree and assemble a final data.frame from the results.
### http://www.zvon.org/xxl/XPathTutorial/Output/example1.html
### for good tutorial examples on xpath operators
###########################################################
library(XML)

# We can either take the xml inline or from a file
doc <- xmlParse( '<?xml version="1.0" encoding="utf-8"?>
                 <iati-activities version="1.03" generated-datetime="2015-07-07T16:49:09+00:00">
                 <iati-activity last-updated-datetime="2014-08-11T14:36:59+00:00" xml:lang="en" default-currency="EUR">
                 <iati-identifier>NL-KVK-41160054-100530</iati-identifier>
                 <title>Improvement of basic health care</title>
                 <reporting-org ref="NL-KVK-41160054" type="21">Stichting Cordaid</reporting-org>
                 <participating-org role="Accountable" ref="NL-KVK-41160054" type="21">Cordaid</participating-org>
                 <participating-org role="Funding" ref="EU" type="15">EU</participating-org>
                 <participating-org role="Funding" type="21">Cordaid Memisa</participating-org>
                 <participating-org role="Funding" ref="NL-1" type="10">Dutch Ministry of Foreign Affairs</participating-org>
                 <participating-org role="Implementing" type="21">CORDAID RCA</participating-org>
                 <recipient-country percentage="100" code="CF">CENTRAL AFRICAN REPUBLIC</recipient-country>
                 <budget type="1">
                 <period-start iso-date="2010-01-01"></period-start>
                 <period-end iso-date="2013-02-28"></period-end>
                 </budget>
                 </iati-activity>
                 <iati-activity last-updated-datetime="2013-07-19T14:12:14+00:00" xml:lang="en" default-currency="EUR">
                 <iati-identifier>NL-KVK-41160054-100625</iati-identifier>
                 <title>Pigs for Pencils</title>
                 <reporting-org ref="NL-KVK-41160054" type="21">Stichting Cordaid</reporting-org>
                 <participating-org role="Funding" ref="NL-1" type="10">Dutch Ministry of Foreign Affairs</participating-org>
                 <participating-org role="Funding" type="60">Stichting Kapatiran</participating-org>
                 <participating-org role="Implementing" type="22">PREDA Foundation Inc.</participating-org>
                 <participating-org role="Accountable" ref="NL-KVK-41160054" type="21">Cordaid</participating-org>
                 <budget type="2">
                 <period-start iso-date="2010-04-20"></period-start>
                 <period-end iso-date="2012-10-02"></period-end>
                 <value value-date="2010-04-20">12500</value>
                 </budget>
                 </iati-activity>
                 <iati-activity last-updated-datetime="2015-04-08T03:01:58+00:00" xml:lang="en" default-currency="EUR">
                 <iati-identifier>NL-KVK-41160054-100815</iati-identifier>
                 <title>Job and housing opportunities for women </title>
                 <reporting-org ref="NL-KVK-41160054" type="21">Stichting Cordaid</reporting-org>
                 <participating-org role="Funding" ref="NL-1" type="10">Dutch Ministry of Foreign Affairs</participating-org>
                 <participating-org role="Implementing" type="22">WISE</participating-org>
                 <participating-org role="Accountable" ref="NL-KVK-41160054" type="21">Cordaid</participating-org>
                 <budget type="2">
                 <period-start iso-date="2010-10-01"></period-start>
                 <period-end iso-date="2011-12-31"></period-end>
                 <value value-date="2010-10-01">227000</value>
                 </budget>
                 </iati-activity>
                 </iati-activities>
                 ')
doc <- xmlInternalTreeParse("test.xml")
# now grab the node set from the tree
nodes<- getNodeSet(doc, "//iati-activity")

#Compare
xpathSApply(doc, "//budget/value", xmlValue)
xpathSApply(doc, "//participating-org[@role='Funding']", xmlValue)

# length of element name
xpathSApply(doc, "//*[string-length(name()) > 6]")

# The descendant axis contains the descendants of the context node;
# a descendant is a child or a child of a child and so on; thus the descendant axis never 
# contains attribute or namespace nodes
# Select all descendants of document root and therefore all elements

xpathSApply(doc, "/descendant::*")
xpathSApply(doc, "//iati-activity/descendant::*")

# parent node of present context
xpathSApply(doc, "//budget/parent::*")

# AND operator
xpathSApply(doc, "//reporting-org | //participating-org")

sapply(nodes, function(x) xpathSApply(x, "./budget/value", xmlValue))
sapply(nodes, function(x) xpathSApply(x, "./participating-org[@role='Funding']", xmlValue))
# Add a function to handle missing or multiple nodes and then create the data.frame

xpath2 <-function(x, path){
    y <- xpathSApply(x, path, xmlValue)
    ifelse(length(y)==0, NA, 
           ifelse(length(y)>1, paste(y, collapse=", "), y))
}

# Build the vars
GrantAmount <- sapply(nodes, function(x) xpath2(x, "./budget/value"))
Funding <- sapply(nodes, function(x) xpath2(x, "./participating-org[@role='Funding']"))
UniqueID  <- sapply(nodes, function(x) xpath2(x, "./iati-identifier"))
GrantTitle <- sapply(nodes, function(x) xpath2(x, "./title"))
Recipient <-  sapply(nodes, function(x) xpath2(x, "./participating-org[@role='Implementing']"))

# Build the df, with NA for missing values and multiple nodes handled!
data.frame(UniqueID, GrantTitle, GrantAmount, Recipient)