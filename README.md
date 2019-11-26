![](https://github.com/rcfgroup/clinconcept/workflows/master/badge.svg)

# clinconcept

Data from electronic healthcare records are often based around clinical concept dictionaries, which can be difficult to use for research due to the design, size and complex nature of these dictionaries. 

clinconcept is an R package which provides tools for building local and remote databases from simple downloads containing commonly used concept dictionaries and then working with the concepts and terms. 

Supported in this initial version are tools for handling READ versions 2 and 3, SNOMED-CT and ICD10 provided for free use by the NHS (https://isd.digital.nhs.uk). The package is designed so it can be embedded in algorithms working with national data sets such as UK Biobank and also local routine health care data. 
