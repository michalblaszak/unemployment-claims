*********************************************************************
* The training application for the challange "Unemployment Claims". *
*********************************************************************

The solution consists of the following programs:
================================================
1. Set of utility programs helping to set up the database
2. The data importer
3. The report generator

COBOL features used in this exercise:
=====================================
1. Copybooks
   a. Setting up a PDS to store copybooks
   b. Setting up a copybook in the JCL for compilation
2. Subprograms
   a. CALL subprograms with IN/OUT parameters
   b. Compiling subprograms as dynamically linked
3. Dynamic data structures
   a. Uning POINTERS to build, traverse and destroy the linked list
   b. Returning a linked list from the subprogram
4. Parsing XML in the 'parse partial' mode
5. Create a KSDS VSAM dataset
6. Create an alternate index for the KSDS VSAM dataset
7. Utilize IDCAM commands to manage the KSDS VSAM and related alternate index
8. Reading records from VSAM based on main and alternate index


1. Setting up the environment.
=============================
1.1. XML parser
     This solution requires XMLPARSE(XMLSS) be enabled in the system.

1.2. Source code of the entire solution is organized in the following way:
     - LOAD - contains executable binaries (after compilation)
     - CBL - contains all *.cbl files
     - CPY - contains all *.cpy files
     - JCL - contains all JCL job scripts
     - UNEMPL - the PDS containing all input *.xml files

     The target database with related indexes and PATH dataset will be created in the main dataset by launching respective JCL scripts.

1.3. Praparing the target database
     The process requires submitting the following sequence of JCL job scripts:
     a. VSAMJ - create the KSDS VSAM cluster with unique primary index.
     b. LOADVSAM - this script compiles and runs the INITKSDS.cbl program which initially loads the KSDS dataset with a single record with primary key 'record-id' = "00000000".
     c. AIXJ - create the alternate unique index based upon the 'data' key.
     d. BUILDAIX - pull data to the alternate index.
     e. PATHJ - set up a path dataset to linking KSDS with the alternate index.

     As a result the following dataset will be created:
     UNEMPL.KSDS
     UNEMPL.KSDS.DATA
     UNEMPL.KSDS.INDEX
     UNEMPL.KAIX
     UNEMPL.KAIX.DATA
     UNEMPL.KAIX.INDEX
     UNEMPL.KAIX.PATH

2. Importing data
=================
2.1. Compiling and running the importer:
     UNEMPLJ.jcl

2.2. Running already compiled imported:
     UNEMPIRU.jcl

2.3. Both scripts launch UNEMPL program which is the import controller.
     It runs subsequent subprograms:
     IMPBYAGE, IMPBYGEN, IMPBYIND, IMPBYRAC, IMPBYETH
     These subprograms transfer data from the external XML source files to the target VSAM dataset. Each XML has different structure (specific attributes). They all share the same 'record-id' which is used to merge data inside the target dataset.

3. Generatig the repport
========================
3.1. Compiling and running the report
     UNEMRPJ.jcl
3.2. Running already compiled report generator
     UNEMPRRU.jcl
3.3. Both script launch the UNEMMAIN program which acts as a fronted for the report presentation.
     This program makes a sequence of calls to the backend subprogram UNEMPAPI.
     The report generation program makes the following requests for data data
     a. All records sorted in a 'record-id' order
     b. A single record by specific 'record-id'
     c. All records sorted by 'date'
     d. A single record by specific 'date'

     In a response backed provides a linked list of records. The frontend program scans the entire list, agregates amounts inside each of attribute groups, displays the result in a tabular form with the total summary for each category.

     The example of the report can be found in the 'report.txt' file.
