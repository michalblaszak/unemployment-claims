      ******************************************************************
      * The main program controlling the process of importing data from
      * XML source files to the VSM dataset.
      *-----------------------------------------------------------------
      * The program calls individual importers which are responsible for
      * reading individual source XML files and transferring data to the
      * to the common VSM file.
      * Each XML source file has a different internal structure thus
      * separate importers needs to process them.
      * records from all the XML source files share the same
      * 'record-id' used to bind data from different XML files into one
      * record.
      * The destination VSM file is a list of records with unique
      * 'record-id'. Each record is a sum of all attributes from all XML
      * files.
      *
      * XML files are located in a UNEMPL PDS dataset:
      *    BYAGE, BYGENDER, BYINDUST, BYRACE, BYETHNIC
      *
      * The structure of the VSAM file is defined in CPY(REPREC).cpy
      *
      * Subprograms:
      *    IMPBYAGE, IMPBYGEN, IMPBYIND,  IMPBYRAC, IMPBYETH
      *
      * The VSM dataset needs to be loaded before first usage. For that
      * purpose the INITUNEJ.jcl JCL script has been created which
      * creates the VSM dataset and initially loads it with a single
      * dummy record with 'record_id' = '00000000'.
      * This program has to delete this record. Since this is
      * implemented in every XML importer, they report back the
      * information that the record has beed deleted so others do not
      * have to repeat this operation. The CLEAN-UP-FLAG is used for
      * this purpose.
      ******************************************************************
      *========================
       IDENTIFICATION DIVISION.
      *========================
       PROGRAM-ID. UNEMPL01.
       AUTHOR. Michal Blaszak.
       DATE-WRITTEN. 2020-05-15.

      *========================
       ENVIRONMENT DIVISION.
      *========================
       CONFIGURATION SECTION.
      *------------------------
      * SOURCE-COMPUTER.
      *     IBM-SYSTEM WITH DEBUGGING MODE.

      *========================
       DATA DIVISION.
      *========================
       FILE SECTION.
      *------------------------
      *------------------------
       WORKING-STORAGE SECTION.
      *------------------------
      * The copybook which brings CLEAN-UP-FLAG used to notify that the
      * dummy record has need deleted.
      * It's shared with all subprograms.
           COPY REPFLAGS.

      * The helper variable to store the subprogram name.
       01  IMPORT-APP PIC X(8).

      *========================
       PROCEDURE DIVISION.
      *========================
       1000-MAIN.
           DISPLAY "Start importing source data."

           MOVE 'IMPBYAGE' TO IMPORT-APP
           CALL IMPORT-APP  USING CLEAN-UP-FLAG

           MOVE 'IMPBYGEN' TO IMPORT-APP
           CALL IMPORT-APP  USING CLEAN-UP-FLAG

           MOVE 'IMPBYIND' TO IMPORT-APP
           CALL IMPORT-APP  USING CLEAN-UP-FLAG

           MOVE 'IMPBYRAC' TO IMPORT-APP
           CALL IMPORT-APP  USING CLEAN-UP-FLAG

           MOVE 'IMPBYETH' TO IMPORT-APP
           CALL IMPORT-APP  USING CLEAN-UP-FLAG

           DISPLAY "Import completed."

           GOBACK
           .

       1000-MAIN-END.
           EXIT.
      ******************************************************************

       END PROGRAM UNEMPL01.
