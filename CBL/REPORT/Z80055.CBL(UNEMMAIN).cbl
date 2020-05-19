      ******************************************************************
      * The main Reporting Program
      ******************************************************************
      * This is the frontend program calling the backend API for report
      * data based upon the record_id value.
      * Subprograms:
      *  UNEMPAPI: parameters:
      *    (in)  record_id     - "00000000" for all records.
      *    (out) RESPONSE-OUT  - the structure containing the status of
      *                          the request and the number of records
      *                          processed.
      *    (out) FIRST-ELEMENT - the pointer to the first element in the
      *                          list of returned records.
      *                          Records are formed in a linked list.
      *                          Each record is wrapped with the
      *                          structure containing the pointer to the
      *                          next record and the payload containing
      *                          actual data. The last record points to
      *                          the NULL as the next one.
      *-----------------------------------------------------------------
      * At this point the program sends two requests:
      * 1. "00000000" - all records. The program lists all them without
      *                 any formatting.
      * 2. "01012012" - just one record. The response is displayed
      *                 without any formatting.
      ******************************************************************
      *========================
       IDENTIFICATION DIVISION.
      *========================
       PROGRAM-ID. UNEMMAIN.
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
      *------------------------
       WORKING-STORAGE SECTION.
      *------------------------
      * The name of the subprogram to call
       01  UNEMPAPI PIC X(8) VALUE "UNEMPAPI".

      * Declaration of parameters used for call to the API subprogram
       COPY REPSTAT.

      * A temporary variable for walking through the list of returned
      * records
       01  CURRENT-NODE POINTER.

       01  REPORT-PAYLOAD-REC.
           COPY PAYLOAD.

      * Report temporary variables
       01  REPORT-COMP-REC.
           05 COMP-BY-AGE      COMP-4 PIC 9(7) VALUE ZERO.
           05 COMP-BY-GENDER   COMP-4 PIC 9(7) VALUE ZERO.
           05 COMP-BY-INDUSTRY COMP-4 PIC 9(7) VALUE ZERO.
           05 COMP-BY-RACE     COMP-4 PIC 9(7) VALUE ZERO.
           05 COMP-BY-ETHNIC   COMP-4 PIC 9(7) VALUE ZERO.

           05 SUM-BY-AGE      COMP-4 PIC 9(7) VALUE ZERO.
           05 SUM-BY-GENDER   COMP-4 PIC 9(7) VALUE ZERO.
           05 SUM-BY-INDUSTRY COMP-4 PIC 9(7) VALUE ZERO.
           05 SUM-BY-RACE     COMP-4 PIC 9(7) VALUE ZERO.
           05 SUM-BY-ETHNIC   COMP-4 PIC 9(7) VALUE ZERO.

      * A reports header
       01  REPORT-DISP-HEADER.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(8)  VALUE '  ID   '.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(10) VALUE '   Date   '.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(9)  VALUE ' By Age  '.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(9)  VALUE 'By Gender'.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(9)  VALUE 'By Indust'.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(9)  VALUE ' By Race '.
           05 FILLER PIC X     VALUE '|'.
           05 FILLER PIC X(9)  VALUE 'By Ethnic'.
           05 FILLER PIC X     VALUE '|'.

       01  REPORT-DISP-SEPARATOR PIC X(71) VALUE ALL '-'.

      * A structure to hold a line of the report to be displayed
       01  REPORT-LINE-DISP-REC.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-RECORD-ID   PIC X(8)      VALUE ALL SPACES.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-DATE        PIC X(10)     VALUE ALL SPACES.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-BY-AGE      PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-BY-GENDER   PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-BY-INDUSTRY PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-BY-RACE     PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER          PIC X         VALUE '|'.
           05 REP-BY-ETHNIC   PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER          PIC X         VALUE '|'.

      * A structure to hold a line of the report summary to be displayed
       01  REPORT-SUMMARY-DISP-REC.
           05 FILLER            PIC X         VALUE '|'.
           05 TOTAL-RECORD-ID   PIC X(19)   VALUE ' Total             '.
           05 FILLER            PIC X         VALUE '|'.
           05 TOTAL-BY-AGE      PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER            PIC X         VALUE '|'.
           05 TOTAL-BY-GENDER   PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER            PIC X         VALUE '|'.
           05 TOTAL-BY-INDUSTRY PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER            PIC X         VALUE '|'.
           05 TOTAL-BY-RACE     PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER            PIC X         VALUE '|'.
           05 TOTAL-BY-ETHNIC   PIC Z,ZZZ,ZZ9 VALUE ZERO.
           05 FILLER            PIC X         VALUE '|'.

       01  ID-PAR         PIC X(10).
       01  SORT-ORDER-PAR PIC X(9).
       
      *------------------------
       LINKAGE SECTION.
      *------------------------
      * Used for destructuring the pointer of the data record from the
      * linked list
      * (CURRENT-NODE pointer mapped to CURRENT-REC structure)
       01  CURRENT-REC.
           05 RET-REC-NEXT-PTR POINTER.
           05 PAYLOAD PIC X(312).

      *========================
       PROCEDURE DIVISION.
      *========================
       1000-MAIN.
      * Request "All" records ("00000000") sorted by record-id
           DISPLAY "[MAIN] CALL-API: All records"

           MOVE "00000000" TO ID-PAR
           MOVE "RECORD-ID" TO SORT-ORDER-PAR

           CALL UNEMPAPI USING
              BY CONTENT ID-PAR
              BY CONTENT SORT-ORDER-PAR
              BY REFERENCE RESPONSE-OUT FIRST-ELEMENT

           DISPLAY "[MAIN] CALL-API DONE"

           PERFORM 2000-DISPLAY-RESULTS

      * Request one record ("01012012")
           DISPLAY "[MAIN] CALL-API: 01012012"

           INITIALIZE RESPONSE-OUT ALL TO VALUE
           INITIALIZE REPORT-COMP-REC ALL TO VALUE

           MOVE "01012012" TO ID-PAR
           MOVE "RECORD-ID" TO SORT-ORDER-PAR

           CALL UNEMPAPI USING
              BY CONTENT ID-PAR
              BY CONTENT SORT-ORDER-PAR
              BY REFERENCE RESPONSE-OUT FIRST-ELEMENT

           DISPLAY "[MAIN] CALL-API DONE"

           PERFORM 2000-DISPLAY-RESULTS

      * Request "All" records ("0000-00-00") sorted by date
           DISPLAY "[MAIN] CALL-API: All records"

           INITIALIZE RESPONSE-OUT ALL TO VALUE
           INITIALIZE REPORT-COMP-REC ALL TO VALUE

      D    DISPLAY "[MAIN] RET-STATUS: " RET-STATUS

           MOVE "0000-00-00" TO ID-PAR
           MOVE "DATE" TO SORT-ORDER-PAR

           CALL UNEMPAPI USING
              BY CONTENT ID-PAR
              BY CONTENT SORT-ORDER-PAR
              BY REFERENCE RESPONSE-OUT FIRST-ELEMENT

           DISPLAY "[MAIN] CALL-API DONE"

           PERFORM 2000-DISPLAY-RESULTS

      * Request single record ("2015-01-01") sorted by date
           DISPLAY "[MAIN] CALL-API: 2015-01-01"

           INITIALIZE RESPONSE-OUT ALL TO VALUE
           INITIALIZE REPORT-COMP-REC ALL TO VALUE

      D    DISPLAY "[MAIN] RET-STATUS: " RET-STATUS

           MOVE "2015-01-01" TO ID-PAR
           MOVE "DATE" TO SORT-ORDER-PAR

           CALL UNEMPAPI USING
              BY CONTENT ID-PAR
              BY CONTENT SORT-ORDER-PAR
              BY REFERENCE RESPONSE-OUT FIRST-ELEMENT

           DISPLAY "[MAIN] CALL-API DONE"

           PERFORM 2000-DISPLAY-RESULTS

           GOBACK
           .

       1000-MAIN-END.
           EXIT.
      ******************************************************************
      * This procedure just displays the list of records.
      * If the single record was requested then the list returned by the
      * API contains only one element so the same procedure can be used.
      * Record's data is displayed in a raw format.
      ******************************************************************
       2000-DISPLAY-RESULTS.
           EVALUATE TRUE  *> Check the request status
              WHEN RET-STATUS-OK
      D          DISPLAY "[MAIN] RET-STATUS-OK"
      D          DISPLAY "[MAIN] FIRST ELEMENT: " FIRST-ELEMENT
      D          DISPLAY "[MAIN] ADDR FIRST ELEMENT: "
      D             ADDRESS OF FIRST-ELEMENT

                 *> The report header
                 DISPLAY REPORT-DISP-SEPARATOR
                 DISPLAY REPORT-DISP-HEADER
                 DISPLAY REPORT-DISP-SEPARATOR
                 *> Process the report lines
                 *> Set the first element from the list
                 SET CURRENT-NODE TO FIRST-ELEMENT
                 *> Loop through all elements in the list
                 PERFORM WITH TEST BEFORE UNTIL CURRENT-NODE = NULL
                    *> Map the pointer to the list node structure
                    SET ADDRESS OF CURRENT-REC TO CURRENT-NODE

      D             DISPLAY CURRENT-NODE "->" RET-REC-NEXT-PTR

                    *> Display the record
                    PERFORM 4000-DISPLAY-REPORT-LINE
      D             DISPLAY PAYLOAD OF CURRENT-REC

      D             DISPLAY "[MAIN] Next element: " RET-REC-NEXT-PTR

                    *> Take the next element from the list
                    SET CURRENT-NODE TO RET-REC-NEXT-PTR
                 END-PERFORM

                 PERFORM 5000-DISP-REPORT-SUMMARY
              WHEN RET-STATUS-REC-NOT-FOUND
                 *> This shouldn't happen for "00000000"
                 DISPLAY "Requested data not found"
              WHEN RET-STATUS-DB-ERROR
                 DISPLAY  "Internal error"
              WHEN OTHER
                 DISPLAY '[MAIN] Unhandled response code "' 
                    RET-STATUS '"'
           END-EVALUATE

      * After the entire list has been processed we need to release
      * allcoated resources
           PERFORM 3000-FREE-UP
           .

       2000-DISPLAY-RESULTS-END.
           EXIT.

      ******************************************************************
      * This procedure releases resources allocated in the called API.
      * The API subprogram returns a linked list of dynamically
      * allocated nodes.
      * We could make another request to the API (what would be more
      * consistent from the architecture standpoint) but it was easier
      * to do it here for such a simple exercise.
      ******************************************************************
       3000-FREE-UP.
           *> Walk though the list and release each node
           SET CURRENT-NODE TO FIRST-ELEMENT

           PERFORM WITH TEST BEFORE UNTIL CURRENT-NODE = NULL
              SET ADDRESS OF CURRENT-REC TO CURRENT-NODE
              SET CURRENT-NODE TO RET-REC-NEXT-PTR

              FREE FIRST-ELEMENT
              SET FIRST-ELEMENT TO CURRENT-NODE
           END-PERFORM
           SET FIRST-ELEMENT TO NULL
           .

       3000-FREE-UP-END.
           EXIT.
      ******************************************************************
      * This procedure takes a record of payload and displays in a
      * formatted way.
      * Collecting the aggregated sum is also handled here.
      ******************************************************************
       4000-DISPLAY-REPORT-LINE.
           MOVE PAYLOAD OF CURRENT-REC TO REPORT-PAYLOAD-REC

           *> Calculate the report line
           ADD FD-INA-AGE FD-LESS-22 FD-22-24 FD-25-34 FD-35-44
               FD-45-54 FD-55-59 FD-60-64 FD-MORE-64
               GIVING COMP-BY-AGE
           ADD FD-INA-GENDER FD-FEMALE FD-MALE
               GIVING COMP-BY-GENDER
           ADD FD-INA-INDUSTRY FD-WHOLESALE-TRADE
               FD-TRANSPORTATION-WAREHOUSE FD-CONSTRUCTION
               FD-FINANCE-INSURANCE FD-MANUFACTURING
               FD-AGRICULT-FORESTRY-FISHING-H FD-PUBLIC-ADMINISTRATION
               FD-UTILITIES FD-ACCOMODATION-FOOD-SERVICES
               FD-INFORMATION FD-PROFESSION-SCIENCE-TECH-SER
               FD-REAL-ESTATE-RENTAL-LEASING
               FD-OTHER-SERV-EXCEPT-PUBLIC-AD
               FD-MGMT-OF-COMPANIES-ENTERPRIS
               FD-EDUCATIONAL-SERVICES FD-MINING
               FD-HEALTH-CARE-SOCIAL-ASSISTAN
               FD-ARTS-ENTERTAINMENT-RECREATI
               FD-ADMIN-SPRT-WASTE-REMEDIA-SE FD-RETAIL-TRADE
               GIVING COMP-BY-INDUSTRY
           ADD FD-INA-RACE FD-WHITE FD-ASIAN
               FD-BLACK-OR-AFRICAN-AMERICAN
               FD-AMERIC-INDIAN-OR-ALASKA-NAT
               FD-NATIVE-HAWAII-OR-OTHER-PACI
               GIVING COMP-BY-RACE
           ADD FD-INA-ETHNIC FD-HISPANIC-OR-LATINO
               FD-NOT-HISPANIC-OR-LATINO
               GIVING COMP-BY-ETHNIC

           MOVE FD-RECORD-ID TO REP-RECORD-ID
           MOVE FD-DATE TO REP-DATE

           MOVE COMP-BY-AGE TO REP-BY-AGE
           MOVE COMP-BY-GENDER TO REP-BY-GENDER
           MOVE COMP-BY-INDUSTRY TO REP-BY-INDUSTRY
           MOVE COMP-BY-RACE TO REP-BY-RACE
           MOVE COMP-BY-ETHNIC TO REP-BY-ETHNIC

           *> Add the line to the report summary
           ADD COMP-BY-AGE TO SUM-BY-AGE
           ADD COMP-BY-GENDER TO SUM-BY-GENDER
           ADD COMP-BY-INDUSTRY TO SUM-BY-INDUSTRY
           ADD COMP-BY-RACE TO SUM-BY-RACE
           ADD COMP-BY-ETHNIC TO SUM-BY-ETHNIC

           DISPLAY REPORT-LINE-DISP-REC
           .

       4000-DISPLAY-REPORT-LINE-END.
           EXIT.
      ******************************************************************
      * Thus procedure displays the report final summary
       5000-DISP-REPORT-SUMMARY.
           MOVE SUM-BY-AGE TO TOTAL-BY-AGE
           MOVE SUM-BY-GENDER TO TOTAL-BY-GENDER
           MOVE SUM-BY-INDUSTRY TO TOTAL-BY-INDUSTRY
           MOVE SUM-BY-RACE TO TOTAL-BY-RACE
           MOVE SUM-BY-ETHNIC TO TOTAL-BY-ETHNIC

           DISPLAY REPORT-DISP-SEPARATOR
           DISPLAY REPORT-SUMMARY-DISP-REC
           DISPLAY REPORT-DISP-SEPARATOR
           .

       5000-DISP-REPORT-SUMMARY-END.
           EXIT.

       END PROGRAM UNEMMAIN.
