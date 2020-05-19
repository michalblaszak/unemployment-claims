      ******************************************************************
      * The backend API subprogram for the Reporting Program
      ******************************************************************
      * This is the backend subprogram called the frontend one
      * (UNEMMAIN).
      * This program reads data from the VSM database file (UNEMPLRP).
      * Parameters:
      *    (in)  RECORD-ID-IN  - "00000000" for all records.
      *    (in)  SORT-OREDR-IN - the sorting sequence/searching key
      *                          'RECORD-ID', 'DATE'
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
      ******************************************************************
      *========================
       IDENTIFICATION DIVISION.
      *========================
       PROGRAM-ID. UNEMPAPI.
       AUTHOR. Michal Blaszak.
       DATE-WRITTEN. 2020-05-15.

      *========================
       ENVIRONMENT DIVISION.
      *========================
       CONFIGURATION SECTION.
      *------------------------
      * SOURCE-COMPUTER.
      *     IBM-SYSTEM WITH DEBUGGING MODE.

      *------------------------
       INPUT-OUTPUT SECTION.
      *------------------------
       FILE-CONTROL.
      *    REPORT-OUT, FD-RECORD-ID from REPREC copybook
      *    REPORT-OUT-STATUS VSAM-CODE from FILESTS copybook
           SELECT REPORT-OUT ASSIGN TO REPORTDD
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FD-RECORD-ID
           ALTERNATE RECORD KEY IS FD-DATE
           FILE STATUS IS REPORT-OUT-STATUS VSAM-CODE.

      *========================
       DATA DIVISION.
      *========================
       FILE SECTION.
      *------------------------
      * Provides:
      *    FD  REPORT-OUT
      *    01  REPORT-OUT-REC.
       COPY REPREC.

      *------------------------
       WORKING-STORAGE SECTION.
      *------------------------
      * Provides:
      *    01  IN-STATUS
      *    01  REPORT-STATUS.
       COPY FILESTS.

      * The flag indicating that we reached the end of the database
       01  FLAGS.
           05 LASTREC PIC X VALUE 'N'.
              88 IS-LASTREC VALUE 'T'.

      * Temporary variables to build the linked list
       01  NEW-NODE POINTER.
       01  CURRENT-NODE POINTER.

      *------------------------
       LINKAGE SECTION.
      *------------------------
      * Input parameters
       01  RECORD-ID-IN   PIC X(10). *> can be used for 'record-id'
                                     *> and 'date'
       01  SORT-ORDER-IN  PIC X(9). *> 'RECORD-ID', 'DATE'
           88 SORT-RECORD-ID VALUE 'RECORD-ID'.
           88 SORT-DATE      VALUE 'DATE'.

      * The copybook provides:
      * RESPONSE-OUT, FIRST-ELEMENT
       COPY REPSTAT.

      * Structures to map allocated memory.
      * Used as temporaty variabled to build a linked list
       01  CURRENT-REC.
           05 RET-REC-NEXT-PTR POINTER VALUE NULL.
           05 PAYLOAD PIC X(312).

       01  NEW-REPORT-LIST-OUT.
           05 RET-REC-NEXT-PTR POINTER VALUE NULL.
           05 PAYLOAD PIC X(312).

      *========================
       PROCEDURE DIVISION
           USING RECORD-ID-IN SORT-ORDER-IN RESPONSE-OUT FIRST-ELEMENT.
      *========================
       1000-MAIN.
      D    DISPLAY "[API] RECORD-ID-IN: '" RECORD-ID-IN "'"
      D    DISPLAY "[API] SORT-ORDER-IN: '" SORT-ORDER-IN "'"

           INITIALIZE FLAGS

      *    Open the VSM dataset
           OPEN INPUT REPORT-OUT

           IF NOT REPORT-OUT-FILE-OK THEN
              DISPLAY "[API] Couldn't open the database."
      D       DISPLAY "[API] " REPORT-STATUS
              SET RET-STATUS-DB-ERROR TO TRUE
              GOBACK
           END-IF

      D    DISPLAY "[API] Record ID: " RECORD-ID-IN

           EVALUATE TRUE          ALSO RECORD-ID-IN
              WHEN SORT-RECORD-ID ALSO "00000000"
              WHEN SORT-DATE      ALSO "0000-00-00"
                    PERFORM 1050-PROCESS-ALL *> Scan all records
              WHEN OTHER
                    PERFORM 1060-PROCESS-ONE *> Process one record
                                             *> by RECORD-ID or by DATE
           END-EVALUATE

      D    DISPLAY "[API] First element: " FIRST-ELEMENT

           CLOSE REPORT-OUT
           GOBACK
           .

       1000-MAIN-END.
           EXIT.

      ******************************************************************
      * This procedure generats the report for all records from the
      * database.
      * The result is the chained list of nodes containing the record
      * data in a raw alphanumeric format.
      ******************************************************************
       1050-PROCESS-ALL.
           SET FIRST-ELEMENT TO NULL

           IF SORT-RECORD-ID THEN
      D       DISPLAY "[API] Use index: FD-RECORD-ID"

              MOVE "00000000" TO FD-RECORD-ID
              START REPORT-OUT KEY IS >= FD-RECORD-ID
      *           INVALID KEY SET IS-LASTREC TO TRUE
           ELSE
      D       DISPLAY "[API] Use index: FD-DATE"

              MOVE "0000-00-00" TO FD-DATE
              START REPORT-OUT KEY IS >= FD-DATE
      *           INVALID KEY SET IS-LASTREC TO TRUE
           END-IF

           IF NOT REPORT-OUT-FILE-OK THEN
              DISPLAY "[API] Index not working."
      D       DISPLAY "[API] " REPORT-STATUS
              SET RET-STATUS-DB-ERROR TO TRUE
              GOBACK
           ELSE
      D        DISPLAY '[API] First record found'
      D        DISPLAY REPORT-OUT-REC
              CONTINUE
           END-IF

           PERFORM 2000-READ-NEXT-RECORD

           PERFORM UNTIL IS-LASTREC *> For all records in the database
      D       DISPLAY '[API] In the loop'
      D       DISPLAY REPORT-OUT-REC

              ADD 1 TO RET-REC-NO

              *> Create a new record node
              ALLOCATE NEW-REPORT-LIST-OUT
                INITIALIZED RETURNING NEW-NODE

              IF FIRST-ELEMENT = NULL THEN *> If this is the first node
                 *> The first node is the head of the list and will be
                 *> returned to the calling program
      D          DISPLAY "[API] BEFORE SET FIRST"
                 SET FIRST-ELEMENT TO NEW-NODE
                 SET CURRENT-NODE TO NEW-NODE

                 SET ADDRESS OF CURRENT-REC TO CURRENT-NODE
                 SET RET-REC-NEXT-PTR OF CURRENT-REC TO NULL
                 MOVE REPORT-OUT-REC TO PAYLOAD OF CURRENT-REC

      D          DISPLAY "[API] FIRST REC: " FIRST-ELEMENT
      D          DISPLAY "[API] NEW-REC: " NEW-NODE
              ELSE *> This is a subsequent node
      D          DISPLAY "[API] " CURRENT-NODE ".." NEW-NODE

                 SET ADDRESS OF CURRENT-REC TO CURRENT-NODE
                 SET RET-REC-NEXT-PTR OF CURRENT-REC TO NEW-NODE

      D          DISPLAY "[API] " CURRENT-NODE "->"
      D                   RET-REC-NEXT-PTR OF CURRENT-REC

                 SET CURRENT-NODE TO NEW-NODE

                 SET ADDRESS OF CURRENT-REC TO CURRENT-NODE
                 SET RET-REC-NEXT-PTR OF CURRENT-REC TO NULL
                 MOVE REPORT-OUT-REC TO PAYLOAD OF CURRENT-REC
              END-IF

      D       DISPLAY "[API] " REPORT-OUT-REC

              PERFORM 2000-READ-NEXT-RECORD
           END-PERFORM
           .

       1050-PROCESS-ALL-END.
           EXIT.
      ******************************************************************
      * This procedure is processing a request for a single record.
      ******************************************************************
       1060-PROCESS-ONE.
           PERFORM 3000-READ-RECORD *> Fint the requested record

           EVALUATE TRUE
              WHEN REPORT-OUT-FILE-OK *> If the record was found
                 *> Allocate a single node
      D          DISPLAY "[API] Record found"

                 ALLOCATE NEW-REPORT-LIST-OUT
                    INITIALIZED RETURNING CURRENT-NODE

                 SET FIRST-ELEMENT TO CURRENT-NODE

                 SET ADDRESS OF CURRENT-REC TO CURRENT-NODE
                 SET RET-REC-NEXT-PTR OF CURRENT-REC TO NULL
                 MOVE REPORT-OUT-REC TO PAYLOAD OF CURRENT-REC

                 SET RET-STATUS-OK TO TRUE
              WHEN REPORT-OUT-RECORD-NOT-FOUND
      D          DISPLAY "[API] Record not found"
                 SET RET-STATUS-REC-NOT-FOUND TO TRUE
              WHEN OTHER
      D          DISPLAY "[API] Other DB error"
                 SET RET-STATUS-DB-ERROR TO TRUE
           END-EVALUATE
           .

       1060-PROCESS-ONE-END.
           EXIT.
      ******************************************************************
      * Reading records sequentially.
      * Used for generating the report for all records.
      ******************************************************************
       2000-READ-NEXT-RECORD.
           READ REPORT-OUT NEXT

           EVALUATE TRUE
                WHEN REPORT-OUT-FILE-OK
      D              DISPLAY "[API] Read OK" 
                     CONTINUE
                WHEN REPORT-OUT-END-OF-FILE 
      D              DISPLAY "[API] EOF"
                     SET IS-LASTREC TO TRUE
                WHEN REPORT-OUT-RECORD-NOT-FOUND
                     PERFORM 4000-VSAM-CODE-DISPLAY 
                WHEN OTHER
                     PERFORM 4000-VSAM-CODE-DISPLAY
           END-EVALUATE
           .

       2000-READ-NEXT-RECORD-END.
           EXIT.
      ******************************************************************
      * Reads a record by key.
      * Used for a single-record requests.
      ******************************************************************
       3000-READ-RECORD.
           IF SORT-RECORD-ID THEN
      D       DISPLAY "[API] Use index: FD-RECORD-ID"

              MOVE RECORD-ID-IN TO FD-RECORD-ID
              START REPORT-OUT KEY IS >= FD-RECORD-ID
              READ REPORT-OUT KEY IS FD-RECORD-ID
           ELSE
      D       DISPLAY "[API] Use index: FD-DATE"

              MOVE RECORD-ID-IN TO FD-DATE
              READ REPORT-OUT KEY IS FD-DATE
           END-IF
           .

       3000-READ-RECORD-END.
           EXIT.

      ******************************************************************
       4000-VSAM-CODE-DISPLAY.
           DISPLAY "[API] File status: " REPORT-OUT-STATUS
           DISPLAY "[API] VSAM-CODE ==>"
              " RETURN: "  VSAM-RETURN-CODE,
              " COMPONENT: "  VSAM-COMPONENT-CODE,
              " REASON: "  VSAM-REASON-CODE.

       4000-VSAM-CODE-DISPLAY-END.
           EXIT.

       END PROGRAM UNEMPAPI.
