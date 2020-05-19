      ******************************************************************
      * XML importer subprogram.
      ******************************************************************
      * This subprogram is controlled by UNEMPL program.
      *
      * Input paraleters:
      *    CLEAN-UP-FLAG - the flag shared with other importers that the
      *    dummy record (record_id = '00000000') has been deleted.
      *
      * The process:
      *    The program opens the XML dataset and parses it using
      *    'partial' parsing approach in which case there is no need to
      *    load the entire XML but rather parsing can be performed line
      *    by line. This is usefull for long XMLs where the bumber of
      *    elements is unknown.
      *    Values of recognized elements are stored in respecive fields
      *    of the record which is saved to the VSM file.
      *    Firstly the program looks for the 'record_id' in the VSAM
      *    dataset. If not found the new one is created. If the
      *    record exists, attributes just read from the XML are added to
      *    the existing record to the respective fields.
      ******************************************************************
      *========================
       IDENTIFICATION DIVISION.
      *========================
       PROGRAM-ID. IMPBYGEN.
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
           *> The XML input dataset
      *    IN-STATUS from FILESTS copybook
           SELECT IN-DATASET ASSIGN TO BYGENDDD
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS IN-STATUS.

           *> The VSAM output dataset
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
       FD  IN-DATASET
              RECORD CONTAINS 176 CHARACTERS
              RECORDING MODE F.
       01  IN-FD-REC.
           05 PIC X(176).

      * Provides:
      *    FD  REPORT-OUT
      *    01  REPORT-OUT-REC.
       COPY REPREC.

      *------------------------
       WORKING-STORAGE SECTION.
      *------------------------
       77  SOURCE-NAME PIC X(12) VALUE 'BYGENDER.XML'.

      * Provides:
      *    01  IN-STATUS
      *    01  REPORT-STATUS.
       COPY FILESTS.

       01  STATE-MACHINE PIC X(6) VALUE ' '.
           88 STATE-ROOT     VALUE ' '.
           88 STATE-RESPONSE VALUE 'resp'.
           88 STATE-ROWS     VALUE 'rows'.
           88 STATE-ROW      VALUE 'row'.
           88 STATE-REC-ID   VALUE 'rec_id'.
           88 STATE-DATE     VALUE 'date'.
           88 STATE-INA      VALUE 'ina'.
           88 STATE-FEMALE   VALUE 'female'.
           88 STATE-MALE     VALUE 'male'.

       01  DATA-RECORD.
           05 REC-RECORD-ID  PIC X(8)        VALUE ALL SPACES.
           05 REC-DATE       PIC X(10)       VALUE ALL SPACES.
           05 REC-INA        PIC 9(7) COMP-4 VALUE ZERO.
           05 REC-FEMALE     PIC 9(7) COMP-4 VALUE ZERO.
           05 REC-MALE       PIC 9(7) COMP-4 VALUE ZERO.

       01  REC-NO PIC 9(5) COMP-4 VALUE ZERO.

      *------------------------
       LINKAGE SECTION.
      *------------------------
           COPY REPFLAGS.
      *========================
       PROCEDURE DIVISION
           USING CLEAN-UP-FLAG.
      *========================
       1000-MAIN.
           PERFORM 2000-OPEN-SOURCES.
           PERFORM 2500-OPEN-OUT-REPORT.

           IF (NOT IN-OK) OR (NOT REPORT-OUT-FILE-OK) THEN
              DISPLAY "Problems with accessing the database."
              DISPLAY "Exiting ..."
              GO TO 3000-CLOSE-EXIT
           END-IF

           PERFORM 2600-CLEAN-UP-REPORT
           PERFORM 4000-PROCESS-DATA

           GO TO 3000-CLOSE-EXIT
           .

       1000-MAIN-END.
           EXIT.
      ******************************************************************
       2000-OPEN-SOURCES.
           OPEN INPUT IN-DATASET.

           IF NOT IN-OK THEN
              DISPLAY "COULDN'T OPEN " SOURCE-NAME " : " IN-STATUS
           END-IF.

       2000-OPEN-SOURCES-END.
           EXIT.
      ******************************************************************
       2500-OPEN-OUT-REPORT.
           OPEN I-O REPORT-OUT.

           IF NOT REPORT-OUT-FILE-OK THEN
              DISPLAY "Couldn't open UNEMP report : "
                 REPORT-OUT-STATUS
              DISPLAY "VSAM-CODE ==>"
                 " RETURN: " VSAM-RETURN-CODE,
                 " COMPONENT: " VSAM-COMPONENT-CODE,
                 " REASON: " VSAM-REASON-CODE
           END-IF.

       2500-OPEN-OUT-REPORT-END.
           EXIT.
      ******************************************************************
       2600-CLEAN-UP-REPORT.
           *> Clean up the dummy record (from initial loading)
           IF NOT IS-REPORT-CLEAN THEN
              MOVE '00000000' TO FD-RECORD-ID
              DELETE REPORT-OUT
                 INVALID KEY DISPLAY "No dummy record present"
                 NOT INVALID KEY DISPLAY "Dummy record deleted"
              END-DELETE

              SET IS-REPORT-CLEAN TO TRUE
           END-IF
           .

       2600-CLEAN-UP-REPORT-END.
           EXIT.
      ******************************************************************
       3000-CLOSE-EXIT.
           *> Close files which have been opened
           IF IN-OK THEN
              CLOSE IN-DATASET
           END-IF

           IF REPORT-OUT-FILE-OK THEN
              CLOSE REPORT-OUT
           END-IF

           GOBACK.

       3000-CLOSE-EXIT-END.
           EXIT.
      ******************************************************************
      * XML PARSE
      ******************************************************************
       4000-PROCESS-DATA.
           READ IN-DATASET
           IF NOT IN-OK THEN
              DISPLAY 'Error reading ' SOURCE-NAME ' : ' IN-STATUS
              GO TO 3000-CLOSE-EXIT
           END-IF

           DISPLAY "Processing " SOURCE-NAME " ..."

           XML PARSE IN-FD-REC
              PROCESSING PROCEDURE IS 5000-HANDLE-XML-EVENTS

           DISPLAY REC-NO " records done."
           .

       4000-PROCESS-DATA-END.
           EXIT.
      ******************************************************************
      * The actual parsing procedure
      ******************************************************************
       5000-HANDLE-XML-EVENTS.
      D     DISPLAY ' ' XML-event '{' XML-text '}'
           EVALUATE XML-event
              WHEN 'END-OF-INPUT'
                 READ IN-DATASET
                 EVALUATE IN-STATUS
                    WHEN "00"
                       MOVE 1 TO XML-code
      D                DISPLAY 'Continuing with : ' IN-FD-REC
                    WHEN "10"
      D                DISPLAY 'At EOF; no more input.'
                       CONTINUE
                    WHEN OTHER
                       DISPLAY 'Read failed, file status:'
                          IN-STATUS
                       GO TO 3000-CLOSE-EXIT
                 END-EVALUATE
              WHEN 'START-OF-ELEMENT'
      D          DISPLAY '<' XML-text '>'
                 EVALUATE TRUE ALSO XML-text
                    WHEN STATE-ROOT ALSO "response"
                       SET STATE-RESPONSE TO TRUE
                    WHEN STATE-RESPONSE ALSO "row"
                       SET STATE-ROWS TO TRUE
                    WHEN STATE-ROWS ALSO "row"
                       SET STATE-ROW TO TRUE
                       INITIALIZE DATA-RECORD
                    WHEN STATE-ROW ALSO "record_id"
                       SET STATE-REC-ID TO TRUE
                    WHEN STATE-ROW ALSO "date"
                       SET STATE-DATE TO TRUE
                    WHEN STATE-ROW ALSO "ina"
                       SET STATE-INA TO TRUE
                    WHEN STATE-ROW ALSO "female"
                       SET STATE-FEMALE TO TRUE
                    WHEN STATE-ROW ALSO "male"
                       SET STATE-MALE TO TRUE
                    WHEN OTHER
                       CONTINUE
                 END-EVALUATE
      D          DISPLAY STATE-MACHINE
              WHEN 'END-OF-ELEMENT'
      D          DISPLAY '</' XML-text '>'
      D          DISPLAY "! " STATE-MACHINE ":" DATA-RECORD
                 EVALUATE TRUE ALSO XML-text
                    WHEN STATE-RESPONSE ALSO "response"
                       SET STATE-ROOT TO TRUE
                    WHEN STATE-ROWS ALSO "row"
                       SET STATE-RESPONSE TO TRUE
                    WHEN STATE-ROW ALSO "row"
                       SET STATE-ROWS TO TRUE
      D                DISPLAY
      D                   REC-RECORD-ID REC-DATE REC-INA
      D                   REC-FEMALE REC-MALE
                       *> Save the record
                       PERFORM 6000-SAVE-REC
                    WHEN STATE-REC-ID ALSO "record_id"
                    WHEN STATE-DATE   ALSO "date"
                    WHEN STATE-INA    ALSO "ina"
                    WHEN STATE-FEMALE ALSO "female"
                    WHEN STATE-MALE   ALSO "male"
                       SET STATE-ROW TO TRUE
                    WHEN OTHER
                       CONTINUE
                 END-EVALUATE
      D          DISPLAY STATE-MACHINE
              WHEN 'CONTENT-CHARACTERS'
      D          DISPLAY STATE-MACHINE
                 EVALUATE TRUE
                    WHEN STATE-REC-ID
                       MOVE XML-text TO REC-RECORD-ID
                    WHEN STATE-DATE
                       MOVE XML-text TO REC-DATE
                    WHEN STATE-INA
                       MOVE XML-text TO REC-INA
                    WHEN STATE-FEMALE
                       MOVE XML-text TO REC-FEMALE
                    WHEN STATE-MALE
                       MOVE XML-text TO REC-MALE
                    WHEN OTHER
                       CONTINUE
                 END-EVALUATE
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           .

       5000-HANDLE-XML-EVENTS-END.
           EXIT.
      ******************************************************************
      * Saving procedure
      * Try to fing a record with the given record_id.
      * Create new one if not found.
      * Update the existing one if found.
      ******************************************************************
       6000-SAVE-REC.
           ADD 1 TO REC-NO
           MOVE REC-RECORD-ID TO FD-RECORD-ID
           READ REPORT-OUT
              INVALID KEY
                 *> New record
                 PERFORM 6010-ADD-NEW
              NOT INVALID KEY
                 *> Record exists
                 PERFORM 6020-UPDATE
           END-READ.

       6000-SAVE-REC-END.
           EXIT.
      ******************************************************************
       6010-ADD-NEW.
           PERFORM 6050-MOVE-DATA-TO-BUFFER
           WRITE REPORT-OUT-REC
              INVALID KEY
                 DISPLAY "Couldn't save the record " FD-RECORD-ID
                 " STATUS: " REPORT-OUT-STATUS
                 " RETURN: " VSAM-RETURN-CODE
                 " COMPONENT: " VSAM-COMPONENT-CODE
                 " REASON: " VSAM-REASON-CODE
           END-WRITE.

       6010-ADD-NEW-END.
           EXIT.
      ******************************************************************
       6020-UPDATE.
           PERFORM 6050-MOVE-DATA-TO-BUFFER
           REWRITE REPORT-OUT-REC
              INVALID KEY
                 DISPLAY "Couldn't update the record " FD-RECORD-ID
                 " STATUS: " REPORT-OUT-STATUS
                 " RETURN: " VSAM-RETURN-CODE
                 " COMPONENT: " VSAM-COMPONENT-CODE
                 " REASON: " VSAM-REASON-CODE
           END-REWRITE.

       6020-UPDATE-END.
           EXIT.
      ******************************************************************
       6050-MOVE-DATA-TO-BUFFER.
           MOVE REC-RECORD-ID TO FD-RECORD-ID
           MOVE REC-DATE      TO FD-DATE
           MOVE REC-INA       TO FD-INA-GENDER
           MOVE REC-FEMALE    TO FD-FEMALE
           MOVE REC-MALE      TO FD-MALE
           .

       6050-MOVE-DATA-TO-BUFFER-END.
           EXIT.

       END PROGRAM IMPBYGEN.
