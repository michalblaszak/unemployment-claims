      ******************************************************************
      * This the utility program initialy loads a newly create KSDS 
      * dataset.
      * It writes a single record and then removes it from the database.
      ******************************************************************
      *========================
       IDENTIFICATION DIVISION. 
      *========================
       PROGRAM-ID. INITKSDS.
       AUTHOR. Michal Blaszak.
       DATE-WRITTEN. 2020-05-15.

      *========================
       ENVIRONMENT DIVISION.
      *========================
       CONFIGURATION SECTION.
      *------------------------
       SOURCE-COMPUTER.
           IBM-SYSTEM WITH DEBUGGING MODE.

      *------------------------
       INPUT-OUTPUT SECTION.
      *------------------------
       FILE-CONTROL.
      *    REPORT-OUT, FD-RECORD-ID from REPREC copybook
      *    REPORT-OUT-STATUS VSAM-CODE from FILESTS copybook
           SELECT REPORT-OUT ASSIGN TO REPORTDD
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS FD-RECORD-ID
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

      *========================
       PROCEDURE DIVISION.
      *========================
       1000-MAIN. 
           OPEN OUTPUT REPORT-OUT

           IF NOT REPORT-OUT-FILE-OK THEN
              DISPLAY "Couldn't open the database."
              PERFORM 2000-VSAM-CODE-DISPLAY

              STOP RUN
           END-IF

           MOVE "00000000" TO FD-RECORD-ID
           MOVE "0000-00-00" TO FD-DATE

           WRITE REPORT-OUT-REC

           IF NOT REPORT-OUT-FILE-OK THEN
              DISPLAY "Couldn't open the database."
              PERFORM 2000-VSAM-CODE-DISPLAY
           END-IF

           CLOSE REPORT-OUT

           STOP RUN.

       1000-MAIN-END.
           EXIT.

       2000-VSAM-CODE-DISPLAY.
           DISPLAY "File status: " REPORT-OUT-STATUS
           DISPLAY "VSAM-CODE ==>"
              " RETURN: "  VSAM-RETURN-CODE,
              " COMPONENT: "  VSAM-COMPONENT-CODE,
              " REASON: "  VSAM-REASON-CODE.

       2000-VSAM-CODE-DISPLAY-END.
           EXIT.

       END PROGRAM INITKSDS.
