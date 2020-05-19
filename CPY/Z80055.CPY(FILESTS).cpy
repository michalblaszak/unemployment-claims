      ******************************************************************
      * The copybook of
      * status flags for input (XML) and output (VSM) files.
      * Author: Michal Blaszak
      * Date:   2020.05.15
      ******************************************************************
       01  IN-STATUS PIC XX.
           88 IN-OK VALUE "00".

       01  REPORT-STATUS.
           05 REPORT-OUT-STATUS PIC X(2).
              88 REPORT-OUT-FILE-OK          VALUE "00".
              88 REPORT-OUT-END-OF-FILE      VALUE "10".
              88 REPORT-OUT-RECORD-NOT-FOUND VALUE "23".
           05 VSAM-CODE.
              10 VSAM-RETURN-CODE     PIC S9(2) USAGE COMP-5 VALUE 0.
              10 VSAM-COMPONENT-CODE  PIC S9(1) USAGE COMP-5 VALUE 0.
              10 VSAM-REASON-CODE     PIC S9(3) USAGE COMP-5 VALUE 0.
