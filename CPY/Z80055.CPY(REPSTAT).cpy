      ******************************************************************
      * The copybook used by the report generator for the communication
      * between the calling and called programs.
      * Author: Michal Blaszak
      * Date:   2020.05.15
      ******************************************************************

      * The status if the requets returned by the called subprogram
       01  RESPONSE-OUT.
           05 RET-STATUS PIC X(9) VALUE 'OK'.
              88 RET-STATUS-OK VALUE 'OK'.
              88 RET-STATUS-REC-NOT-FOUND VALUE 'NOT FOUND'.
              88 RET-STATUS-DB-ERROR VALUE 'DB ERROR'.
           05 RET-REC-NO PIC 9(5) VALUE ZERO.

      * The first element in the list of returned records
       01  FIRST-ELEMENT POINTER VALUE IS NULL.
