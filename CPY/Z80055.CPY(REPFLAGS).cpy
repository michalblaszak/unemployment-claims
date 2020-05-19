      ******************************************************************
      * The copybook of
      * the flag for cleaning the report database file (UNEMPLRP VSAM).
      * Indicates whether the dummy record was removed from the
      * datafile so subsequent import subprograms can skip this step.
      * Author: Michal Blaszak
      * Date:   2020.05.15
      ******************************************************************
       01  CLEAN-UP-FLAG PIC X VALUE 'F'.
           88 IS-REPORT-CLEAN VALUE 'T'.
