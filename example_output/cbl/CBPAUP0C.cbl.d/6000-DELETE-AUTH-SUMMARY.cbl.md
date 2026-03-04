```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
      *
       FILE SECTION.
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'CBPAUP0C'.
         05 CURRENT-DATE               PIC 9(06).
         05 CURRENT-YYDDD              PIC 9(05).
         05 WS-AUTH-DATE               PIC 9(05).
         05 WS-EXPIRY-DAYS             PIC S9(4) COMP.
         05 WS-DAY-DIFF                PIC S9(4) COMP.
         05 IDX                        PIC S9(4) COMP.
```
