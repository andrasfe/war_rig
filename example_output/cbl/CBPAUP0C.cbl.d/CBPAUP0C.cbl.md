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
         05 WS-CURR-APP-ID             PIC 9(11).
      *
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.
         05 WS-NO-DTL-READ             PIC S9(8) COMP VALUE 0.
         05 WS-NO-DTL-DELETED          PIC S9(8) COMP VALUE 0.
      *
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-END-OF-AUTHDB-FLAG      PIC X(01) VALUE 'N'.
           88 END-OF-AUTHDB                      VALUE 'Y'.
           88 NOT-END-OF-AUTHDB                  VALUE 'N'.
         05 WS-MORE-AUTHS-FLAG         PIC X(01) VALUE 'N'.
           88 MORE-AUTHS                         VALUE 'Y'.
           88 NO-MORE-AUTHS                      VALUE 'N'.
         05 WS-QUALIFY-DELETE-FLAG     PIC X(01) VALUE 'N'.
           88 QUALIFIED-FOR-DELETE               VALUE 'Y'.
           88 NOT-QUALIFIED-FOR-DELETE           VALUE 'N'.
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.
            88 END-OF-FILE                       VALUE '10'.
      *
         05 WK-CHKPT-ID.
            10  FILLER              PIC  X(04) VALUE 'RMAD'.
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.
      *
       01 WS-IMS-VARIABLES.
          05 PSB-NAME                        PIC X(8) VALUE 'PSBPAUTB'.
          05 PCB-OFFSET.
             10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +2.
          05 IMS-RETURN-CODE                 PIC X(02).
             88 STATUS-OK                    VALUE '  ', 'FW'.
             88 SEGMENT-NOT-FOUND            VALUE 'GE'.
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.
             88 WRONG-PARENTAGE              VALUE 'GP'.
             88 END-OF-DB                    VALUE 'GB'.
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).
             88  IMS-PSB-SCHD                VALUE 'Y'.
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.

      *
       01 PRM-INFO.
          05 P-EXPIRY-DAYS          PIC 9(02).
          05 FILLER                 PIC X(01).
          05 P-CHKP-FREQ            PIC X(05).
          05 FILLER                 PIC X(01).
          05 P-CHKP-DIS-FREQ        PIC X(05).
          05 FILLER                 PIC X(01).
          05 P-DEBUG-FLAG           PIC X(01).
             88 DEBUG-ON            VALUE 'Y'.
             88 DEBUG-OFF           VALUE 'N'.
          05 FILLER                 PIC X(01).
      *
      *
      *----------------------------------------------------------------*
      *  IMS SEGMENT LAYOUT
      *----------------------------------------------------------------*

      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT
       01 PENDING-AUTH-SUMMARY.
       COPY CIPAUSMY.

      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD
       01 PENDING-AUTH-DETAILS.
       COPY CIPAUDTY.

      *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      * PCB MASKS FOLLOW
       01 IO-PCB-MASK               PIC X.
       01 PGM-PCB-MASK              PIC X.
      *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION                  USING IO-PCB-MASK
                                                 PGM-PCB-MASK.
      *----------------------------------------------------------------*
      *
       MAIN-PARA.
      *
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT
      *
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT

           PERFORM UNTIL ERR-FLG-ON OR END-OF-AUTHDB

              PERFORM 3000-FIND-NEXT-AUTH-DTL     THRU 3000-EXIT

              PERFORM UNTIL NO-MORE-AUTHS
                 PERFORM 4000-CHECK-IF-EXPIRED    THRU 4000-EXIT

                 IF QUALIFIED-FOR-DELETE
                    PERFORM 5000-DELETE-AUTH-DTL  THRU 5000-EXIT
                 END-IF

                 PERFORM 3000-FIND-NEXT-AUTH-DTL  THRU 3000-EXIT
              END-PERFORM

              IF PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0
                 PERFORM 6000-DELETE-AUTH-SUMMARY THRU 6000-EXIT
              END-IF

              IF WS-AUTH-SMRY-PROC-CNT > P-CHKP-FREQ
```
