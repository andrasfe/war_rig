      *----------------------------------------------------------------*00260026
       01 WS-VARIABLES.                                                 00270026
         05 WS-PGMNAME                 PIC X(08) VALUE 'IMSUNLOD'.      00280026
         05 CURRENT-DATE               PIC 9(06).                       00290026
         05 CURRENT-YYDDD              PIC 9(05).                       00300026
         05 WS-AUTH-DATE               PIC 9(05).                       00310026
         05 WS-EXPIRY-DAYS             PIC S9(4) COMP.                  00320026
         05 WS-DAY-DIFF                PIC S9(4) COMP.                  00330026
         05 IDX                        PIC S9(4) COMP.                  00340026
         05 WS-CURR-APP-ID             PIC 9(11).                       00350026
      *                                                                 00360026
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.               00370026
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.               00380026
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.          00390026
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.          00400026
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.          00410026
