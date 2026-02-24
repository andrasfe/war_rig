```cobol
      *                                                                 00210000
      *----------------------------------------------------------------*00220000
       DATA DIVISION.                                                   00230000
      *----------------------------------------------------------------*00240000
      *                                                                 00250000
      *FILE SECTION.                                                    00260000
      *FD OPFILE1.                                                      00270000
      *01 OPFIL1-REC                    PIC X(100).                     00280000
      *FD OPFILE2.                                                      00290000
      *01 OPFIL2-REC.                                                   00300000
      *   05 ROOT-SEG-KEY               PIC S9(11) COMP-3.              00310000
      *   05 CHILD-SEG-REC              PIC X(200).                     00320000
      *                                                                 00330000
      *----------------------------------------------------------------*00340000
       WORKING-STORAGE SECTION.                                         00350000
      *----------------------------------------------------------------*00360000
       01 OPFIL1-REC                    PIC X(100).                     00361000
       01 OPFIL2-REC.                                                   00362000
          05 ROOT-SEG-KEY               PIC S9(11) COMP-3.              00363000
          05 CHILD-SEG-REC              PIC X(200).                     00364000
       01 WS-VARIABLES.                                                 00370000
         05 WS-PGMNAME                 PIC X(08) VALUE 'IMSUNLOD'.      00380000
         05 CURRENT-DATE               PIC 9(06).                       00390000
         05 CURRENT-YYDDD              PIC 9(05).                       00400000
         05 WS-AUTH-DATE               PIC 9(05).                       00410000
         05 WS-EXPIRY-DAYS             PIC S9(4) COMP.                  00420000
         05 WS-DAY-DIFF                PIC S9(4) COMP.                  00430000
         05 IDX                        PIC S9(4) COMP.                  00440000
         05 WS-CURR-APP-ID             PIC 9(11).                       00450000
      *                                                                 00460000
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.               00470000
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.               00480000
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.          00490000
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.          00500000
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.          00510000
         05 WS-NO-DTL-READ             PIC S9(8) COMP VALUE 0.          00520000
         05 WS-NO-DTL-DELETED          PIC S9(8) COMP VALUE 0.          00530000
      *                                                                 00540000
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.             00550000
           88 ERR-FLG-ON                         VALUE 'Y'.             00560000
           88 ERR-FLG-OFF                        VALUE 'N'.             00570000
         05 WS-END-OF-AUTHDB-FLAG      PIC X(01) VALUE 'N'.             00580000
```
