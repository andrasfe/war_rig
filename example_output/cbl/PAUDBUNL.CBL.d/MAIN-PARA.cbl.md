```cobol
          05 P-CHKP-FREQ            PIC X(05).                          00870026
          05 FILLER                 PIC X(01).                          00880026
          05 P-CHKP-DIS-FREQ        PIC X(05).                          00890026
          05 FILLER                 PIC X(01).                          00900026
          05 P-DEBUG-FLAG           PIC X(01).                          00910026
             88 DEBUG-ON            VALUE 'Y'.                          00920026
             88 DEBUG-OFF           VALUE 'N'.                          00930026
          05 FILLER                 PIC X(01).                          00940026
      *                                                                 00950026
      *                                                                 00960026
       COPY IMSFUNCS.                                                   00961032
      *----------------------------------------------------------------*00970026
      *  IMS SEGMENT LAYOUT                                             00980026
      *----------------------------------------------------------------*00990026
                                                                        01000026
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                   01010026
       01 PENDING-AUTH-SUMMARY.                                         01020026
       COPY CIPAUSMY.                                                   01030026
                                                                        01040026
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                  01050026
       01 PENDING-AUTH-DETAILS.                                         01060026
       COPY CIPAUDTY.                                                   01070026
                                                                        01080026
      *                                                                 01090026
      *----------------------------------------------------------------*01100026
       LINKAGE SECTION.                                                 01110026
      *----------------------------------------------------------------*01120026
      * PCB MASKS FOLLOW                                                01130026
       COPY PAUTBPCB.                                                   01140027
      *                                                                 01160026
      *----------------------------------------------------------------*01170026
       PROCEDURE DIVISION                  USING PAUTBPCB.              01180028
      *                                          PGM-PCB-MASK.          01190028
      *----------------------------------------------------------------*01200026
      *                                                                 01210026
       MAIN-PARA.                                                       01220026
            ENTRY 'DLITCBL'                 USING PAUTBPCB.             01225033
                                                                        01226029
      *                                                                 01230026
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01240026
      *                                                                 01250026
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT        01260026
           UNTIL   WS-END-OF-ROOT-SEG = 'Y'                             01280050
                                                                        01531150
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01532030
```
