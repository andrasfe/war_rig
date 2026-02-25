```cobol
          05 FILLER                 PIC X(08) VALUE 'PAUTSUM0'.         00831953
          05 FILLER                 PIC X(01) VALUE ' '.                00832053
      *                                                                 00832153
       01 CHILD-UNQUAL-SSA.                                             00832253
          05 FILLER                 PIC X(08) VALUE 'PAUTDTL1'.         00832353
          05 FILLER                 PIC X(01) VALUE ' '.                00832453
      *                                                                 00833029
       01 PRM-INFO.                                                     00840026
          05 P-EXPIRY-DAYS          PIC 9(02).                          00850026
          05 FILLER                 PIC X(01).                          00860026
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
       01 IO-PCB-MASK   PIC X(1).                                       01131057
       COPY PAUTBPCB.                                                   01140027
      *                                                                 01160026
      *----------------------------------------------------------------*01170026
       PROCEDURE DIVISION                  USING IO-PCB-MASK            01180057
                                                 PAUTBPCB.              01181056
      *                                          PGM-PCB-MASK.          01190028
```
