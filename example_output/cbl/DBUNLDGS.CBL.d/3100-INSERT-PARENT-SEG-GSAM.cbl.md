```cobol
          05 FILLER                 PIC X(08) VALUE 'PAUTDTL1'.         01000000
          05 FILLER                 PIC X(01) VALUE ' '.                01010000
      *                                                                 01020000
       01 PRM-INFO.                                                     01030000
          05 P-EXPIRY-DAYS          PIC 9(02).                          01040000
          05 FILLER                 PIC X(01).                          01050000
          05 P-CHKP-FREQ            PIC X(05).                          01060000
          05 FILLER                 PIC X(01).                          01070000
          05 P-CHKP-DIS-FREQ        PIC X(05).                          01080000
          05 FILLER                 PIC X(01).                          01090000
          05 P-DEBUG-FLAG           PIC X(01).                          01100000
             88 DEBUG-ON            VALUE 'Y'.                          01110000
             88 DEBUG-OFF           VALUE 'N'.                          01120000
          05 FILLER                 PIC X(01).                          01130000
      *                                                                 01140000
      *                                                                 01150000
```
