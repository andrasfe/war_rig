```cobol
           88 NO-MORE-AUTHS                      VALUE 'N'.             00630000
         05 WS-END-OF-ROOT-SEG         PIC X(01) VALUE SPACES.          00640000
         05 WS-END-OF-CHILD-SEG        PIC X(01) VALUE SPACES.          00650000
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.          00660000
         05 WS-OUTFL1-STATUS           PIC X(02) VALUE SPACES.          00670000
         05 WS-OUTFL2-STATUS           PIC X(02) VALUE SPACES.          00680000
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.          00690000
            88 END-OF-FILE                       VALUE '10'.            00700000
      *                                                                 00710000
         05 WK-CHKPT-ID.                                                00720000
            10  FILLER              PIC  X(04) VALUE 'RMAD'.            00730000
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.            00740000
      *                                                                 00750000
       01 WS-IMS-VARIABLES.                                             00760000
      *   05 PSB-NAME                        PIC X(8) VALUE 'IMSUNLOD'. 00770000
      *   05 PCB-OFFSET.                                                00780000
      *      10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +2.   00790000
          05 IMS-RETURN-CODE                 PIC X(02).                 00800000
             88 STATUS-OK                    VALUE '  ', 'FW'.          00810000
             88 SEGMENT-NOT-FOUND            VALUE 'GE'.                00820000
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.                00830000
             88 WRONG-PARENTAGE              VALUE 'GP'.                00840000
             88 END-OF-DB                    VALUE 'GB'.                00850000
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.                00860000
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.                00870000
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.                00880000
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.    00890000
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).                  00900000
             88  IMS-PSB-SCHD                VALUE 'Y'.                 00910000
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.                 00920000
                                                                        00930000
      *                                                                 00940000
       01 ROOT-UNQUAL-SSA.                                              00950000
          05 FILLER                 PIC X(08) VALUE 'PAUTSUM0'.         00960000
```
