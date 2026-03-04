```cobol
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.                00720026
             88 WRONG-PARENTAGE              VALUE 'GP'.                00730026
             88 END-OF-DB                    VALUE 'GB'.                00740026
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.                00750026
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.                00760026
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.                00770026
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.    00780026
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).                  00790026
             88  IMS-PSB-SCHD                VALUE 'Y'.                 00800026
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.                 00810026
                                                                        00820026
      *                                                                 00830026
       01 ROOT-QUAL-SSA.                                                00831053
          05 QUAL-SSA-SEG-NAME      PIC X(08) VALUE 'PAUTSUM0'.         00831153
          05 FILLER                 PIC X(01) VALUE '('.                00831253
          05 QUAL-SSA-KEY-FIELD     PIC X(08) VALUE 'ACCNTID '.         00831354
          05 QUAL-SSA-REL-OPER      PIC X(02) VALUE 'EQ'.               00831454
          05 QUAL-SSA-KEY-VALUE     PIC S9(11) COMP-3.                  00831553
          05 FILLER                 PIC X(01) VALUE ')'.                00831653
      *                                                                 00831753
       01 ROOT-UNQUAL-SSA.                                              00831853
```
