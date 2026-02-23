```cobol
             10   PIC X(20) VALUE '4400EXCED DAILY LMT'.
             10   PIC X(20) VALUE '5100CARD FRAUD'.
             10   PIC X(20) VALUE '5200MERCHANT FRAUD'.
             10   PIC X(20) VALUE '5300LOST CARD'.
             10   PIC X(20) VALUE '9000UNKNOWN'.
          05 WS-DECLINE-REASON-TAB REDEFINES WS-DECLINE-REASON-TABLE
                                OCCURS 10 TIMES
                                ASCENDING KEY IS DECL-CODE
                                INDEXED BY WS-DECL-RSN-IDX.
             10 DECL-CODE                PIC X(4).
             10 DECL-DESC                PIC X(16).

       01 WS-IMS-VARIABLES.
          05 PSB-NAME                        PIC X(8) VALUE 'PSBPAUTB'.
          05 PCB-OFFSET.
             10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +1.
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
```
