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
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).
             88  IMS-PSB-SCHD                VALUE 'Y'.
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.

       01 WS-FRAUD-DATA.
          02 WS-FRD-ACCT-ID                PIC 9(11).
          02 WS-FRD-CUST-ID                PIC 9(9).
          02 WS-FRAUD-AUTH-RECORD          PIC X(200).
          02 WS-FRAUD-STATUS-RECORD.
             05 WS-FRD-ACTION              PIC X(01).
                88 WS-REPORT-FRAUD         VALUE 'F'.
                88 WS-REMOVE-FRAUD         VALUE 'R'.
             05 WS-FRD-UPDATE-STATUS       PIC X(01).
                88 WS-FRD-UPDT-SUCCESS     VALUE 'S'.
                88 WS-FRD-UPDT-FAILED      VALUE 'F'.
             05 WS-FRD-ACT-MSG             PIC X(50).




       COPY COCOM01Y.
          05 CDEMO-CPVD-INFO.
             10 CDEMO-CPVD-PAU-SEL-FLG     PIC X(01).
             10 CDEMO-CPVD-PAU-SELECTED    PIC X(08).
