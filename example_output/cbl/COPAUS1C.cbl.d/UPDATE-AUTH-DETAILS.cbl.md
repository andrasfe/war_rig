```cobol
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
             10 CDEMO-CPVD-PAUKEY-PREV-PG  PIC X(08) OCCURS 20 TIMES.
             10 CDEMO-CPVD-PAUKEY-LAST     PIC X(08).
             10 CDEMO-CPVD-PAGE-NUM        PIC S9(04) COMP.
             10 CDEMO-CPVD-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CPVD-AUTH-KEYS       PIC X(08) OCCURS 5 TIMES.
             10 CDEMO-CPVD-FRAUD-DATA      PIC X(100).

       COPY COPAU01.
```
