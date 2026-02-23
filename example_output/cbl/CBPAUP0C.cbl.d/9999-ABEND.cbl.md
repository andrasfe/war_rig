```cobol
         05 WK-CHKPT-ID.
            10  FILLER              PIC  X(04) VALUE 'RMAD'.
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.
      *
       01 WS-IMS-VARIABLES.
          05 PSB-NAME                        PIC X(8) VALUE 'PSBPAUTB'.
          05 PCB-OFFSET.
             10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +2.
          05 IMS-RETURN-CODE                 PIC X(02).
```
