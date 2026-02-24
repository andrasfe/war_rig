```cobol
       5000-DELETE-AUTH-DTL.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH DTL DLET : ' PA-ACCT-ID
            END-IF

            EXEC DLI DLET USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTDTL1)
                 FROM (PENDING-AUTH-DETAILS)
            END-EXEC

            IF DIBSTAT = SPACES
               ADD 1                     TO WS-NO-DTL-DELETED
            ELSE
               DISPLAY 'AUTH DETAIL DELETE FAILED :' DIBSTAT
               DISPLAY 'AUTH APP ID               :' PA-ACCT-ID
               PERFORM 9999-ABEND
            END-IF

            .
       5000-EXIT.
            EXIT.
```
