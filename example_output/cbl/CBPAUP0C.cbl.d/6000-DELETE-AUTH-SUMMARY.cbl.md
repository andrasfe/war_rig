```cobol
       6000-DELETE-AUTH-SUMMARY.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH SMRY DLET : ' PA-ACCT-ID
            END-IF

            EXEC DLI DLET USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTSUM0)
                 FROM (PENDING-AUTH-SUMMARY)
            END-EXEC

            IF DIBSTAT = SPACES
               ADD 1                     TO WS-NO-SUMRY-DELETED
            ELSE
               DISPLAY 'AUTH SUMMARY DELETE FAILED :' DIBSTAT
               DISPLAY 'AUTH APP ID                :' PA-ACCT-ID
               PERFORM 9999-ABEND
            END-IF
            .
       6000-EXIT.
            EXIT.
```
