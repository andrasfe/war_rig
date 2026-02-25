```cobol
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
      *
      *----------------------------------------------------------------*
       9000-TAKE-CHECKPOINT.
      *----------------------------------------------------------------*
      *
           EXEC DLI CHKP ID(WK-CHKPT-ID)
           END-EXEC
      *
           IF DIBSTAT = SPACES
              ADD 1                      TO WS-NO-CHKP
              IF WS-NO-CHKP >= P-CHKP-DIS-FREQ
                 MOVE 0                  TO WS-NO-CHKP
                 DISPLAY 'CHKP SUCCESS: AUTH COUNT - ' WS-NO-SUMRY-READ
                      ', APP ID - ' WS-CURR-APP-ID
              END-IF
           ELSE
              DISPLAY 'CHKP FAILED: DIBSTAT - ' DIBSTAT
                      ', REC COUNT - ' WS-NO-SUMRY-READ
                      ', APP ID - ' WS-CURR-APP-ID
              PERFORM 9999-ABEND
           END-IF
      *
            .
       9000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       9999-ABEND.
      *----------------------------------------------------------------*
      *
           DISPLAY 'CBPAUP0C ABENDING ...'
```
