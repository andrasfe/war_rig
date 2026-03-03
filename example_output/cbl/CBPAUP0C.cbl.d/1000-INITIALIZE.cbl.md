```cobol
           IF P-EXPIRY-DAYS IS NUMERIC
              MOVE P-EXPIRY-DAYS     TO WS-EXPIRY-DAYS
           ELSE
              MOVE 5                 TO WS-EXPIRY-DAYS
           END-IF
           IF P-CHKP-FREQ = SPACES OR 0 OR LOW-VALUES
              MOVE 5                 TO P-CHKP-FREQ
           END-IF
           IF P-CHKP-DIS-FREQ = SPACES OR 0 OR LOW-VALUES
              MOVE 10                TO P-CHKP-DIS-FREQ
           END-IF
           IF P-DEBUG-FLAG NOT = 'Y'
              MOVE 'N'               TO P-DEBUG-FLAG
           END-IF
           .
      *
       1000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
       2000-FIND-NEXT-AUTH-SUMMARY.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH SMRY READ : ' WS-NO-SUMRY-READ
            END-IF

            EXEC DLI GN USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTSUM0)
                 INTO (PENDING-AUTH-SUMMARY)
            END-EXEC

            EVALUATE DIBSTAT
               WHEN '  '
                    SET NOT-END-OF-AUTHDB TO TRUE
                    ADD 1                 TO WS-NO-SUMRY-READ
```
