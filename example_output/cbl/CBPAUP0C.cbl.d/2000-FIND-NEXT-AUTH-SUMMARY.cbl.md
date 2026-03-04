```cobol
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
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT
                    MOVE PA-ACCT-ID       TO WS-CURR-APP-ID
               WHEN 'GB'
                    SET END-OF-AUTHDB     TO TRUE
               WHEN OTHER
                    DISPLAY 'AUTH SUMMARY READ FAILED  :' DIBSTAT
                    DISPLAY 'SUMMARY READ BEFORE ABEND :'
                                                        WS-NO-SUMRY-READ
                    PERFORM 9999-ABEND
            END-EVALUATE
            .
       2000-EXIT.
            EXIT.
```
