```cobol
       3000-FIND-NEXT-AUTH-DTL.
      *----------------------------------------------------------------*
      *
            IF DEBUG-ON
               DISPLAY 'DEBUG: AUTH DTL READ : ' WS-NO-DTL-READ
            END-IF

            EXEC DLI GNP USING PCB(PAUT-PCB-NUM)
                 SEGMENT (PAUTDTL1)
                 INTO (PENDING-AUTH-DETAILS)
            END-EXEC
            EVALUATE DIBSTAT
               WHEN '  '
                    SET MORE-AUTHS       TO TRUE
                    ADD 1                TO WS-NO-DTL-READ
               WHEN 'GE'
               WHEN 'GB'
                    SET NO-MORE-AUTHS    TO TRUE
               WHEN OTHER
                    DISPLAY 'AUTH DETAIL READ FAILED  :' DIBSTAT
                    DISPLAY 'SUMMARY AUTH APP ID      :' PA-ACCT-ID
                    DISPLAY 'DETAIL READ BEFORE ABEND :' WS-NO-DTL-READ
                    PERFORM 9999-ABEND
            END-EVALUATE
            .
       3000-EXIT.
            EXIT.
```
