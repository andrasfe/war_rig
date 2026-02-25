```cobol
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
      *
      *----------------------------------------------------------------*
       4000-CHECK-IF-EXPIRED.
      *----------------------------------------------------------------*
      *
            COMPUTE WS-AUTH-DATE = 99999 - PA-AUTH-DATE-9C

            COMPUTE WS-DAY-DIFF = CURRENT-YYDDD - WS-AUTH-DATE

            IF WS-DAY-DIFF >= WS-EXPIRY-DAYS
               SET QUALIFIED-FOR-DELETE       TO TRUE

               IF PA-AUTH-RESP-CODE = '00'
                  SUBTRACT 1                  FROM PA-APPROVED-AUTH-CNT
                  SUBTRACT PA-APPROVED-AMT    FROM PA-APPROVED-AUTH-AMT
               ELSE
                  SUBTRACT 1                  FROM PA-DECLINED-AUTH-CNT
                  SUBTRACT PA-TRANSACTION-AMT FROM PA-DECLINED-AUTH-AMT
               END-IF
            ELSE
               SET NOT-QUALIFIED-FOR-DELETE   TO TRUE
            END-IF

            .
       4000-EXIT.
            EXIT.
      *
      *----------------------------------------------------------------*
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
```
