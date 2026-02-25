```cobol
              IF WS-AUTH-SMRY-PROC-CNT > P-CHKP-FREQ
                 PERFORM 9000-TAKE-CHECKPOINT     THRU 9000-EXIT

                 MOVE 0                         TO WS-AUTH-SMRY-PROC-CNT
              END-IF
              PERFORM 2000-FIND-NEXT-AUTH-SUMMARY THRU 2000-EXIT

           END-PERFORM
      *
           PERFORM 9000-TAKE-CHECKPOINT           THRU 9000-EXIT
      *
           DISPLAY ' '
           DISPLAY '*-------------------------------------*'
           DISPLAY '# TOTAL SUMMARY READ  :' WS-NO-SUMRY-READ
           DISPLAY '# SUMMARY REC DELETED :' WS-NO-SUMRY-DELETED
           DISPLAY '# TOTAL DETAILS READ  :' WS-NO-DTL-READ
           DISPLAY '# DETAILS REC DELETED :' WS-NO-DTL-DELETED
           DISPLAY '*-------------------------------------*'
           DISPLAY ' '
      *
           GOBACK.
      *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
      *----------------------------------------------------------------*
      *
           ACCEPT CURRENT-DATE     FROM DATE
           ACCEPT CURRENT-YYDDD    FROM DAY

           ACCEPT PRM-INFO FROM SYSIN
           DISPLAY 'STARTING PROGRAM CBPAUP0C::'
           DISPLAY '*-------------------------------------*'
           DISPLAY 'CBPAUP0C PARM RECEIVED :' PRM-INFO
           DISPLAY 'TODAYS DATE            :' CURRENT-YYDDD
           DISPLAY ' '

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
      *
      *
      *----------------------------------------------------------------*
       3000-FIND-NEXT-AUTH-DTL.
      *----------------------------------------------------------------*
      *
```
