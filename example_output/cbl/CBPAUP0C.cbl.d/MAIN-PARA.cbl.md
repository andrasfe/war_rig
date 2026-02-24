```cobol
       MAIN-PARA.
      *
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT
      *
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT

           PERFORM UNTIL ERR-FLG-ON OR END-OF-AUTHDB

              PERFORM 3000-FIND-NEXT-AUTH-DTL     THRU 3000-EXIT

              PERFORM UNTIL NO-MORE-AUTHS
                 PERFORM 4000-CHECK-IF-EXPIRED    THRU 4000-EXIT

                 IF QUALIFIED-FOR-DELETE
                    PERFORM 5000-DELETE-AUTH-DTL  THRU 5000-EXIT
                 END-IF

                 PERFORM 3000-FIND-NEXT-AUTH-DTL  THRU 3000-EXIT
              END-PERFORM

              IF PA-APPROVED-AUTH-CNT <= 0 AND PA-APPROVED-AUTH-CNT <= 0
                 PERFORM 6000-DELETE-AUTH-SUMMARY THRU 6000-EXIT
              END-IF

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
```
