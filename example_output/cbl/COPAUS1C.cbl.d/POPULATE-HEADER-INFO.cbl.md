```cobol

      *****************************************************************
      * ROLLBACK THE DB CHANGES                                       *
      *****************************************************************
       ROLL-BACK.
           EXEC CICS
              SYNCPOINT ROLLBACK
           END-EXEC
           .

      *****************************************************************
      * SCHEDULE PSB                                                  *
      *****************************************************************
       SCHEDULE-PSB.
           EXEC DLI SCHD
                PSB((PSB-NAME))
                NODHABEND
           END-EXEC
           MOVE DIBSTAT        TO IMS-RETURN-CODE
           IF PSB-SCHEDULED-MORE-THAN-ONCE
              EXEC DLI TERM
              END-EXEC

              EXEC DLI SCHD
                   PSB((PSB-NAME))
                   NODHABEND
              END-EXEC
              MOVE DIBSTAT     TO IMS-RETURN-CODE
           END-IF
           IF STATUS-OK
```
